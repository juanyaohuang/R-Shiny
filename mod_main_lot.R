# RWE: exposure (ex) and outcome (oc) data to determine line of therapy

# Module UI function
mod_main_lot_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    id,
    tabsetPanel(
      
      tabPanel(
        "Data Source",
        shinydashboardPlus::box(
          width = 12,
          title = "File Upload",
          collapsible = TRUE,
          dropdownMenu = reset_dropdown(ns("reset_file")),
          fluidRow(
            column(6, fileInput(ns("file_ex"), "Choose an EX file", 
                                accept = file_accept)),
            column(6, fileInput(ns("file_oc"), "Choose an OC file (optional)", 
                                accept = file_accept))
          ),
          fluidRow(
            column(6, fileInput(ns("file_dm"), "Choose a DM file (optional)",
                                accept = file_accept)),
          ),
          fluidRow(
            column(3, selectInput(ns("subj"), "Subject ID", NULL)),
            column(3, selectInput(ns("trt"), "Treatment", NULL)),
            column(3, selectInput(ns("trt_start"), "Start date", NULL)),
            column(3, selectInput(ns("trt_end"), "End date", NULL))
          ),
          hr(),
          p(strong("Optional"), align = "center"),
          fluidRow(
            column(3, selectInput(ns("outcome"), "Outcome", "None",
                                  selected = "None")),
            column(3, selectInput(ns("method"), "Method", "None",
                                  selected = "None")),
            column(3, selectInput(ns("outcome_date"), "Outcome date", "None",
                                  selected = "None"))
          )
        ),
        tabBox(
          width = 12,
          tabPanel("Exposure", 
                   column(12, align = "center", htmlOutput(ns("subjects_data"))),
                   DTOutput_div(ns("table_ex"))
          ),
          tabPanel("Outcome", DTOutput_div(ns("table_oc"))),
          tabPanel("Demographic", DTOutput_div(ns("table_dm")))
        )
      ),
      
      # Line of treatment plot + tables ----------------------------------------
      tabPanel(
        "Treatment Pattern",
        shinydashboardPlus::box(
          width = 12,
          title = "Treatment Graph",
          fluidRow(
            column(4, selectInput(ns("subj_sel"), "Subject ID", NULL)),
            div(column(6, awesomeCheckbox(ns("show_dates"), "Display dates", TRUE)
            ), style = "padding-top:32px")
          ),
          uiOutput(ns("gantt_chart_ui"))
        ),
        shinydashboardPlus::box(
          width = 12, 
          title = "Treatment Tables",
          dropdownMenu = shinydashboardPlus::boxDropdown(
            icon = shiny::icon("download"),
            shinydashboardPlus::boxDropdownItem(downloadLink(ns("download_treatment_excel"), "Excel")),
            shinydashboardPlus::dropdownDivider(),
            shinydashboardPlus::boxDropdownItem(downloadLink(ns("download_treatment_csv"), "CSV"))
          ),
          uiOutput(ns("table_lot_ui"))
        )
      )
      
    )
  )
}

# Module server function
mod_main_lot_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Upload files
    observe({
      req(input$file_ex)
      rv$df_lot_ex <- upload_file(input$file_ex)
    })
    observe({
      req(input$file_oc)
      rv$df_lot_oc <- upload_file(input$file_oc)
    })
    observe({
      req(input$file_dm)
      rv$df_lot_dm <- upload_file(input$file_dm)
    })
    
    # Reset fileInputs
    observeEvent(input$reset_file, {
      rv$df_lot_ex <- NULL
      rv$df_lot_oc <- NULL
      rv$df_lot_dm <- NULL
      shinyjs::reset("file_ex")
      shinyjs::reset("file_oc")
      shinyjs::reset("file_dm")
    })
    
    # Ensure Subject ID variable is present in all uploaded files
    subj_choices <- reactive({
      all_names <- names(rv$df_lot_ex)
      if(!is.null(rv$df_lot_oc)) {
        all_names <- intersect(all_names, names(rv$df_lot_oc))
      }
      if(!is.null(rv$df_lot_dm)) {
        all_names <- intersect(all_names, names(rv$df_lot_dm))
      }
      unique(all_names)
    })
    
    # Update variables
    observe({
      updateSelectInput(session, "subj", choices = subj_choices(),
                        selected = grep_var("subjid", subj_choices()))
    })
    observe({
      all_names <- names(rv$df_lot_ex)
      date_names <- names(rv$df_lot_ex)[purrr::map_lgl(rv$df_lot_ex, lubridate::is.Date)]
      updateSelectInput(session, "trt", choices = all_names,
                        selected = grep_var("trt$", all_names))
      updateSelectInput(session, "trt_start", choices = date_names,
                        selected = grep_var("sdt", date_names))
      updateSelectInput(session, "trt_end", choices = date_names,
                        selected = grep_var("edt", date_names))
    })
    observe({
      all_names <- names(rv$df_lot_oc)
      date_names <- names(rv$df_lot_oc)[purrr::map_lgl(rv$df_lot_oc, lubridate::is.Date)]
      if(grepl_var("aval", all_names)) {
        resp_select <- grep_var("aval", all_names)
      } else if(grepl_var("resp", all_names)) {
        resp_select <- grep_var("resp", all_names)
      } else {
        resp_select <- "None"
      }
      updateSelectInput(session, "outcome", choices = c("None", all_names),
                        selected = resp_select)
      updateSelectInput(session, "method", choices = c("None", all_names),
                        selected = ifelse_var("method", all_names))
      if(grepl_var("avaldt", date_names)) {
        rdate_select <- grep_var("avaldt", date_names)
      } else if(grepl_var("date", date_names)) {
        rdate_select <- grep_var("date", date_names)
      } else {
        rdate_select <- "None"
      }
      updateSelectInput(session, "outcome_date", choices = c("None", date_names),
                        selected = rdate_select)
    })
    
    # Output number of subjects
    output$subjects_data <- renderUI({
      req(rv$df_lot_ex)
      req(input$subj)
      subjects_HTML(subjects_fn(rv$df_lot_ex, input$subj))
    })
    
    # Data tables
    output$table_ex <- DT::renderDT({
      req(rv$df_lot_ex)
      data_table_fn(rv$df_lot_ex)
    })
    output$table_oc <- DT::renderDT({
      req(rv$df_lot_oc)
      data_table_fn(rv$df_lot_oc)
    })
    output$table_dm <- DT::renderDT({
      req(rv$df_lot_dm)
      data_table_fn(rv$df_lot_dm)
    })
    
    # Main data
    main_ex <- reactive({
      req(input$subj %in% names(rv$df_lot_ex))
      process_file(rv$df_lot_ex, input$subj)
    })
    main_oc <- reactive({
      req(input$subj %in% names(rv$df_lot_oc))
      process_file(rv$df_lot_oc, input$subj)
    })
    main_dm <- reactive({
      req(input$subj %in% names(rv$df_lot_dm))
      process_file(rv$df_lot_dm, input$subj)
    })
    
    
    # Timeline ---------------------------------------------------------------------
    # Update Line of Therapy's subject select
    observe({
      id <- sort(unique(main_ex()[[input$subj]]))
      updateSelectInput(session, "subj_sel", choices = id)
    })
    
    # Treatment data frame
    observe({
      req(main_ex())
      req(all(c(input$subj, input$trt, 
                input$trt_start, input$trt_end) %in% names(main_ex())))
      columns <- c("Subject ID", "Index", "Line", "Comment", "Treatment", "Start Date", "End Date")
      df_trt <- data.frame(matrix(nrow = 0, ncol = length(columns)))
      names(df_trt) <- columns
      
      n_subj <- unique(main_ex()[[input$subj]])
      for(i in 1:length(n_subj)) {
        df_i <- main_ex() %>%
          filter(.data[[input$subj]] == n_subj[i]) %>%
          select(all_of(input$subj), all_of(input$trt), all_of(input$trt_start), all_of(input$trt_end)) %>%
          drop_na(all_of(input$trt), all_of(input$trt_start)) %>%
          arrange(.data[[input$trt_start]]) %>%
          distinct() %>%
          mutate(across(all_of(input$trt), 
                        ~forcats::fct_inorder(as.character(.x)))) %>%
          mutate(Line = as.numeric(.data[[input$trt]]), 
                 Comment = "",
                 .after = 1) %>%
          mutate(Index = "", .after = 1)
        df_i$Index[1] <- 1
        names(df_i) <- columns
        df_trt <- rbind(df_trt, df_i)
      }
      rv$df_trt <- df_trt
    })
    
    # Outcome data frame
    observe({
      req(main_oc())
      req(all(c(input$subj, input$outcome, input$method) %in% names(main_oc())))
      columns <- c("Subject ID", "Line", "Comment", "Outcome", "Method", "Outcome Date")
      df_oc <- data.frame(matrix(nrow = 0, ncol = length(columns)))
      names(df_oc) <- columns
      
      n_subj <- unique(main_oc()[[input$subj]])
      for(i in 1:length(n_subj)) {
        df_i <- main_oc() %>%
          filter(.data[[input$subj]] == n_subj[i]) %>%
          select(all_of(input$subj), all_of(input$outcome), all_of(input$method), 
                 all_of(input$outcome_date)) %>%
          drop_na() %>%
          distinct(.keep_all = TRUE) %>%
          arrange(.data[[input$outcome_date]])
        df_ex <- rv$df_trt %>%
          filter(`Subject ID` == n_subj[i])
        df_i <- df_i %>%
          mutate(int = findInterval(df_i[[input$outcome_date]], 
                                    df_ex[["Start Date"]], all.inside = TRUE), 
                 Line = df_ex$Line[int], 
                 Comment = "",
                 .after = 1) %>%
          select(-int)
        names(df_i) <- columns
        df_oc <- rbind(df_oc, df_i)
      }
      rv$df_oc <- df_oc
    })
    
    # Update/combine treatments based on table changes
    df_trt_update <- reactive({
      df <- rv$df_trt
      req(df)
      df_new <- head(df, 0)
      ids <- unique(df[["Subject ID"]])
      for(i in 1:length(ids)) {
        df_i <- df %>%
          filter(`Subject ID` == ids[i])
        vec <- df_i %>%
          group_split(Line) %>%
          purrr::map_chr(function(...) {
            current <- tibble(...)
            current[["Treatment"]] %>%
              as.character() %>%
              paste(collapse = ",") %>%
              strsplit(",") %>% 
              unlist() %>%
              unique() %>%
              paste(collapse = ",")
          })
        new_trt <- tibble(Line = sort(unique(df_i$Line)), trt2 = vec)
        df_i <- df_i %>%
          purrr::pmap_dfr(function(...) {
            current <- tibble(...)
            mutate(current, trt2 = filter(new_trt, Line == current$Line)$trt2)
          }) %>%
          mutate(trt2 = forcats::fct_inorder(trt2))
        df_i["Treatment"] <- df_i$trt2
        df_i <- select(df_i, -trt2)
        df_new <- rbind(df_new, df_i)
      }
      df_new
    })
    
    # Update outcome table with matching treatment
    df_oc_update <- reactive({
      df <- rv$df_oc
      req(df)
      df_new <- head(df, 0)
      ids <- unique(df[["Subject ID"]])
      for(i in 1:length(ids)) {
        df_i <- df %>%
          filter(`Subject ID` == ids[i])
        df_ex <- df_trt_update() %>%
          filter(`Subject ID` == ids[i])
        df_i["Treatment"] <- df_ex[["Treatment"]][match(df_i$Line, df_ex$Line)]
        df_i <- df_i %>%
          relocate(Treatment, .after = Comment)
        df_new <- rbind(df_new, df_i)
      }
      df_new
    })
    
    # Line of treatment plot (Gantt chart) -------------------------------------
    output$gantt_chart <- plotly::renderPlotly({
      df <- df_trt_update() %>%
        filter(`Subject ID` == input$subj_sel) %>%
        mutate(Treatment = forcats::fct_inorder(as.character(Treatment)))
      
      # Display standard date format
      date_format <- function(date) {
        gsub("(\\s)0", " ", format(date, format = "%B %d, %Y"))
      }
      # Choose colors based on number of treatments
      n <- n_distinct(df[["Treatment"]])
      cols <- set_colors(n)[1:n]
      df$color <- factor(df[["Treatment"]], labels = cols)
      
      hover_font <- list(color = "black", size = 14)
      p <- plotly::plot_ly()
      for(i in 1:(nrow(df))){
        # Marker points
        if(identical(df[["Start Date"]][i], df[["End Date"]][i]) |
           is.na(df[["End Date"]][i])) {
          # Add points (no end date)
          p <- plotly::add_markers(
            p,
            x = df[["Start Date"]][i],
            y = df[["Treatment"]][i],
            marker = list(color = df$color[i], size = 8),
            hoverinfo = "text",
            hoverlabel = list(
              bgcolor = colorspace::lighten(df$color[i]),
              font = hover_font
            ),
            text = paste(df[["Treatment"]][i],
                         "<br>",
                         date_format(df[["Start Date"]][i]))
          )
          if(input$show_dates) {
            # Date label
            p <- plotly::add_text(
              p,
              x = df[["Start Date"]][i],
              y = df[["Treatment"]][i],
              hoverinfo = "none",
              text = paste0("  ", format(df[["Start Date"]][i], 
                                         format = "%m/%d/%y")),
              textposition = "middle right"
            )
          }
        } else {
          # Gantt lines
          if(input$show_dates) {
            p <- p %>% 
              # Add text first to ensure lines go on top
              # Starting date label
              plotly::add_text(
                x = df[["Start Date"]][i],
                y = df[["Treatment"]][i],
                hoverinfo = "none",
                text = paste0(format(df[["Start Date"]][i], 
                                     format = "%m/%d/%y"), " "),
                textposition = "middle left"
              ) %>% 
              # Ending date label
              plotly::add_text(
                x = df[["End Date"]][i],
                y = df[["Treatment"]][i],
                hoverinfo = "none",
                text = paste0(" ", format(df[["End Date"]][i], 
                                          format = "%m/%d/%y")),
                textposition = "middle right"
              )
          }
          # Treatment line
          p <- p %>%
            plotly::add_lines(
              x = c(df[["Start Date"]][i], df[["End Date"]][i]),
              y = df[["Treatment"]][i],
              line = list(color = df$color[i], width = 10),
              hoverinfo = "text",
              hoverlabel = list(
                bgcolor = colorspace::lighten(df$color[i]),
                font = hover_font
              ),
              text = paste(df[["Treatment"]][i],
                           "<br>Start:", 
                           date_format(df[["Start Date"]][i]), 
                           "<br>End:", 
                           date_format(df[["End Date"]][i]), 
                           "<br>Duration:", 
                           df[["End Date"]][i] - df[["Start Date"]][i], 
                           "days")
            ) %>%
            # Add points to the line (for hoverinfo)
            plotly::add_markers(
              x = as.Date(seq(as.numeric(df[["Start Date"]][i]), 
                              as.numeric(df[["End Date"]][i]),
                              by = 10),
                          origin = "1970-01-01"),
              y = df[["Treatment"]][i],
              marker = list(color = df$color[i], opacity = 0),
              hoverinfo = "text",
              hoverlabel = list(
                bgcolor = colorspace::lighten(df$color[i]),
                font = hover_font
              ),
              text = paste(df[["Treatment"]][i],
                           "<br>Start:", 
                           date_format(df[["Start Date"]][i]), 
                           "<br>End:", 
                           date_format(df[["End Date"]][i]), 
                           "<br>Duration:", 
                           df[["End Date"]][i] - df[["Start Date"]][i], 
                           "days")
            )
        }
      }
      # Add outcomes
      if(isTruthy(rv$df_oc)) {
        df2 <- df_oc_update() %>%
          filter(`Subject ID` == input$subj_sel)
        if(nrow(df2) > 0) {
          for(i in 1:nrow(df2)) {
            # Diamond marker
            p <- plotly::add_markers(
              p,
              x = df2[["Outcome Date"]][i],
              y = df2[["Treatment"]][i],
              marker = list(color = "black", 
                            size = 8,
                            symbol = "diamond-tall"),
              hoverinfo = "text",
              hoverlabel = list(bgcolor = "#f2f2f2",
                                font = hover_font),
              text = paste0(df2[["Outcome"]][i], ": ",
                            df2[["Method"]][i], "<br>",
                            date_format(df2[["Outcome Date"]][i]))
            )
          }
          # Outcome label
          p <- plotly::add_annotations(
            p,
            x = df2[["Outcome Date"]],
            y = df2[["Treatment"]],
            text = df2[["Outcome"]],
            arrowcolor = "black",
            font = list(color = "red"),
            arrowhead = 0,
            ax = 0,
            ay = -12
          )
          if(input$show_dates) {
            # Date label
            p <- plotly::add_annotations(
              p,
              x = df2[["Outcome Date"]],
              y = df2[["Treatment"]],
              text = format(df2[["Outcome Date"]], 
                            format = "%m/%d/%y"),
              arrowcolor = "black",
              font = list(color = "#ff7f7f"),
              arrowhead = 0,
              ax = 0,
              ay = 12
            )
          }
        }
      }
      p <- p %>%
        plotly::layout(title = list(text = input$subj_sel, 
                                    font = list(size = 16)),
                       showlegend = FALSE,
                       font = list(size = 14),
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "", autorange = "reversed")) %>%
        custom_config(paste0("treatment_plot_(", input$subj_sel, ")"))
      p
    })
    
    # Render LOT plot
    output$gantt_chart_ui <- renderUI({
      req(rv$df_trt)
      df <- rv$df_trt %>%
        filter(`Subject ID` == input$subj_sel)
      height <- n_distinct(df[["Treatment"]]) * 50
      plotly::plotlyOutput(ns("gantt_chart"), height = max(200, height)) %>% 
        add_spinner()
    })
    
    # LOT tables -------------------------------------------------------------------
    # Allow for changing of line number
    observeEvent(input$table_trt_cell_edit, {
      info <- input$table_trt_cell_edit
      rv$df_trt <- DT::editData(rv$df_trt, info, rownames = FALSE)
    })
    observeEvent(input$table_oc_cell_edit, {
      info <- input$table_oc_cell_edit
      rv$df_oc <- DT::editData(rv$df_oc, info, rownames = FALSE)
    })
    
    # Subject data/demographics table
    output$table_dm <- DT::renderDT({
      req(main_dm())
      main_dm() %>%
        filter(.data[[input$subj]] == input$subj_sel) %>%
        DT::datatable(rownames = FALSE,
                      options = list(dom = "t"))
    })
    
    # Treatment table with editable line numbers
    output$table_trt <- DT::renderDT({
      req(rv$df_trt)
      rv$df_trt %>%
        filter(`Subject ID` == input$subj_sel) %>%
        DT::datatable(rownames = FALSE,
                      editable = list(target = "cell", 
                                      disable = list(columns = c(0, 4, 5, 6))),
                      options = list(columnDefs = list(
                        list(className = 'dt-center', targets = 1),
                        list(visible = FALSE, targets = 0)
                      )))
    })
    # Outcome table with editable line numbers
    output$table_oc <- DT::renderDT({
      req(rv$df_oc)
      rv$df_oc %>%
        filter(`Subject ID` == input$subj_sel) %>%
        DT::datatable(rownames = FALSE,
                      editable = list(target = "cell", 
                                      disable = list(columns = c(0, 3, 4, 5))),
                      options = list(columnDefs = list(
                        list(visible = FALSE, targets = 0)
                      )))
    })
    
    # Tables with monospace font
    DTOutput_div <- function(outputId) {
      div(DT::DTOutput(outputId), style = "font-family:monospace; font-size:small")
    }
    
    # Determine whether demographic and outcome data exist
    dm_cond <- reactive(!is.null(rv$df_lot_dm))
    oc_cond <- reactive({
      !any(input$outcome == "None", is.null(rv$df_oc), nrow(rv$df_oc) == 0)
    })
    
    # Render treatment and outcome tables
    output$table_lot_ui <- renderUI({
      
      out <- div()
      if(dm_cond()) {
        out <- div(out,
                   br(),
                   column(12, align = "center", h5(strong("Subject Data"))),
                   DTOutput_div(ns("table_dm")))
      }
      out <- div(out,
                 column(12, align = "center", h5(strong("Treatment"))),
                 DTOutput_div(ns("table_trt"))
      )
      if(oc_cond()) {
        out <- div(out,
                   br(),
                   column(12, align = "center", h5(strong("Outcome"))),
                   DTOutput_div(ns("table_oc")))
      }
      out
    })
    
    # Download -----------------------------------------------------------------
    output$download_treatment_excel <- downloadHandler(
      filename = function() {
        paste0("lot_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        x <- list()
        if(dm_cond()) {
          x <- c(x, list("Subject Data" = main_dm()))
        }
        x <- c(x, Treatment = list(df_trt_update()))
        if(oc_cond()) {
          x <- c(x, Outcome = list(df_oc_update()))
        }
        writexl::write_xlsx(x, file)
      }
    )
    output$download_treatment_csv <- downloadHandler(
      filename = function() {
        paste0("lot_", Sys.Date(), ".zip")
      },
      content = function(file) {
        tmpdir <- tempdir()
        setwd(tempdir())
        fs <- NULL
        if(dm_cond()) {
          fs <- c(fs, "Subject Data.csv")
          write.csv(main_dm(), "Subject Data.csv", row.names = FALSE)
        }
        fs <- c(fs, "Treatment.csv")
        write.csv(df_trt_update(), "Treatment.csv", row.names = FALSE)
        if(oc_cond()) {
          fs <- c(fs, "Outcome.csv")
          write.csv(df_oc_update(), "Outcome.csv", row.names = FALSE)
        }
        zip::zip(file, fs)
      }
    )
    
  })
}
