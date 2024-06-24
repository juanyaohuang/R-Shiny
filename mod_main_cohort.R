# Subject-level data (cohort selection), mainly adsl files

# Module UI function
mod_main_cohort_ui <- function(id, is.gen = FALSE) {
  ns <- NS(id)
  
  # Add "Missing Percentage" tab only in RWE section
  if(is.gen) {
    group_var_ui <- NULL
    missing_tab_ui <- NULL
  } else{
    group_var_ui <- div(
      column(3, selectInput(ns("group"), 
                            help_link("Group (optional)",
                                      ns("group_help")), "None",
                            selected = "None")),
      shinyBS::bsPopover(
        ns("group_help"), NULL,
        paste("For grouping Missing Percentage")
      )
    )
    missing_tab_ui <- tabPanel(
      "Missing Percentage",
      shinydashboardPlus::box(
        width = 12, 
        title = "Missing Percentage",
        dropdownMenu = mod_download_ui(ns("download_missing"), format = "data"),
        fluidRow(
          column(3, pickerInput_vars(ns("missing_sel")))
        ),
        DTOutput_div(ns("table_missing"))
      )
    )
  }
  
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
            column(6, fileInput(ns("file"), "Choose an ADSL file", 
                                accept = file_accept)),
            column(6, pickerInput_vars(ns("columns"), "Column select"))
          ),
          fluidRow(
            column(3, selectInput(ns("subj"), "Subject ID", NULL)),
            group_var_ui
          )
        ),
        shinydashboardPlus::box(
          width = 12, 
          title = "Subject Data Listing",
          dropdownMenu = mod_download_ui(ns("download_data"), format = "data"),
          column(12, align = "center", htmlOutput(ns("subjects_data"))),
          DTOutput_div(ns("table_data"))
        )
      ),
      
      # Population Selection ---------------------------------------------------
      tabPanel(
        "Population Selection",
        mod_sub_varfilter_ui(ns("criteria"), is.table = FALSE),
        shinydashboardPlus::box(
          width = 12,
          title = "Attrition Table",
          collapsible = TRUE,
          column(12, align = "center", 
                 uiOutput(ns("attrition_ui")) %>% add_spinner())
        ),
        shinydashboardPlus::box(
          width = 12, 
          title = "Filtered Table",
          dropdownMenu = mod_download_ui(ns("download_criteria"), format = "data"),
          column(12, align = "center", htmlOutput(ns("subjects_criteria"))),
          DTOutput_div(ns("table_criteria"))
        )
      ),
      
      # Missing Percentage -----------------------------------------------------
      missing_tab_ui,
      
      # Statistical Summary ----------------------------------------------------
      mod_sub_statsummary_ui(ns("summary"))
      
    )
  )
}

# Module server function
mod_main_cohort_server <- function(id, is.gen = FALSE) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Upload file
    observe({
      req(input$file)
      rv$df_adsl <- upload_file(input$file)
    })
    
    # Reset fileInput
    observeEvent(input$reset_file, {
      rv$df_adsl <- NULL
      shinyjs::reset("file")
    })
    
    # Update variables
    observe({
      choices <- names(rv$df_adsl)
      updateSelectInput(session, "subj", choices = choices,
                        selected = grep_var("subjid", choices))
      if(!is.gen) {
        updateSelectInput(session, "group", choices = c("None", choices),
                          selected = ifelse_var("^trt", choices))
      }
    })
    
    # Output number of subjects
    output$subjects_data <- renderUI({
      req(rv$df_adsl)
      req(input$subj)
      subjects_HTML(subjects_fn(rv$df_adsl, input$subj))
    })
    
    # Column names
    observe({
      choices <- names(rv$df_adsl)
      choice_labels <- var_labels(rv$df_adsl)
      updatePickerInput(session, "columns", choices = choices, selected = choices,
                        choicesOpt = list(subtext = choice_labels))
    })
    
    # Main data
    main <- reactive({
      req(rv$df_adsl)
      req(input$subj %in% names(rv$df_adsl))
      process_file(rv$df_adsl, input$subj)
    })
    
    # Data to display
    data_display <- reactive({
      select(main(), any_of(input$columns))
    })
    
    # Main data table
    output$table_data <- DT::renderDT({
      data_display() %>%
        data_table_fn()
    })
    
    # Inclusion/Exclusion Criteria ---------------------------------------------
    
    # Apply filter criteria
    cohort_out <- mod_sub_varfilter_server("criteria", main, is.table = FALSE)
    
    # Filtered data
    df_cohort <- reactive({
      cohort_out()[[1]]
    })
    
    # Selected filter variables and their values
    criteria_sel <- reactive({
      cohort_out()[[2]]
    })
    
    # Number of subjects in cohort select table
    output$subjects_criteria <- renderUI({
      subjects_HTML(subjects_fn(df_cohort(), input$subj))
    })
    
    # Displayed cohort select table
    df_cohort_display <- reactive({
      req(input$subj)
      select(df_cohort(), 
             all_of(input$subj), any_of(names(criteria_sel())))
    })
    
    # Cohort select table
    output$table_criteria <- DT::renderDT({
      DT::datatable(df_cohort_display(),
                    extensions = "FixedHeader",
                    options = list(fixedHeader = TRUE),
                    filter = "top")
    })
    
    # Attrition table ----------------------------------------------------------
    attrition_table <- reactive({
      df <- main()
      criteria_sel <- criteria_sel()
      req(criteria_sel)
      n <- length(criteria_sel)
      desc <- vector(length = n)
      cond <- vector(length = n)
      for(i in 1:n) {
        param <- names(criteria_sel)[i]
        val <- criteria_sel[[i]]
        req(val)

        # Date vs numeric vs character values
        if(lubridate::is.Date(val) | lubridate::is.POSIXt(val)) {
          # Need to convert date to numeric for condition to work
          df[[param]] <- as.numeric(df[[param]])
          desc[i] <- paste0(i, ". ", val[1], " \U2264 ",
                            param, " \U2264 ", val[2])
          cond[i] <- paste0(as.numeric(val[1]), "<=", param, "&",
                            param, "<=", as.numeric(val[2]))
        } else if(is.numeric(val)) {
          desc[i] <- paste0(i, ". ", val[1], " \U2264 ",
                            param, " \U2264 ", val[2])
          cond[i] <- paste0(val[1], "<=", param, "&",
                            param, "<=", val[2])
        } else {
          val_chr <- paste0("c(", paste0('"', val, '"', collapse = ","), ")")
          desc[i] <- paste0(i, ". ", param, ": ",
                            paste(val, collapse = ", "))
          cond[i] <- paste0(param, "%in%", val_chr)
        }
      }
      req(any(desc != FALSE))
      get_attrition(df, desc, cond, input$subj) %>%
        filter(Criteria != FALSE)
    })

    # Attrition plot
    output$attrition_plot <- renderPlot({
      visr(attrition_table(), font_size = 14)
    })

    # Attrition plot UI
    output$attrition_ui <- renderUI({
      height <- 50 + nrow(attrition_table()) * 50
      plotOutput(ns("attrition_plot"), height = height)
    })
    
    # Missing values -----------------------------------------------------------
    
    # Only run in RWE section
    if(!is.gen) {
      
      # Choices for variables
      miss_var_choices <- reactive({
        all_names <- names(main())
        all_names[!all_names %in% c(input$subj, input$group)] %>%
          sort()
      })
      # Set variable picker
      observe({
        vars <- miss_var_choices()
        updatePickerInput(session, "missing_sel", choices = vars, selected = vars)
      })
      
      # Missing values
      df_missing <- reactive({
        vars <- input$missing_sel
        df <- df_cohort()
        req(vars)
        req(df)
        
        final <- tibble()
        for(i in 1:length(vars)) {
          if(input$group == "None") {
            df_i <- df
          } else {
            df_i <- df %>%
              group_by(.data[[input$group]])
          }
          df_i <- df_i %>%
            summarize(n = sum(is.na(.data[[vars[i]]])),
                      N = n(),
                      "% Missing" = sum(is.na(.data[[vars[i]]]))/n())
          if(nrow(df_i) > 0) {
            df_i <- cbind(Variable = vars[i], df_i)
          }
          final <- rbind(final, df_i)
        }
        final
      })
      
      # Missing values UI
      output$table_missing <- DT::renderDT({
        if(input$group == "None") {
          DT::datatable(df_missing(),
                        options = list(pageLength = 100)) %>%
            miss_table(4)
        } else {
          DT::datatable(df_missing(),
                        extensions = "RowGroup",
                        colnames = c("Variable", "Treatment", "n", "N", "% Missing"),
                        options = list(pageLength = 100,
                                       rowGroup = list(dataSrc = 1))) %>%
            miss_table(5)
        }
      })
      
      # Download missing percentage data
      mod_download_server("download_missing", df_missing,
                          filename = "units", format = "data")
    }
    
    # Statistical summary ------------------------------------------------------
    mod_sub_statsummary_server("summary", df_cohort)
    
    # Download -----------------------------------------------------------------
    
    # Uploaded data
    mod_download_server("download_data", data_display,
                        filename = "data", format = "data")
    # Data after population selection
    mod_download_server("download_criteria", df_cohort_display,
                        filename = "missing", format = "data")
    
  })
}
