# RWE: IPTW files, propensity score analysis

# Module UI function
mod_main_ps_ui <- function(id) {
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
            column(6, fileInput(ns("file"), "Choose an IPTW file", 
                                accept = ".sas7bdat")),
            column(6, pickerInput_vars(ns("columns"), "Column select"))
          ),
          fluidRow(
            column(3, selectInput(ns("study"), "Study ID", NULL)),
            column(3, selectInput(ns("imput"), "Imputation Number", NULL)),
            column(3, selectInput(ns("trt"), "Treatment Arm", NULL))
          ),
          fluidRow(
            column(3, radioButtons(ns("plot_var"), "Analysis Variable", 
                                   c("Stabilize Weight", "Propensity Score"))),
            column(3, selectInput(ns("weight"), "Stabilize Weight", NULL)),
            conditionalPanel(
              "input.plot_var == 'Propensity Score'", ns = ns,
              column(3, selectInput(ns("pscore"), "Propensity Score", NULL))
            )
          )
        ),
        shinydashboardPlus::box(
          width = 12,
          title = "Subject Data Listing",
          dropdownMenu = mod_download_ui(ns("download_data"), format = "data"),
          DTOutput_div(ns("table_data"))
        )
      ),
      
      # Plots by Imputation ----------------------------------------------------
      tabPanel(
        "Plots by Imputation",
        shinydashboardPlus::box(
          width = 12,
          title = "Plots by Imputation",
          dropdownMenu = mod_download_ui(ns("download_plot_imput"), format = "graph"),
          fluidRow(
            column(3, pickerInput(ns("imput_num"), "Imputation Number", 
                                  NULL, multiple = TRUE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `selected-text-format` = "count > 5"))),
            column(3, selectInput(ns("imput_approach"), "Trimming Approach",
                                  c("Replace", "Remove"))),
            column(3, selectInput(ns("imput_method"), "Trimming Method",
                                  list("None" = 0,
                                       "Extreme values" = "extreme",
                                       "1% and 99% percentile" = 1,
                                       "2.5% and 97.5% percentile" = 2,
                                       "5% and 95% percentile" = 3,
                                       "10% and 90% percentile" = 4))),
            column(3, radioGroupButtons(ns("imput_per_row"), "Graphs per row",
                                        c(1, 2, 3), selected = 2))
          ),
          tabsetPanel(
            tabPanel(
              "Overview",
              uiOutput(ns("plot_imput")) %>% add_spinner()
            ),
            tabPanel(
              "Boxplot Only",
              uiOutput(ns("plot_imput_boxplot_ui")) %>% add_spinner()
            )
          )
        )
      ),
      
      # Plots by Trim Method ---------------------------------------------------
      tabPanel(
        "Plots by Trim Method",
        shinydashboardPlus::box(
          width = 12,
          title = "Plots by Trimming Method",
          dropdownMenu = mod_download_ui(ns("download_plot_trim"), format = "graph"),
          fluidRow(
            column(3, selectInput(ns("trim_num"), "Imputation Number", 
                                  NULL)),
            column(3, selectInput(ns("trim_approach"), "Trimming Approach",
                                  c("Replace", "Remove"))),
            column(3, pickerInput(ns("trim_method"), "Trimming Method",
                                  list("None" = 0,
                                       "Extreme values" = "extreme",
                                       "1% and 99% percentile" = 1,
                                       "2.5% and 97.5% percentile" = 2,
                                       "5% and 95% percentile" = 3,
                                       "10% and 90% percentile" = 4),
                                  selected = c(0, 3), multiple = TRUE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `selected-text-format` = "count > 3"))),
            column(3, radioGroupButtons(ns("trim_per_row"), "Graphs per row",
                                        c(1, 2, 3), selected = 2))
          ),
          uiOutput(ns("plot_trim")) %>% add_spinner()
        )
      ),
      
      # Covariate Balance Table ------------------------------------------------
      tabPanel(
        "Covariate Balance Table",
        shinydashboardPlus::box(
          width = 12,
          title = "Covariate Balance Table by Trimming Method",
          dropdownMenu = mod_download_ui(ns("download_tbl_trim"), format = "table"),
          fluidRow(
            column(3, selectInput(ns("tbl_trim_approach"), "Trimming Approach",
                                  c("Replace", "Remove"))),
            column(3, pickerInput(ns("tbl_trim_method"), "Trimming Method",
                                  list("None" = 0,
                                       "Extreme values" = "extreme",
                                       "1% and 99% percentile" = 1,
                                       "2.5% and 97.5% percentile" = 2,
                                       "5% and 95% percentile" = 3,
                                       "10% and 90% percentile" = 4),
                                  selected = 3, multiple = TRUE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `selected-text-format` = "count > 3"))),
            column(6, pickerInput_vars(ns("tbl_trim_cov"), "Covariates"))
          ),
          uiOutput(ns("table_covbal")) %>% add_spinner()
        )
      )
      
    )
  )
}

# Module server function
mod_main_ps_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Upload file
    observe({
      req(input$file)
      rv$df_ps <- upload_file(input$file)
    })
    
    # Reset fileInput
    observeEvent(input$reset_file, {
      rv$df_ps <- NULL
      shinyjs::reset("file")
    })
    
    # Update variables
    observe({
      choices <- names(rv$df_ps)
      updateSelectInput(session, "imput", choices = choices,
                        selected = grep_var("imputation", choices))
      updateSelectInput(session, "study", choices = choices,
                        selected = grep_var("study", choices))
      updateSelectInput(session, "trt", choices = choices,
                        selected = grep_var("trt", choices))
      updateSelectInput(session, "weight", choices = choices,
                        selected = grep_var("stwgt", choices))
      updateSelectInput(session, "pscore", choices = choices,
                        selected = grep_var("pscore", choices))
    })
    
    # Column names
    observe({
      choices <- names(rv$df_ps)
      choice_labels <- var_labels(rv$df_ps)
      updatePickerInput(session, "columns", choices = choices, selected = choices,
                        choicesOpt = list(subtext = choice_labels))
    })
    
    # Data to display
    data_display <- reactive({
      req(rv$df_ps)
      select(rv$df_ps, any_of(input$columns))
    })
    
    # Main data table
    output$table_data <- DT::renderDT({
      data_display() %>%
        data_table_fn()
    })
    
    # Select Imputation numbers ------------------------------------------------
    observe({
      choices <- unique(rv$df_ps[[input$imput]])
      updatePickerInput(session, "imput_num", 
                        selected = choices[c(1, 2)], choices = choices)
      updateSelectInput(session, "trim_num", 
                        selected = choices[1], choices = choices)
    })
    # Covariates
    observe({
      choices <- names(rv$df_ps)
      choice_labels <- var_labels(rv$df_ps)
      remove <- grep("imputation|id$|trt|^ps|logit|wgt|wt$|fl$", choices, TRUE)
      selected <- setdiff(choices, choices[remove])
      updatePickerInput(session, "tbl_trim_cov", 
                        choices = choices, selected = selected,
                        choicesOpt = list(subtext = choice_labels))
    })
    
    # Find pscores corresponding to edge weights
    find_ps_edge <- function(data, quantile, is.extreme) {
      ps_left <- vector(length = nrow(quantile))
      ps_right <- vector(length = nrow(quantile))
      
      # Loop for Covariance Balance tables with multiple imputations
      for(i in 1:nrow(quantile)) {
        quantile_i <- quantile[i, ]
        df_i <- filter(data, .data[[input$imput]] == quantile_i[[input$imput]])
        left <- filter(df_i, .data[[input$weight]] == quantile_i$q_left)
        right <- filter(df_i, .data[[input$weight]] == quantile_i$q_right)
        ps_left[i] <- left %>%
          select(all_of(input$pscore)) %>%
          purrr::pluck(1)
        ps_right[i] <- right %>%
          select(all_of(input$pscore)) %>%
          purrr::pluck(1)
      }
      cbind(quantile, ps_left = ps_left, ps_right = ps_right)
    }
    
    # Apply trimming approach and method to dataframe
    df_trim <- function(df, trim_approach, trim_method) {
      if(trim_method != 0) {
        
        # Extreme value trimming
        if(trim_method == "extreme") {
          quantile <- df %>%
            group_by(.data[[input$imput]], .data[[input$study]]) %>%
            summarize(q_left = min(.data[[input$weight]]),
                      q_right = max(.data[[input$weight]])) %>%
            group_by(.data[[input$imput]]) %>%
            summarize(q_left = max(q_left),
                      q_right = min(q_right))
          
          if(input$plot_var == "Propensity Score") {
            quantile <- find_ps_edge(df, quantile, is.extreme = TRUE)
          }
          df <- left_join(df, quantile, by = input$imput)
          
          # Quantile trimming
        } else {
          if(trim_method == 1) {
            left <- 0.01
            right <- 0.99
          } else if(trim_method == 2) {
            left <- 0.025
            right <- 0.975
          } else if(trim_method == 3) {
            left <- 0.05
            right <- 0.95
          } else if(trim_method == 4) {
            left <- 0.1
            right <- 0.9
          }
          quantile <- df %>%
            group_by(.data[[input$imput]]) %>%
            summarize(q_left = quantile(.data[[input$weight]], left, type = 2),
                      q_right = quantile(.data[[input$weight]], right, type = 2))
          
          if(input$plot_var == "Propensity Score") {
            quantile <- find_ps_edge(df, quantile, is.extreme = FALSE)
          }
          df <- left_join(df, quantile, by = input$imput)
        }
        
        # Replace or removed trimmed values
        if(trim_approach == "Replace") {
          if(input$plot_var == "Propensity Score") {
            df <- df %>%
              mutate(across(input$pscore, 
                            ~ifelse(.data[[input$weight]] < q_left, ps_left, .)),
                     across(input$pscore, 
                            ~ifelse(.data[[input$weight]] > q_right, ps_right, .)))
          } else {
            df <- df %>%
              mutate(across(input$weight, ~ifelse(. < q_left, q_left, .)),
                     across(input$weight, ~ifelse(. > q_right, q_right, .)))
          }
        } else if(trim_approach == "Remove") {
          df <- df %>%
            filter(.data[[input$weight]] > q_left & .data[[input$weight]] < q_right)
        }
      }
      df
    }
    
    # Create PS distribution plots
    create_psdist <- function(df, title) {
      if(input$plot_var == "Propensity Score") {
        var <- input$pscore
      } else {
        var <- input$weight
      }
      
      # Density plot + histogram
      p_density <- ggplot(df, aes(x = .data[[var]])) +
        geom_histogram(aes(y = after_stat(density), 
                           fill = .data[[input$study]]),
                       position = "identity", bins = 8, alpha = 0.4) +
        geom_density(aes(color = .data[[input$study]])) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        theme_light() +
        theme(legend.position = "bottom")
      
      if(input$plot_var == "Propensity Score") {
        p_density <- p_density +
          scale_x_continuous(limits = c(-0.2, 1.2), breaks = seq(-0.2, 1.2, 0.2)) +
          labs(x = "Estimated Probability",
               y = "Density")
      } else {
        p_density <- p_density +
          labs(x = "Stabilize Weight",
               y = "Density")
      }
      
      # Boxplot
      p_boxplot <- ggplot(df, aes(x = .data[[input$study]],
                                  y = .data[[var]],
                                  fill = .data[[input$study]],
                                  color = .data[[input$study]])) +
        geom_boxplot(alpha = 0.4) +
        scale_x_discrete(limits = rev) +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        theme_light() +
        theme(legend.position = "none")
      
      if(input$plot_var == "Propensity Score") {
        p_boxplot <- p_boxplot +
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
          labs(x = "",
               y = "Estimated Probability")
      } else {
        p_boxplot <- p_boxplot +
          labs(x = "",
               y = "Stabilize Weight")
      }
      
      # Summary table
      t_summary <- df %>%
        rename(`Study ID` = all_of(input$study)) %>%
        group_by(`Study ID`) %>%
        summarize(n = n(),
                  Minimum = min(.data[[var]], na.rm = TRUE),
                  Q1 = quantile(.data[[var]], 0.25, type = 2),
                  Median = quantile(.data[[var]], 0.5, type = 2),
                  Q3 = quantile(.data[[var]], 0.75, type = 2),
                  Maximum = max(.data[[var]], na.rm = TRUE),
                  Mean = mean(.data[[var]], na.rm = TRUE),
                  "Std Dev" = sd(.data[[var]], na.rm = TRUE)) %>%
        mutate(across(!all_of(c("Study ID", "n")), ~sprintf("%.3f", .)))
      
      # Return list
      list(title, p_density, p_boxplot, t_summary)
    }
    
    # Arrange and display plots and table
    display_plots <- function(x, rows) {
      if(rows == 1) {
        col_width <- 12
      } else if(rows == 2) {
        col_width <- 6
      } else if(rows == 3) {
        col_width <- 4
      }
      
      # Convert plots and table
      p_density <- plotly::ggplotly(x[[2]]) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2)) %>%
        custom_config("ps_dist")
      p_boxplot <- plotly::ggplotly(x[[3]]) %>%
        custom_config("ps_boxplot")
      t_summary <- DT::datatable(x[[4]], 
                                 rownames = FALSE,
                                 class = "row-border",
                                 selection = "none",
                                 options = list(dom = "t", ordering = FALSE))
      # UI chunk
      div(
        column(col_width,
               fluidRow(column(12, align = "center", 
                               renderUI(HTML(paste0("<h4>", x[[1]], "</h4>"))))),
               plotly::renderPlotly(p_density),
               plotly::renderPlotly(p_boxplot),
               div(DT::renderDT(t_summary), style = "font-family:monospace; font-size:small"),
               br()
        )
      )
    }
    
    # Plots by imputation ------------------------------------------------------
    psdist_imput_plots <- reactive({
      psdist_imput_fn <- function(n) {
        df <- rv$df_ps %>%
          filter(.data[[input$imput]] == n) %>%
          df_trim(input$imput_approach, input$imput_method)
        create_psdist(df, paste("Imputation Number:", n))
      }
      purrr::map(input$imput_num, psdist_imput_fn)
    })
    output$plot_imput <- renderUI({
      plots <- psdist_imput_plots()
      tagList(purrr::map(plots, display_plots, rows = input$imput_per_row))
    })
    
    # Consolidated boxplots
    output$plot_imput_boxplot <- plotly::renderPlotly({
      req(input$imput_num)
      if(input$plot_var == "Propensity Score") {
        var <- input$pscore
      } else {
        var <- input$weight
      }
      df <- rv$df_ps %>%
        filter(.data[[input$imput]] %in% input$imput_num) %>%
        mutate(across(input$imput, as.factor)) %>%
        df_trim(input$imput_approach, input$imput_method)
      
      p <- ggplot(df, aes(x = .data[[input$imput]],
                          y = .data[[var]],
                          fill = .data[[input$study]],
                          color = .data[[input$study]])) +
        geom_boxplot(alpha = 0.4) +
        scale_x_discrete(limits = rev) +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        theme_light() +
        theme(legend.position = "none") +
        facet_grid(cols = vars(.data[[input$study]]))
      
      if(input$plot_var == "Propensity Score") {
        p <- p +
          labs(x = "Imputation",
               y = "Estimated Probability")
      } else {
        p <- p +
          labs(x = "Imputation",
               y = "Stabilize Weight")
      }
      p %>%
        plotly::ggplotly() %>%
        custom_config("ps_boxplot")
    })
    output$plot_imput_boxplot_ui <- renderUI({
      height <- length(input$imput_num) * 40
      plotly::plotlyOutput(ns("plot_imput_boxplot"), height = max(200, height))
    })
    
    # Plots by trimming method -------------------------------------------------
    psdist_trim_plots <- reactive({
      req(input$trim_num)
      psdist_trim_fn <- function(method) {
        df_i <- rv$df_ps %>%
          filter(.data[[input$imput]] == input$trim_num)
        df <- df_trim(df_i, input$trim_approach, method)
        create_psdist(df, paste("Trimmed:", method_desc[[as.character(method)]]))
      }
      purrr::map(input$trim_method, psdist_trim_fn)
    })
    output$plot_trim <- renderUI({
      plots <- psdist_trim_plots()
      tagList(purrr::map(plots, display_plots, rows = input$trim_per_row))
    })
    
    # Create Covariate Balance table -------------------------------------------
    create_covbal <- function(method) {
      vars <- input$tbl_trim_cov
      req(vars)
      validate(
        need(length(vars) >= 2, "Please select at least two covariates.")
      )
      df <- df_trim(rv$df_ps, input$tbl_trim_approach, method)
      
      # Construct a table
      tabUnmatched <- tableone::CreateTableOne(vars = vars,
                                               strata = input$trt,
                                               data = df,
                                               test = FALSE)
      # Weighted data
      rhcSvy <- survey::svydesign(ids = ~ 1,
                                  data = df,
                                  weights = ~ df[[input$weight]])
      
      # Construct a table
      tabWeighted <- tableone::svyCreateTableOne(vars = vars,
                                                 strata = input$trt,
                                                 data = rhcSvy,
                                                 test = FALSE)
      resCombo <- cbind(print(tabUnmatched, pDigits = 4, varLabels = TRUE,
                              smd = TRUE, printToggle = FALSE),
                        print(tabWeighted, pDigits = 4, varLabels = TRUE,
                              smd = TRUE, printToggle = FALSE))
      t1 <- as_tibble(resCombo[-1, ], 
                      .name_repair = ~as.character(1:6), 
                      rownames = "Covariate") %>%
        # Remove " (mean (SD))" from Covariate column
        mutate(Covariate = sub(" \\(mean \\(SD\\)\\)", "", Covariate))
      
      # Add negative signs back into SMD columns
      comp1 <- as.numeric(sub(" \\(.*", "", t1$`2`)) - as.numeric(sub(" \\(.*", "", t1$`1`))
      t1$`3` <- ifelse(comp1 < 0, sub(" ", "-", t1$`3`), t1$`3`)
      comp1 <- as.numeric(sub(" \\(.*", "", t1$`5`)) - as.numeric(sub(" \\(.*", "", t1$`4`))
      t1$`6` <- ifelse(comp1 < 0, sub(" ", "-", t1$`6`), t1$`6`)
      list(method, t1)
    }
    method_desc <- list("0" = "None",
                        "extreme" = "Extreme values",
                        "1" = "1% and 99% percentile",
                        "2" = "2.5% and 97.5% percentile",
                        "3" = "5% and 95% percentile",
                        "4" = "10% and 90% percentile")
    
    # Create Covariate Balance gt table
    display_covbal <- function(x) {
      tbl <- x[[2]] %>%
        gt::gt(rowname_col = "Covariate") %>%
        gt::tab_header("Covariate Balance Before and After Balancing Using Trimmed Stabilized IPTW",
                       paste("Trimmed:", method_desc[[as.character(x[[1]])]])) %>%
        gt::tab_spanner("Before Balancing", 2:4) %>%
        gt::tab_spanner("After Balancing", 5:7) %>%
        gt::tab_stubhead("Covariate") %>%
        gt::tab_style(
          gt::cell_text(align = "left"),
          gt::cells_stub()
        ) %>%
        gt::tab_style(
          gt::cell_text(align = "right"),
          gt::cells_body()
        ) %>%
        gt::tab_style(
          gt::cell_text(align = "center"),
          gt::cells_column_labels()
        ) %>%
        gt::cols_label("1" = "Control",
                       "2" = "Active",
                       "3" = "SMD",
                       "4" = "Control",
                       "5" = "Active",
                       "6" = "SMD")
      div(gt::render_gt(tbl), br())
    }
    
    # Covariate Balance tables
    covbal_tables <- reactive({
      purrr::map(input$tbl_trim_method, create_covbal)
    })
    output$table_covbal <- renderUI({
      tbls <- covbal_tables()
      tagList(purrr::map(tbls, display_covbal))
    })
    
    # Download -----------------------------------------------------------------
    
    # Create report with ps distribution plots and tables
    plot_report <- function(plots) {
      add_plots <- function(x, num) {
        plot <- plots[[num]]
        txt <- reporter::create_text(plot[[1]], align = "center")
        plt_dens <- reporter::create_plot(plot[[2]], height = 4, width = 6.5)
        plt_box <- reporter::create_plot(plot[[3]], height = 2, width = 6.5)
        tbl <- plot[[4]]
        names(tbl)[c(1, 9)] <- c("StudyID", "StdDev")
        tbl <- reporter::create_table(tbl, width = 6) %>%
          reporter::column_defaults(2:9, align = "right") %>%
          reporter::define(StudyID, "Study ID") %>%
          reporter::define(StdDev, "Std Dev")
        x %>%
          reporter::add_content(txt, page_break = FALSE) %>%
          reporter::add_content(plt_dens, page_break = FALSE) %>%
          reporter::add_content(plt_box, page_break = FALSE) %>%
          reporter::add_content(tbl)
      }
      rpt <- reporter::create_report(orientation = "portrait", font = "Arial") %>%
        reporter::set_margins(1, 1, 1, 1)
      for(i in 1:length(plots)) rpt <- add_plots(rpt, i)
      rpt
    }
    
    # Reports for plots
    imput_plot_report <- reactive(plot_report(psdist_imput_plots()))
    trim_plot_report <- reactive(plot_report(psdist_trim_plots()))
    
    # Create report with covariate balance tables
    covbal_table_report <- reactive({
      tables <- covbal_tables()
      add_tables <- function(x, num) {
        table <- tables[[num]]
        tbl <- reporter::create_table(table[[2]], width = 6.5) %>%
          reporter::column_defaults(2:7, align = "right", label_align = "center") %>%
          reporter::define("Covariate", width = 1.5) %>%
          reporter::define("1", "QCC") %>%
          reporter::define("2", "LKC") %>%
          reporter::define("3", "SMD") %>%
          reporter::define("4", "ACC") %>%
          reporter::define("5", "LKC") %>%
          reporter::define("6", "SMD") %>%
          reporter::spanning_header(from = 2, to = 4, label = "Before Balancing",
                                    level = 2) %>%
          reporter::spanning_header(from = 5, to = 7, label = "After Balancing",
                                    level = 2) %>%
          reporter::titles("Covariate Balance Before and After Balancing Using Trimmed Stabilized IPTW",
                           method_desc[[as.character(table[[1]])]])
        x %>%
          reporter::add_content(tbl)
      }
      rpt <- reporter::create_report(orientation = "portrait", font = "Arial") %>%
        reporter::set_margins(1, 1, 1, 1)
      for(i in 1:length(tables)) rpt <- add_tables(rpt, i)
      rpt
    })
    
    # Uploaded data
    mod_download_server("download_data", data_display,
                        filename = "data", format = "data")
    
    # Plots for each selected imputation
    mod_download_server("download_plot_imput", imput_plot_report,
                        filename = "ps_imput", format = "graph")
    
    # Plots for each selected trimming method
    mod_download_server("download_plot_trim", trim_plot_report,
                        filename = "ps_trim", format = "graph")
    
    # Covariate balance tables
    mod_download_server("download_tbl_trim", covbal_table_report,
                        filename = "cov_bal", format = "table")
    
  })
}
