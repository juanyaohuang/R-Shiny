# Statistical summary

# Module UI function
mod_sub_statsummary_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Statistical Summary",
    shinydashboardPlus::box(
      width = 12, 
      title = "Statistical Summary",
      dropdownMenu = mod_download_ui(ns("download_summary"), format = "data"),
      sidebar = shinydashboardPlus::boxSidebar(
        id = ns("outlier_sidebar"), 
        width = 33,
        h4("Filter Range", align = "center"),
        uiOutput(ns("outlier_range_ui"))
      ),
      fluidRow(
        conditionalPanel(
          "output.show_var_type", ns = ns,
          column(3, selectInput(ns("var_type"), "Variable Type",
                                c("Quantitative", "Categorical")))
        ),
        conditionalPanel(
          "input.var_type == 'Categorical'", ns = ns,
          column(3, pickerInput_vars(ns("cat_var_sel")))
        ),
        conditionalPanel(
          "input.var_type == 'Quantitative'", ns = ns,
          column(3, pickerInput_vars(ns("quan_var_sel"))),
          column(6, 
                 wellPanel(
                   fluidRow(
                     column(12, align = "center", 
                            h4(strong("Outliers"), style = "margin:5px"))
                   ),
                   fluidRow(
                     column(8, numericRangeInput(ns("outlier_range"), 
                                                 "Filter Range", 
                                                 c(-Inf, Inf))),
                     div(column(4, div(awesomeCheckbox(
                       ns("outlier_auto"), 
                       help_link("Auto (IQR)")
                     )), id = ns("iqr_help")), style = "padding-top:32px"),
                     shinyBS::bsPopover(
                       ns("iqr_help"), NULL,
                       paste("Remove outliers based on interquartile range<br>",
                             "(value is 1.5 times greater than or less than IQR)")
                     )
                   ),
                   fluidRow(
                     column(6, align = "center",
                            actionButton(ns("reset_outlier"), 
                                         "Reset",
                                         style = "background-color:#fff")),
                     column(6,
                            actionButton(ns("toggle_outlier_sidebar"), 
                                         "Show more options",
                                         style = "background-color:#fff"))
                   ),
                   style = "padding-top:5px; padding-bottom:5px")
          )
        )
      ),
      DTOutput_div(ns("table_summary"))
    ),
    shinydashboardPlus::box(
      width = 12, 
      title = "Summary Graph",
      conditionalPanel("input.var_type == 'Quantitative'", ns = ns,
                       materialSwitch(ns("show_boxplot_points"),
                                      strong("Display all points"), 
                                      status = "primary",
                                      right = TRUE),
                       uiOutput(ns("summary_boxplot_ui"))),
      conditionalPanel("input.var_type == 'Categorical'", ns = ns,
                       radioGroupButtons(ns("graph_cat_sel"),
                                         choices = c("Pie", "Bar")),
                       uiOutput(ns("summary_cat_ui")))
    )
  )
}

# Module UI function
mod_sub_statsummary_server <- function(id, dataset, is.bds = FALSE,
                                    var_name = reactive("Variable")) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Visibility of Variable type select input
    output$show_var_type <- reactive(rv$show_var_type)
    outputOptions(output, "show_var_type", suspendWhenHidden = FALSE)
    
    # Select quantitative variables
    select_quan <- function(df) {
      select(df, where(is.numeric))
    }
    
    # Select categorical variables
    select_cat <- function(df) {
      df %>%
        select(where(is.character)) %>%
        select(-any_of(ends_with("DTC"))) %>%
        select(-any_of(contains("SUBJID")))
    }
    
    # Update Variable type select input
    observe({
      df <- dataset()
      req(df)
      
      # BDS data
      if(is.bds) {
        nonnumeric <- nonnumeric(df$Value[!is.na(df$Value)])
        all_numeric <- !any(nonnumeric)
        all_nonnumeric <- all(nonnumeric)
        
        if(all_numeric) {
          rv$show_var_type <- FALSE
        } else if(all_nonnumeric) {
          updateSelectInput(session, "var_type", selected = "Categorical")
          rv$show_var_type <- FALSE
        } else {
          rv$show_var_type <- TRUE
        }
        
        # Subject-level data
      } else {
        df_quan <- select_quan(df)
        df_cat <- select_cat(df)
        
        # Only quantitative
        if(ncol(df_quan) > 0 && ncol(df_cat) == 0) {
          rv$show_var_type <- FALSE
          # Only categorical
        } else if(ncol(df_quan) == 0 && ncol(df_cat) > 0) {
          updateSelectInput(session, "var_type", selected = "Categorical")
          rv$show_var_type <- FALSE
          # Both
        } else {
          rv$show_var_type <- TRUE
        }
      }
    })
    
    
    # Set variable pickers
    observe({
      df <- dataset()
      req(df)
      
      # BDS data
      if(is.bds) {
        if(!is.null(rv$df_quan)) {
          quan_vars <- unique(rv$df_quan$Variable)
          updatePickerInput(session, "quan_var_sel",
                            choices = quan_vars, selected = quan_vars)
        }
        if(!is.null(rv$summary_cat)) {
          cat_vars <- unique(rv$summary_cat[[1]])
          updatePickerInput(session, "cat_var_sel",
                            choices = cat_vars, selected = cat_vars)
        }
        # Subject-level data
      } else {
        quan_vars <- sort(names(select_quan(df)))
        cat_vars <- sort(names(select_cat(df)))
        cat_sel <- cat_vars[!cat_vars == "ARM"]
        updatePickerInput(session, "quan_var_sel",
                          choices = quan_vars, selected = quan_vars)
        updatePickerInput(session, "cat_var_sel",
                          choices = cat_vars, selected = cat_sel)
        
      }
    })
    
    # Separate quantitative and categorical values
    # Set value of show_var_type
    observe({
      df <- dataset()
      req(df)
      
      # BDS data
      if(is.bds) {
        df <- df %>%
          drop_na(Value) %>%
          arrange(Variable)
        
        # Separate variable types
        if(is.character(df$Value)) {
          
          # Categorical (also create summary)
          df_cat <- df %>%
            filter(is.na(as.numeric(Value))) %>%
            count(Variable, Value) %>%
            suppressWarnings()
          rv$summary_cat <- df_cat
          
          # Quantitative
          df_quan <- df %>%
            filter(!is.na(as.numeric(Value))) %>%
            mutate(Value = as.numeric(Value)) %>%
            suppressWarnings()
          rv$df_quan <- df_quan
          
        } else {
          # Only quantitative
          rv$df_quan <- df
        }
        
        # Subject-level data
      } else {
        df_quan <- select_quan(df)
        df_cat <- select_cat(df)
        
        show_quan <- TRUE
        show_cat <- TRUE
        if(ncol(df_quan) > 0 && ncol(df_cat) == 0) {
          show_cat <- FALSE
        } else if(ncol(df_quan) == 0 && ncol(df_cat) > 0) {
          show_quan <- FALSE
        }
        
        # Quantitative
        if(show_quan) {
          if(ncol(df_quan) > 0) {
            df_quan <- df_quan %>%
              pivot_longer(everything(), names_to = "Variable",
                           values_to = "Value") %>%
              drop_na(Value) %>%
              arrange(Variable)
            rv$df_quan <- df_quan
          } else {
            rv$df_quan <- NULL
            rv$summary_quan <- NULL
          }
        }
        # Categorical
        if(show_cat) {
          if(ncol(df_cat) > 0) {
            df_cat <- df_cat %>%
              pivot_longer(everything(), names_to = "Variable",
                           values_to = "Value") %>%
              drop_na(Value) %>%
              count(Variable, Value)
            rv$summary_cat <- df_cat
          } else {
            rv$summary_cat <- NULL
          }
        }
      }
    })
    
    # Filter outliers (quantitative)
    filtered_quan <- reactive({
      req(input$outlier_range)
      req(rv$df_quan)
      df <- rv$df_quan %>%
        filter(Value >= input$outlier_range[1],
               Value <= input$outlier_range[2])
      
      vars <- sort(unique(df$Variable))
      n <- length(vars)
      
      # Automatic removal based on IQR
      if(input$outlier_auto) {
        out_vec <- vector()
        for(i in 1:n) {
          df_i <- filter(df, Variable == vars[i])
          out_vec <- c(out_vec, find_outliers(df_i$Value))
        }
        df$out_flag <- out_vec
        df <- df %>%
          filter(out_flag == FALSE) %>%
          select(-out_flag)
      }
      
      # Filter each parameter
      test_var <- input$outlier_range_1
      if(isTruthy(test_var)) {
        df_final <- head(df, 0)
        for(i in 1:n) {
          var_i <- vars[i]
          var_range <- input[[paste0("outlier_range_", i)]]
          req(var_range)
          
          df_i <- df %>%
            filter(Variable == var_i,
                   Value >= var_range[1],
                   Value <= var_range[2])
          df_final <- rbind(df_final, df_i)
        }
        df_final
      } else {
        df
      }
    })
    
    # Create quantitative summary
    observe({
      df <- numeric_summary(filtered_quan(), "Variable", "Value")
      rv$summary_quan <- df
    })
    
    # Flag if min/max in summary is an outlier
    df_outlier_flag <- reactive({
      outlier_flag(filtered_quan(), rv$summary_quan, "Variable", "Value")
    })
    
    # Final filtered summary data frame (for download)
    df_summary <- reactive({
      if(input$var_type == "Quantitative") {
        req(rv$summary_quan)
        req(input$quan_var_sel)
        rv$summary_quan %>%
          filter(Variable %in% input$quan_var_sel) %>%
          mutate(Mean = round(Mean, 3),
                 "Std Dev" = round(`Std Dev`, 3))
      } else {
        req(input$cat_var_sel)
        rv$summary_cat %>%
          filter(Variable %in% input$cat_var_sel)
      }
    })
    
    # Summary table
    output$table_summary <- DT::renderDT({
      if(input$var_type == "Quantitative") {
        req(rv$summary_quan)
        req(input$quan_var_sel)
        cbind(rv$summary_quan, df_outlier_flag()) %>%
          filter(Variable %in% input$quan_var_sel) %>%
          mutate(Mean = round(Mean, 3),
                 "Std Dev" = round(`Std Dev`, 3)) %>%
          DT::datatable(options = list(
            columnDefs = list(list(targets = c(8, 9), visible = FALSE)))) %>%
          DT::formatStyle(c("Minimum", "Maximum"),
                          c("min_flag", "max_flag"),
                          color = DT::styleEqual(c(TRUE, FALSE), c("red", "black")))
      } else {
        df_summary() %>%
          DT::datatable(extensions = "RowGroup",
                        options = list(rowGroup = list(dataSrc = 1)))
      }
    })
    
    # Outliers -----------------------------------------------------------------
    
    # Update/reset outlier range input
    observeEvent(list(input$reset_outlier, rv$df_quan), {
      df <- rv$df_quan
      req(df)
      req(nrow(df) > 0)
      min <- min(df$Value, na.rm = TRUE)
      max <- max(df$Value, na.rm = TRUE)
      updateNumericRangeInput(session, "outlier_range", value = c(min, max))
    })
    
    # Outlier range select inputs
    outlier_range_input <- reactive({
      vars <- input$quan_var_sel
      df <- rv$df_quan
      req(vars)
      req(df)
      n <- length(vars)
      input_tagList <- vector("list", n)
      
      for(i in 1:n) {
        var_i <- vars[i]
        new_input_id <- paste0("outlier_range_", i)
        df_i <- filter(df, Variable == var_i)
        
        min <- min(df_i$Value, na.rm = TRUE) %>% suppressWarnings()
        max <- max(df_i$Value, na.rm = TRUE) %>% suppressWarnings()
        
        new_input <- div(
          numericRangeInput(ns(new_input_id),
                            paste0(i, ". ", var_i),
                            value = c(min, max),
                            min = min, max = max),
          shinyBS::bsPopover(ns(new_input_id), "Min, Max", 
                             paste0(min, ", ", max))
        )
        input_tagList[[i]] <- new_input
      }
      input_tagList
    })
    
    # Arrange outlier range selections
    output$outlier_range_ui <- renderUI({
      req(input$quan_var_sel)
      outlier_range <- outlier_range_input()
      lapply(seq_along(outlier_range), function(i) {
        column(12, outlier_range[[i]])
      })
    })
    
    # Show outlier range sidebar
    observeEvent(input$toggle_outlier_sidebar, {
      shinydashboardPlus::updateBoxSidebar("outlier_sidebar")
    })
    
    # Summary graphs -----------------------------------------------------------
    # Summary box plot (quantitative)
    output$summary_boxplot <- plotly::renderPlotly({
      req(input$quan_var_sel)
      df <- filtered_quan() %>%
        filter(Variable %in% input$quan_var_sel)
      req(nrow(df) > 0)
      summary_boxplot(df, "Variable", "Value", var_name(),
                      input$show_boxplot_points)
    })
    
    # Render summary box plot
    output$summary_boxplot_ui <- renderUI({
      req(input$quan_var_sel)
      req(filtered_quan())
      height <- length(input$quan_var_sel) * 40
      plotly::plotlyOutput(ns("summary_boxplot"), height = max(200, height))
    })
    
    # Summary pie charts (categorical)
    output$summary_pie <- plotly::renderPlotly({
      req(input$cat_var_sel)
      df <- rv$summary_cat %>%
        filter(Variable %in% input$cat_var_sel)
      req(nrow(df) > 0)
      custom_pie(df,
                 unique(df[[1]]),
                 label = "Value",
                 value = "n",
                 title_text = "",
                 show_legend = FALSE,
                 legend_title = NULL,
                 download_text = "-summary_chart")
    })
    
    # Render summary pie charts
    output$summary_pie_ui <- renderUI({
      req(input$cat_var_sel)
      req(rv$summary_cat)
      n <- length(input$cat_var_sel)
      height <- ceiling(n/3) * 500
      plotly::plotlyOutput(ns("summary_pie"), height = height)
    })
    
    # Render summary bar charts (categorical)
    output$summary_bar_ui <- renderUI({
      df <- rv$summary_cat %>%
        filter(Variable %in% input$cat_var_sel)
      params <- unique(df[[1]])
      tagList(purrr::map(params, ~create_barchart(df, ., "Value"))) %>%
        add_spinner()
    })
    
    # Choose between pie and bar charts (categorical)
    output$summary_cat_ui <- renderUI({
      if(input$graph_cat_sel == "Pie") {
        uiOutput(ns("summary_pie_ui"))
      } else if(input$graph_cat_sel == "Bar") {
        uiOutput(ns("summary_bar_ui")) %>%
          add_spinner()
      }
    })
    
    # Download summary as Excel/CSV file
    mod_download_server("download_summary", df_summary,
                        filename = "summary", format = "data")
  })
}
