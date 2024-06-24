mod_graph_waterfall_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "File Upload",
      collapsible = TRUE,
      dropdownMenu = reset_dropdown(ns("waterfall")),
      fluidRow(
        column(6, fileInput(ns("waterfall_efficacy"), "Choose an Eficacy file", 
                            accept = file_accept))
      ),
      fluidRow(
        column(2, selectInput(ns("waterfall_param"), "Parameter", NULL)),
        column(3, selectInput(ns("waterfall_pchg"), "Change from Baseline", NULL)),
        column(2, numericInput(ns('waterfall_cap1'), "Positive Cap Value", 100, min=0, max=200)),
        column(2, numericInput(ns('waterfall_cap2'), "Negative Cap Value", -100, min=-100, max=0)),
        column(2, numericInput(ns('waterfall_ref'), "Reference Line", 0, min=-100, max=200))
      )
    ),
    shinydashboardPlus::box(width = 9,
                            title = "Waterfall Plot",
                            fluidRow(
                              column(3, selectInput(ns("waterfall_param_sel"), "Parameter Select", NULL)),
                              column(3, selectInput(ns('waterfall_color'), "Color Variable", NULL))
                            ),
                            plotOutput(ns("waterfall_plot"))
    ),
    
    mod_sub_varfilter_ui(ns("filter"))
    
  )
}



mod_graph_waterfall_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      df_waterfall = NULL
    )
    

    # Waterfall plot -------------------------------------------------------------
    observe({
      req(input$waterfall_efficacy)
      rv$df_waterfall <- upload_file(input$waterfall_efficacy)
    })
    
    # Create Waterfall plot
    # Reset fileInputs
    observeEvent(input$waterfall_reset, {
      rv$df_waterfall <- NULL
      reset("waterfall_efficacy")
    })
    
    observe({
      choices <- names(rv$df_waterfall)
      var_choices <- grep("id$|^trt", choices, TRUE, value = TRUE, invert = TRUE)
      updateSelectInput(session, "waterfall_param", choices = choices,
                        selected = grep_var("param", choices))
      updateSelectInput(session, "waterfall_pchg", choices = choices,
                        selected = grep_var("pchg", choices))
      updateSelectInput(session, "waterfall_color", choices = choices,
                        selected = grep_var("trt01", choices))
      updatePickerInput(session, "waterfall_filter", choices = var_choices)
    })
    
    observe({
      choices <- unique(rv$df_waterfall[[input$waterfall_param]])
      updateSelectInput(session, "waterfall_param_sel", choices = choices)
    })
    
    main <- reactive({
      req(rv$df_waterfall)
      process_file(rv$df_waterfall)
    })
    
    # Display filter variable selections
   
    
    df_filter <- mod_sub_varfilter_server("filter", main)  
 
    
    df_waterfall <- reactive({
      req(input$waterfall_param_sel)
      df <- df_filter()
      req(df)
      
      df <- df %>%
        dplyr::filter(.data[[input$waterfall_param]] == input$waterfall_param_sel) 
      
  
    })
    
    waterfall_plot <- reactive({
      
      ef <- df_waterfall()
      ef <- ef %>%
        arrange(desc(.data[[input$waterfall_pchg]])) %>%
        mutate(obs = row_number())
      
      N <- dim(ef)
      
      ef <- ef %>% 
        mutate(yvar=.data[[input$waterfall_pchg]]) %>%
        mutate(yvar=ifelse(yvar > input$waterfall_cap1, input$waterfall_cap1, yvar)) %>%
        mutate(yvar=ifelse(yvar < input$waterfall_cap2, input$waterfall_cap2, yvar)) 
      
      # define plot
      if(input$waterfall_ref!=0){
        ggplot(ef, aes(x = obs)) +
          geom_col(aes(y = yvar, fill=.data[[input$waterfall_color]]), 
                   width = 0.6, position = position_dodge(width = 0.9), alpha = 0.9) +
          coord_cartesian(ylim=c(input$waterfall_cap2, input$waterfall_cap1)) +
          scale_color_manual(name = "",  label=ef[[input$waterfall_color]]) +
          geom_hline(yintercept = input$waterfall_ref, linetype = "dashed") +   
          labs(x = paste0("Number of Subjects: N=", N[1]), y = "% Change from Baseline", 
               title = paste("Waterfall Plot of Change in", input$waterfall_param_sel), 
               subtitle = "Test Plot") +
          theme_minimal() +
          theme(legend.position = c(0.8, 0.8), legend.title = element_blank())
      }
      else{
        ggplot(ef, aes(x = obs)) +
          geom_col(aes(y = yvar, fill=.data[[input$waterfall_color]]), 
                   width = 0.6, position = position_dodge(width = 0.9), alpha = 0.9) +
          coord_cartesian(ylim=c(input$waterfall_cap2, input$waterfall_cap1)) +
          scale_color_manual(name = "",  label=ef[[input$waterfall_color]]) +
          #geom_hline(yintercept = input$waterfall_ref, linetype = "dashed") +   
          labs(x = paste0("Number of Subjects: N=", N[1]), y = "% Change from Baseline", 
               title = paste("Waterfall Plot of Change in", input$waterfall_param_sel), 
               subtitle = "Test Plot") +
          theme_minimal() +
          theme(legend.position = c(0.8, 0.8), legend.title = element_blank())  
      }  
    })
    
    output$waterfall_plot <- renderPlot({
      waterfall_plot()
    })
    
  })
}