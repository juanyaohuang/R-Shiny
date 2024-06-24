# Apply variable filter to data

# Module UI function
mod_sub_varfilter_ui <- function(id, is.table = TRUE) {
  ns <- NS(id)
  if(is.table) {
    shinydashboardPlus::box(
      width = 3,
      title = "Filters",
      fluidRow(column(12, pickerInput_vars(ns("filter_vars"), "Variables"))),
      fluidRow(uiOutput(ns("filter_ui"))),
      fluidRow(
        column(12, align = "center", 
               actionButton(ns("run_button"), "Update", class = "btn-primary"))
      )
    )
  } else {
    shinydashboardPlus::box(
      width = 12, 
      title = "Inclusion/Exclusion Criteria",
      collapsible = TRUE,
      fluidRow(column(6, pickerInput_vars(ns("filter_vars")))),
      fluidRow(uiOutput(ns("filter_ui"))),
      fluidRow(
        column(12, align = "center", 
               actionButton(ns("run_button"), "Update", class = "btn-primary"))
      )
    )
  }
  
}

# Module server function
mod_sub_varfilter_server <- function(id, dataset, is.table = TRUE) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update table when input data changes by simulating button click
    observeEvent(dataset(), {
      shinyjs::click("run_button")
    })
    
    # Update variable
    observe({
      choices <- names(dataset())
      choice_labels <- var_labels(dataset())
      updatePickerInput(session, "filter_vars", choices = choices,
                        choicesOpt = list(subtext = choice_labels))
    })
    
    # Create inputs for selecting variable values
    filter_inputs <- eventReactive(input$filter_vars, {
      params <- input$filter_vars
      n <- length(params)
      input_tagList <- vector("list", n)
      for(i in 1:n) {
        param <- params[i]
        new_input_id <- paste0("filter_", param)
        choices <- sort(unique(dataset()[[param]]))
        nonnumeric <- nonnumeric(choices)
        
        # Prevent reset of values
        new_input_value <- choices
        
        # Date variables
        if(lubridate::is.Date(choices) | lubridate::is.POSIXt(choices)) {
          if(is.null(input[[new_input_id]])) {
            new_input_value <- c(min(choices), max(choices))
          } else if(new_input_id %in% names(input)) {
            new_input_value <- input[[new_input_id]]
          }
          new_input <- dateRangeInput(ns(new_input_id), param,
                                      start = new_input_value[1],
                                      end = new_input_value[2],
                                      min = min(choices), max = max(choices)) %>%
            suppressWarnings()
          
          # Character variables (and numeric variables with few choices)
        } else if(any(nonnumeric) | length(choices) <= 3 | any(grepl("^0.+", choices))) {
          if(new_input_id %in% names(input)) {
            new_input_value <- input[[new_input_id]]
          }
          new_input <- pickerInput(ns(new_input_id), param,
                                   choices, selected = new_input_value,
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE,
                                                  `live-search` = TRUE,
                                                  `selected-text-format` = "count > 5"))
          # Numeric variables
        } else {
          choices <- as.numeric(choices)
          if(is.null(input[[new_input_id]])) {
            new_input_value <- c(min(choices), max(choices))
          } else if(new_input_id %in% names(input)) {
            new_input_value <- c(input[[new_input_id]], max(choices))
          }
          
          min <- min(choices, na.rm = TRUE) %>% suppressWarnings()
          max <- max(choices, na.rm = TRUE) %>% suppressWarnings()
          
          new_input <- div(
            numericRangeInput(ns(new_input_id), 
                              param,
                              value = c(new_input_value[1],
                                        new_input_value[2]),
                              min = min, max = max),
            shinyBS::bsPopover(ns(new_input_id), "Min, Max", 
                               paste0(min, ", ", max))
          )
        }
        input_tagList[[i]] <- new_input
      }
      input_tagList
    })
    
    # Arrange inputs
    output$filter_ui <- renderUI({
      filter_inputs <- filter_inputs()
      width <- ifelse(is.table, 12, 6)
      lapply(seq_along(filter_inputs), function(i) {
        column(width, filter_inputs[[i]])
      })
    })
    
    # Apply filters to table
    df_filter <- eventReactive(input$run_button, {
      params <- input$filter_vars
      if(is.null(params)) {
        df_final <- dataset()
        filter_values <- NULL
      } else {
        df_final <- dataset()
        
        # Store filter selections in case of population selection
        if(!is.table) {
          filter_values <- vector("list", length = length(params))
          names(filter_values) <- params
        }
        
        for(i in seq_along(params)) {
          param <- params[i]
          val <- input[[paste0("filter_", param)]]
          req(val)
          df_i <- dataset()
          
          # Store filter selections
          if(!is.table) filter_values[[i]] <- val
          
          # Date/numeric vs character values
          if(lubridate::is.Date(val) | lubridate::is.POSIXt(val) | is.numeric(val)) {
            df_i <- filter(df_i, between(.data[[param]],
                                         val[1], val[2])) %>%
              suppressWarnings()
          } else {
            df_i <- filter(df_i, .data[[param]] %in% val)
          }
          df_final <- intersect(df_final, df_i)
        }
      }
      
      if(is.table) {
        return(df_final)
      } else {
        # Also return filter selections in case of population selection
        return(list(df_final, filter_values))
      }
    })
    
    return(df_filter)
  })
}
