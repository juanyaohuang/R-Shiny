# Apply adjustments to tables based on Additional Options

# Module UI function
mod_sub_tableoption_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("option_ui"))
  )
}

# Module server function
mod_sub_tableoption_server <- function(id, tbls, params, header = "Variables") {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create option inputs
    option_inputs <- reactive({
      n <- length(params())
      input_tagList <- vector("list", n)
      for(i in 1:n) {
        var_i <- params()[i]
        new_input <- fluidRow(
          column(2, p(var_i)),
          column(2, numericInput(ns(paste0("position_", var_i)), NULL,
                                 value = i, min = 1, max = n)),
          column(8, textInput(ns(paste0("label_", var_i)), NULL)),
          # Default margins were -15px, which forced a horizontal scroll bar
          style = "margin-left:0px;margin-right:0px"
        )
        input_tagList[[i]] <- new_input
      }
      div(input_tagList,
          style = "overflow-y:auto;max-height:400px;")
    })
    
    # Arrange inputs
    output$option_ui <- renderUI({
      req(params())
      div(
        p(strong(header), align = "center"),
        fluidRow(
          column(2, p(strong("Variable"))),
          column(2, p(strong("Position"))),
          column(8, p(strong("Label"))),
          # Match margins of option_inputs
          style = "margin-left:0px;margin-right:0px"
        ),
        option_inputs(),
        hr(style = "border-top:1px solid #777; margin-bottom:10px")
      )
    })
    
    # Apply additional options to table
    apply_table_option <- reactive({
      if(!is.null(input[[paste0("position_", params()[1])]])) {
        var_n <- length(params())
        option_df <- data.frame(var = vector(length = var_n),
                                position = vector(length = var_n),
                                label = vector(length = var_n))
        for (i in 1:var_n) {
          var_i <- params()[i]
          pos_i <- input[[paste0("position_", var_i)]]
          req(pos_i)
          label_i <- input[[paste0("label_", var_i)]]
          option_df[i, ] <- c(var_i, pos_i, label_i)
        }
        
        # Variable order
        var_order <- option_df %>%
          mutate(position = as.numeric(position)) %>%
          arrange(position) %>%
          `$`(var)
        
        # Variable labels
        var_labels <- option_df %>%
          select(var, label)
        
        # Sort tables
        all_tbls <- tbls()[var_order]
      } else {
        all_tbls <- tbls()[params()]
      }
      
      # Combine tables
      final_tbl <- do.call(rbind, all_tbls) %>%
        tibble::rownames_to_column()
      
      # Replace row labels
      if(!is.null(input[[paste0("position_", params()[1])]])) {
        for(i in 1:nrow(var_labels)) {
          label_i <- var_labels$label[i]
          if(label_i != "") {
            final_tbl <- final_tbl %>%
              mutate(Variable = ifelse(rowname == paste0(var_labels$var[i], ".1"),
                                       label_i,
                                       Variable))
          }
        }
      }
      select(final_tbl, -rowname)
    })
    
    return(apply_table_option)
    
  })
}
