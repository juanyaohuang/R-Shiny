mod_graph_forest1_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    id,
    shinydashboardPlus::box(
        width = 12,
        title = "File Upload",
        collapsible = TRUE,
        dropdownMenu = reset_dropdown(ns("forest")),
        fluidRow(
          column(6, fileInput(ns("forest_adsl"), "Choose an ADSL file", 
                              accept = file_accept)),
          column(6, fileInput(ns("forest_adrs"), "Choose an ADRS", 
                              accept = file_accept))
        ),
        fluidRow(
          column(3, selectInput(ns("forest_param"), "Parameter", NULL)),
          column(3, pickerInput_vars(ns("forest_subgrp"), "Subgroup")),
          column(3, numericInput(ns('height'), "Plot Height", 400, min=400, max=2000))
        )
    ),
    shinydashboardPlus::box(width = 12,
        title = 'Input Your Own Labels',
        fluidRow(
          column(12, uiOutput(ns("label_inputs"))),
          column(3, actionButton(ns("submit_labels"), "Submit Labels"))
        )
    ),
    shinydashboardPlus::box(width = 9,
        title = "Forest Plot",
        fluidRow(
          column(6, selectInput(ns("forest_param_sel"), "Parameter Select", NULL))
        ),
        plotOutput(ns("forest_plot"))
    ),
    mod_sub_varfilter_ui(ns("filter"))
  ) 
}



mod_graph_forest1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  #Get Labels of ADSL variables
  #Get Labels of ADSL variables
  data <- reactive({
    req(input$forest_adsl)
    sas_data <- read_sas(input$forest_adsl$datapath)
    label_lookup_map <- tibble(
      col_name = sas_data %>% names(),
      label = sas_data %>% map_chr(attr_getter("label"))  )
  })
  output$data_table <- renderTable({
    data()
  })
  
  rv <- reactiveValues(
    df_forest_adsl = NULL,
    df_forest_adrs = NULL,
  )
  
  #This is the function to generate the data that is used to draw the plot.
  #This function is for each individual subgroup variable selected.
  #All data of each each individual subgroup will be combined later.
  g<-function(var){
    adsl <- mutate(df_forest_adsl(), CATEGORY=.data[[var]])
    adsl <- mutate(adsl, CATEGORY=ifelse(is.na(CATEGORY), 'Missing', CATEGORY))
    adrs <- df_forest_adrs()           
    BOR <- merge(select(adrs, SUBJID, PARAM, AVALC, AVAL), adsl, by="SUBJID") # Merge adrs and adsl datasets by SUBJID
    BOR$ORR <- ifelse(BOR$AVALC %in% c("CR", "PR") | BOR$AVAL==1, 1, 2) # Create ORR variable based on avalc values

    ORR <- function(data){
      N1 <- dim(subset(data, ORR==1)) # Calculate N1
      N1 <- N1[1]
      N2 <- dim(data) # Calculate N2
      N2 <- N2[1]
      
      freq <- binom.test(N1, N2, conf.level=0.95) # Perform binomial test to calculate ORR and confidence interval
      rr <- round(freq$estimate*100, 1)
      rrLowerCL <- round(freq$conf.int[1]*100, 1)
      rrUpperCL <- round(freq$conf.int[2]*100, 1)
      
      orr_ci1 <- data.frame(rr=rr, rrLowerCL=rrLowerCL, rrUpperCL=rrUpperCL) # Create orr_ci1 dataframe
      
      result <- data.frame(var1=paste0(N1, "/", N2), 
                           var2=paste0(format(rr, nsmall=1), " (", format(rrLowerCL, nsmall=1), " - ", format(rrUpperCL, nsmall=1), ")"), 
                           rr, rrLowerCL, rrUpperCL) # Create result dataframe
      
      return(result)
    }
    
    # Using 'by()' to apply custom function to each group  
    result_by <- by(BOR, BOR$CATEGORY, ORR)
    
    # Convert the output of 'by()' to a data frame
    result_df <- do.call(rbind, as.list(result_by))
    
    value <- BOR %>%
      arrange(CATEGORY) %>%
      distinct(CATEGORY, .keep_all = F)
    result_df$label <- value$CATEGORY
    result_df <- mutate(result_df, label=paste0('      ', label))
    
    return(result_df)
  }
  #End of the g function
  
  
  # Forest plot -------------------------------------------------------------
  observe({
    req(input$forest_adsl)
    rv$df_forest_adsl <- upload_file(input$forest_adsl)
    req(input$forest_adrs)
    rv$df_forest_adrs <- upload_file(input$forest_adrs)
  })
  
  # Create forest plot
  # Reset fileInputs
  observeEvent(input$forest_reset, {
    rv$df_forest_adsl <- NULL
    reset("forest_adsl")
    rv$df_forest_adrs <- NULL
    reset("forest_adrs")
  })
  
  observe({
    choices <- names(rv$df_forest_adsl)
    updatePickerInput(session, "forest_subgrp", choices = choices, selected = grep_var("sex", choices))
    updatePickerInput(session, "forest_filter", choices = choices)
  })
  
  observe({
    choices <- names(rv$df_forest_adrs)
    updateSelectInput(session, "forest_param", choices = choices, selected = grep_var("param", choices))
  })
  
  observe({
    choices <- unique(rv$df_forest_adrs[[input$forest_param]])
    updateSelectInput(session, "forest_param_sel", choices = choices)
  })
  
  
  main1 <- reactive({
    req(rv$df_forest_adsl)
    process_file(rv$df_forest_adsl)
  })
  
  main2 <- reactive({
    req(rv$df_forest_adrs)
    process_file(rv$df_forest_adrs)
  })
  
  df_filter1 <- mod_sub_varfilter_server("filter", main1)
  
  
  df_forest_adsl <- reactive({
    df <- df_filter1()
    req(df)
  })
  
  df_forest_adrs <- reactive({
    req(input$forest_param_sel)
    df <- main2()
    req(df)
    
    df <- df %>%
      dplyr::filter(.data[[input$forest_param]] == input$forest_param_sel) 
  })
  
  ##############################
  #### Get User-Input Label ####
  ##############################
  filter_var_label <- reactive({
    req(data)
    df2 <- data()
    var_n <- length(input$forest_subgrp)
    for (i in 1:var_n){
      var_i <- input$forest_subgrp[i]
      df <- subset(df2, col_name==var_i)
      assign(paste0('df_', i), df)
    }
    filter_var_label <- rbindlist(mget(ls(pattern = "^df_")))
  })
  
  
  output$label_inputs <- renderUI({
    req(filter_var_label)
    df<-filter_var_label()
    lapply(seq_len(nrow(df)), function(i) {
      var_name <- df$col_name[i]
      label_value <- df$label[i]
      
      div(textInput(ns(paste0("label_", i)), label = var_name, value = label_value),
          style = "display: inline-block;")
    })
  })
  
  generated_dataframe <- reactiveVal(NULL)
  observeEvent(input$submit_labels, {
    req(filter_var_label)
    df <- filter_var_label()
    updated_label <- data.frame(
      col_name = df$col_name,
      label = sapply(seq_len(nrow(df)), function(i) {
        input_id <- paste0("label_", i)
        print(input_id)
        input_value <- input[[input_id]]
        print(input_value)
        if (is.null(input_value)) {
          df$label[i]
        } else {
          input_value
        }
      })
    )
    generated_dataframe(updated_label)
  })
  
  output$outputTable <- renderTable({
    generated_dataframe()
  })
  
  
  
  ggplot_data <- reactive({
    df_forest_adsl <- df_forest_adsl()
    req(df_forest_adsl)
    df_forest_adrs <- df_forest_adrs()
    req(df_forest_adrs)
    
    var_n <- length(input$forest_subgrp)
    for (i in 1:var_n){
      var_i <- input$forest_subgrp[i]
      g_data <- g(var_i)
      g_data$rowname <- var_i
      assign(paste0('g_data_', i), g_data)
    }
    final_tbl <- rbindlist(mget(ls(pattern = "^g_data_"))) #combine individual subgroup data
  })
  
  
  forest_plot <- reactive({
    req(ggplot_data)
    result_df <- ggplot_data()
    result_df <- mutate(result_df, order=row_number())
    result_df <- mutate(result_df, order=order+1)
    
    
    req(filter_var_label)
    df <- filter_var_label()
    

    req(generated_dataframe)
    df_new <- generated_dataframe()
    

    req(data)
    label_lookup_map <- data()
    
    title <- result_df %>%
      mutate(label=rowname) %>%
      arrange(label, order) %>%
      group_by(label) %>%
      filter(row_number()==1) %>%
      ungroup()
    title <- mutate(title, rowname=NA, var1=NA, var2=NA, rr=NA, rrLowerCL=NA, rrUpperCL=NA, order=order-0.5)
    title <- rename(title, col_name=label)

    if (is_empty(df_new)==TRUE){title <-  merge(title, label_lookup_map, by='col_name')}
                       else{title <-  merge(title, df_new, by='col_name')}
    title <- select(title, -c('col_name'))
    
    
    
    result_df <- rbind(title, result_df) 
    result_df <- result_df %>%
      arrange(order) %>%
      mutate(order=row_number()) %>%
      mutate(order=order+1)
    result_df$low <-  result_df$order-0.01*(400/input$height)*dim(result_df)[1]
    result_df$high <- result_df$order+0.01*(400/input$height)*dim(result_df)[1]        
    
    ggplot(result_df, aes(y = order)) +
      geom_segment(aes(x = rrLowerCL, xend = rrUpperCL, y = order, yend = order), stat = "identity", position = position_dodge(width = 0),  linewidth = 0.3, alpha = 1) +
      geom_point(aes(x = rr, y = order), shape = 21, fill = "black", size = 3) +
      geom_segment(aes(x = rrLowerCL, xend = rrLowerCL, y = low, yend = high), stat = "identity", position = position_dodge(width = 0), linewidth = 0.3, alpha = 1) + 
      geom_segment(aes(x = rrUpperCL, xend = rrUpperCL, y = low, yend = high), stat = "identity", position = position_dodge(width = 0), linewidth = 0.3, alpha = 1) +
      #geom_vline(xintercept = ref, linetype = "dashed") +
      
      annotate("text", x = -95, y = 1, label = "Sub-groups:", hjust = 0, size = 3.0, fontface = "bold") +
      geom_text(aes(label = label, y=order, x=-95), size = 3.0, hjust=0) +
      
      annotate("text", x = -15, y = 1, label = "Responders/Patients", hjust = 0.5, size = 3.0, fontface = "bold") +
      geom_text(aes(label = var1, y=order, x=-15), size = 3.0) +
      
      annotate("text", x = 115, y = 1, label = "Response Rate (95% CI):", hjust = 0.5, size = 3.0, fontface = "bold") +
      geom_text(aes(label = var2, y=order, x=115), size = 3.0) +
      
      scale_x_continuous(
        limits = c(-100, 125), 
        breaks = seq(0, 100, by = 10)) +
      scale_y_reverse(expand = c(0.1, 0.1)) +
      theme_bw() + theme_classic() +
      theme(
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")  
  })
  
  observe({
    output$forest_plot <- renderPlot({
      forest_plot()
    }, height=input$height)
  })
  
  })
}
  