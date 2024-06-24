mod_graph_tepee_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "File Upload",
      collapsible = TRUE,
      dropdownMenu = reset_dropdown(ns("reset_file")),
      fluidRow(
        column(6, fileInput(ns("tepee"), "Choose an input file", 
                            accept = file_accept))
      )
    ),
    shinydashboardPlus::box(
      width = 12,
      title = "Plot Parameter",
      collapsible = TRUE,
      fluidRow(
        column(3, numericInput(ns('font_size'), "Font Size", 3, min=2, max=7, step=0.1)),
        column(3, numericInput(ns('label_position'), "Text Postion", 15, min=5, max=25, step=1)),
        column(3, pickerInput_multi(ns("disp_columns"), "Column Select")),
        column(3, pickerInput_multi(ns("disp_rows"), "Row Select")),
        column(3, numericInput(ns('height'), "Plot Height", 400, min=400, max=8000, step=100)),
        column(3, numericInput(ns('width'), "Plot Width", 900, min=600, max=2000, step=100)),
        column(3, numericInput(ns('legend_ncol'), "Legend Column", 1, min=1, max=15, step=1)),
        column(3, selectInput(ns("legend_position"), "Legend Position", choices = c("right", "bottom"), selected = 'right')),
        column(6, radioButtons(ns("display_pct"), "Display Percentage", choices = list("Yes" = 1, "No" = 2), selected = 1, inline=T))
      )
    ),
    shinydashboardPlus::box(width = 12,
                            collapsible = TRUE,
                            title = 'Customize the Colors',
                            fluidRow(
                              column(12, uiOutput(ns("color_input")))
                            )
    ),
    shinydashboardPlus::box(width = 12,
                            title = "Tepee Plot",
                            plotOutput(ns("tepee_plot"))
    )
  ) 
}



mod_graph_tepee_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      df_tepee = NULL,
    )
    
    #Function to create Teppe plot
  tepee <- function(indata) {
    n_row <- nrow(indata)
    n_col <- ncol(indata)
    vnames <- names(indata[,-1]) #Names of columns in the input file except the first column
    indata <- indata %>% 
              mutate(order = n_row - row_number() + 1)
    
    indata$total <- rowSums(indata[,2:n_col])
    
    for(i in 2:n_col) {
      if (i>2) {indata[, i]=indata[, i] + indata[, i-1]} #add columns from left to right 
    }
    
    #If do not display pct, we usually want to display the plot in a symmetric shape
    for(i in 2:n_col) {
      if (input$display_pct==2) {
        indata[, i]=indata[, i]-indata$total/2
      }
    }
        

    #This for loop put horizontal data into vertical 
    for (i in 2:n_col) {
      if (i==2) {
        if (input$display_pct==1) {
          therapy <- data.frame(indata[,1], 0, indata[,i], indata$order)
        }
        else {
          therapy <- data.frame(indata[,1], -indata$total/2, indata[,i], indata$order)
        }
      }
      else {
        therapy <- data.frame(indata[,1], indata[,i-1], indata[,i], indata$order)
      }
      
      names(therapy) <- c('group', 'percent_l', 'percent_h', 'group_order')
      therapy <- mutate(therapy, TERM=names(indata[,i]), treatment_order=i-1)
      assign(paste0('cnt_',i), therapy)
    }
    cnt <- rbindlist(mget(ls(pattern = "^cnt_")))
    cnt <- arrange(cnt, treatment_order, desc(group_order))

    #Find min percent_l and max percent_h within each level of group (the 1st column in the input file)#
    range_pct1 <- cnt %>% group_by(group) %>% summarise(min_pct = min(percent_l))
    range_pct2 <- cnt %>% group_by(group) %>% summarise(max_pct = max(percent_h))
    range_pct <- merge(range_pct1, range_pct2, by='group')
    cnt <- merge(cnt, range_pct, by='group')
    cnt <- arrange(cnt, treatment_order, desc(group_order))
    write.csv(cnt, 'C:/Users/huangj74/Downloads/tepee2.csv')
    
    
    ##### Romove all data-frame with name beigns with cnt_ #####
    # Get a list of all objects in the global environment
    all_objects <- ls()
    
    # Identify data frames with names starting with "cnt_"
    data_frames_to_remove <- grep("^cnt_", all_objects, value = TRUE)
    
    # Remove identified data frames
    if (length(data_frames_to_remove) > 0) {
      rm(list = data_frames_to_remove)
      cat("Data frames removed:", data_frames_to_remove, "\n")
    } else {
      cat("No data frames found with names starting with 'cnt_'.\n")
    }
    #####Done#####
    

    ###Deal with colors###
    default_color <- colors()
    cols <- reactive({
      lapply(seq_len(length(vnames)), function(i) {
        column(3, colourInput(ns(paste("col", i, sep="_")), vnames[i], default_color[i])   )     
      })
    })
    
    output$color_input <- renderUI({cols()})  
    
    # Put all the input in a vector
    colors2 <- reactive({
      lapply(seq_len(length(vnames)), function(i) {
        input[[paste("col", i, sep="_")]]
      })
    })
    
    updated_color <- unlist(colors2())
    req(updated_color)
    
    default_color <- updated_color
    if (length(default_color) < length(vnames)) {default_color<-colors()}
    ###End of deal with colors###
    
    
    x_min=min(cnt$percent_l)
    x_max=max(cnt$percent_h)
    g <- ggplot(data=cnt, aes(y = group_order))+
      geom_ribbon(aes(y = group_order, xmin = percent_l, xmax = percent_h, fill = as.factor(treatment_order)), alpha = 1) +
      geom_path(aes(x = percent_h, group=as.factor(treatment_order)), linewidth=0.1) +
      geom_path(data=filter(cnt, treatment_order==1), aes(x = percent_l, group=as.factor(treatment_order)), linewidth=0.1) +
      scale_x_continuous(limits = c(x_min-25, x_max), breaks = seq(0, x_max, by = 10)) +
      geom_segment(aes(x = min_pct, xend = max_pct, y = group_order, yend = group_order), linewidth = 0.01) +
      geom_text(data=filter(cnt, treatment_order==1), aes(label = group, y = group_order, x = min_pct-input$label_position), size = input$font_size, hjust = 0) +
      scale_fill_manual(name = '', labels = vnames, values = updated_color) +
      labs(x = "Percentage (%)", y = "") +
      theme_bw() +
      guides(fill=guide_legend(ncol=input$legend_ncol))
    
      if (input$display_pct==1) {
        g <- g +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),        
                legend.position = input$legend_position,
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0),
                plot.subtitle = element_text(hjust = 0),
                plot.caption = element_text(hjust = 0),
                
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank())
      }
      else {
        g <- g +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),        
                legend.position = input$legend_position,
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0),
                plot.subtitle = element_text(hjust = 0),
                plot.caption = element_text(hjust = 0),
                
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank())
      }
    g
  }
  #End of the tepee function
    
    
    observe({
      req(input$tepee)
      rv$df_tepee <- upload_file(input$tepee)
    })
    
    # Reset fileInputs
    observeEvent(input$reset_file, {
      rv$df_tepee <- NULL
      reset("tepee")
    })
    
    
    main <- reactive({
      req(rv$df_tepee)
      process_file(rv$df_tepee)
    })
    
    
    # Update selected rows and/or columns
    observe({
      df <- main()
      cols <- names(df[, 2:ncol(df)])
      rows <- df[[1]]
      updatePickerInput(session, "disp_columns", choices = cols, selected = cols)
      updatePickerInput(session, "disp_rows", choices = rows, selected = rows)
    })
    
    
    tepee_data <- reactive({
      req(main())
      req(input$disp_columns)
      req(input$disp_rows)
      
      indata <- main() %>%
        select(1, all_of(input$disp_columns)) %>%
        filter(.[[1]] %in% input$disp_rows)
    })
    
    
    ##############################################################################################################
    ###Generate Plot Parameter Input Box (X-axis range, step by value) depend on which efficacy data you upload###
    ##############################################################################################################
    tepee_plot <- reactive({
      tepee_data <- tepee_data()
      req(tepee_data)
      
      tepee(indata=tepee_data)
    })
    
    observe({
      output$tepee_plot <- renderPlot({
        tepee_plot()
      }, height=input$height, width=input$width)
    })
    
  })
}