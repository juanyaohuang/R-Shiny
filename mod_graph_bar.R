mod_graph_bar_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    id,
    shinydashboardPlus::box(
        width = 12,
        title = "File Upload",
        collapsible = TRUE,
        dropdownMenu = reset_dropdown(ns("reset_file")),
        fluidRow(
          column(6, fileInput(ns("bar"), "Choose an input file", 
                              accept = file_accept))
        )
    ),
    shinydashboardPlus::box(width = 12,
        title = "Plot Parameter",
        collapsible = TRUE,
        fluidRow(
          column(3, numericInput(ns('height'), "Plot Height", 400, min=400, max=8000, step=100)),
          column(3, numericInput(ns('bar_width'), "Bar Width", 0.6, min=0.1, max=1, step=0.1)),
          column(3, numericInput(ns('font_size'), "Font Size", 3, min=1, max=5, step=0.25)),
          column(3, numericInput(ns('limit'), "Max Range", 100, min=0, max=110, step=1)),
          column(3, radioButtons(ns("layout"), "Layout of the bar", choices = list("Horizontally" = 1, "Vertically" = 2), selected = 1, inline=T)),
        )
    ),
    shinydashboardPlus::box(width = 12,
                            collapsible = TRUE,
                            title = 'Selected column to be displayed',
                            fluidRow(
                              column(12, uiOutput(ns("select_display1")))
                            ),
                            fluidRow(
                              column(12, uiOutput(ns("select_display2")))
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
        title = "Bar Plot",
        plotOutput(ns("bar_plot"))
    )
  ) 
}



mod_graph_bar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
      
      rv <- reactiveValues(
        df_bar = NULL,
      )
      

      #Function to create Tepee plot
      bar <- function(indata)
      {
        input_data <- indata
        
        Ncol <- dim(input_data)[2] 
        input_data <- mutate(input_data, order=row_number())

        for (i in 2:Ncol) {
          therapy <- data.frame(input_data[,1], input_data[,i], input_data$order)
          names(therapy) <- c('group', 'percent', 'group_order')
          therapy <- mutate(therapy, TERM=names(input_data[,i]), treatment_order=i)
          assign(paste0('cnt_',i), therapy)
        }
        
        cnt <- rbindlist(mget(ls(pattern = "^cnt_")))
        # cnt <- mutate(cnt, pct=paste0(TERM, ': ', percent, '%'))
        cnt <- mutate(cnt, pct=paste0('', percent, ''))
        
        
        funs <- names(input_data[, 2:Ncol])

        # Allow user to select which column (usually is therapy name) to be displayed
        updated_cnt <- reactive({
          df <- subset(cnt, TERM %in% input$checkbox1 & group %in% input$checkbox2)
          df
        })
          
        cnt2 <- updated_cnt()
        req(cnt2)
        
        
        #######################################################################################
        ##################################### Create plot #####################################
        #######################################################################################
        
        # Set Color
        cnt2 <- arrange(cnt2, treatment_order)
        funs2 <- unique(cnt2$TERM)
        
        default_color <- colors()
        cols <- reactive({
          lapply(seq_len(length(funs2)), function(i) {
            column(3, colourInput(ns(paste("col", i, sep="_")), funs2[i], default_color[i])   )     
          })
        })
        
        output$color_input <- renderUI({cols()})  
        
        # Put all the input in a vector
        colors2 <- reactive({
          lapply(seq_len(length(funs2)), function(i) {
            input[[paste("col", i, sep="_")]]
          })
        })
        
        updated_color <- unlist(colors2())
        req(updated_color)
        
        default_color <- updated_color
        if (length(default_color) != length(funs2)) {default_color<-colors()}
        
        
        # Create plot
        if (input$layout==1) {
          ggplot(cnt2, aes(x = percent, y = reorder(group, desc(group_order)), fill = reorder(TERM, treatment_order))) +
            geom_col(position = "dodge", width = input$bar_width) +
            scale_fill_manual(values = updated_color) + 
            geom_text(aes(label = pct), position = position_dodge(width = input$bar_width), hjust = -0.2, vjust = 0.5, size = input$font_size) +
            labs(x = "Percentage (%)", y = "") +
            guides(fill=guide_legend(title="")) +
            scale_x_continuous(limits = c(0, input$limit)) +
            theme(panel.grid.major.x = element_line(color = "white", linetype = "dashed"))
        }
        else {
          ggplot(cnt2, aes(y = percent, x = reorder(group, group_order), fill = reorder(TERM, treatment_order))) +
            geom_col(position = "dodge", width = input$bar_width) +
            scale_fill_manual(values = updated_color) +
            geom_text(aes(label = pct), position = position_dodge(width = input$bar_width), hjust = -0.2, vjust = 0.5, angle = 90, size = input$font_size) +
            labs(y = "Percentage (%)", x = "") +
            guides(fill=guide_legend(title="")) +
            scale_y_continuous(limits = c(0, input$limit)) +
            theme(panel.grid.major.x = element_line(color = "white", linetype = "dashed"), legend.position = "bottom")
        }
      }
      #End of the bar-plot function
      
      
      observe({
        req(input$bar)
        rv$df_bar <- upload_file(input$bar)
      })
      
      # Reset fileInputs
      observeEvent(input$reset_file, {
        rv$df_bar <- NULL
        reset("bar")
      })
      
      
      main1 <- reactive({
        req(rv$df_bar)
        process_file(rv$df_bar)
      })

      observeEvent(input$bar, {
        df <- rv$df_bar
        choices <- names(df[,2:ncol(df)])
        output$select_display1 <- renderUI({
          checkboxGroupInput(ns("checkbox1"), "Select item to display", choices = choices, selected = choices, inline = TRUE)
        })
      })
      observeEvent(input$bar, {
        df <- rv$df_bar
        df <- df[,1]
        names(df) <- c('period')
        choices <- as.vector(unique(df$period))
        output$select_display2 <- renderUI({
          checkboxGroupInput(ns("checkbox2"), "Select group to display", choices = choices, selected = choices, inline = TRUE)
        })
      })
      
      
      ##############################################################################################################
      ###Generate Plot Parameter Input Box (X-axis range, step by value) depend on which efficacy data you upload###
      ##############################################################################################################
      bar_plot <- reactive({
        bar_data <- main1()
        req(bar_data)
        
        bar(indata=bar_data)
      })
      
      observe({
        output$bar_plot <- renderPlot({
          bar_plot()
        }, height=input$height)
      })
      
    })
}