# Correlation (dual y-axis) plot 

# Module UI function
mod_graph_dualy_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "File Upload",
      collapsible = TRUE,
      dropdownMenu = reset_dropdown(ns("reset_file")),
      fluidRow(
        column(6, fileInput(ns("file"), "Choose a file",
                            accept = ".sas7bdat")),
      ),
      fluidRow(
        column(3, selectInput(ns("subj"), "Subject ID", NULL)),
        column(3, selectInput(ns("event"), "Event", NULL)),
        column(3, selectInput(ns("event_date"), "Event Date", NULL)),
        column(3, selectInput(ns("value"), "Value", NULL))
      ),
      fluidRow(
        column(3, selectInput(ns("trt_sdate"), "Treatment Start Date", NULL)),
        column(3, selectInput(ns("trt_edate"), "Treatment End Date", NULL)),
        column(3, selectInput(ns("disc_date"), "Study Discontinuation Date", NULL))
      )
    ),
    shinydashboardPlus::box(
      width = 12,
      title = "Graph",
      dropdownMenu = mod_download_ui(ns("download"), format = "graph"),
      fluidRow(
        column(2, selectInput(ns("event1"), "Event 1", NULL)),
        column(2, selectInput(ns("event2"), "Event 2", NULL)),
        column(2, textInput(ns("cutoff"), "Cutoff (days)")),
        column(3, sliderInput(ns("graph_col"), "Graph columns",
                              min = 1, max = 5, value = 3, step = 1)),
        column(3, sliderInput(ns("graph_height"), "Graph height (each)", 
                              min = 200, max = 600, value = 300, step = 100,
                              post = "px"))
      ),
      uiOutput(ns("plot_ui")) %>% add_spinner()
    )
  )
}

# Module server function
mod_graph_dualy_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Upload file
    observe({
      req(input$file)
      rv$df_upload <- upload_file(input$file)
    })
    
    # Reset fileInput
    observeEvent(input$reset_file, {
      rv$df_upload <- NULL
      shinyjs::reset("file")
    })
    
    # Update variables
    observe({
      choices <- names(rv$df_upload)
      updateSelectInput(session, "subj", choices = choices,
                        selected = grep_var("^pt", choices))
      updateSelectInput(session, "event", choices = choices,
                        selected = grep_var("event", choices))
      updateSelectInput(session, "event_date", choices = choices,
                        selected = grep_var("^dt", choices))
      updateSelectInput(session, "value", choices = choices,
                        selected = grep_var("value", choices))
      updateSelectInput(session, "disc_date", choices = choices,
                        selected = grep_var("adscdt", choices))
      updateSelectInput(session, "trt_sdate", choices = choices,
                        selected = grep_var("lstdt", choices))
      updateSelectInput(session, "trt_edate", choices = choices,
                        selected = grep_var("lspdt", choices))
    })
    
    # All events
    all_events <- reactive({
      unique(rv$df_upload[[input$event]])
    })
    
    # Selectable events for plots
    observe({
      updateSelectInput(session, "event1", choices = all_events(),
                        selected = grep_var("hgb", all_events()))
    })
    observe({
      choices <- all_events()[!(all_events() == input$event1)]
      updateSelectInput(session, "event2", choices = choices)
    })
    
    # Create main data and plots
    dual_y_plot <- reactive({
      df <- rv$df_upload
      req(df)
      event1 <- input$event1
      event2 <- input$event2
      req(event1)
      req(event2)
      req(event1 != event2)
      
      ids <- unique(df[[input$subj]])
      
      dual_plot <- function(n) {
        bsl <- df %>% 
          filter(.data[[input$subj]] == ids[n],
                 .data[[input$event]] == event2)
        bsl_value <- min(bsl[[input$event_date]])
        
        # Assessment before cutoff number of days
        if(input$cutoff != "" & !is.na(as.numeric(input$cutoff))) {
          bsl_value <- bsl[[input$trt_sdate]][1] - as.numeric(input$cutoff)
        }
        
        df2 <- df %>%
          filter(.data[[input$subj]] == ids[n],
                 .data[[input$event]] %in% c(event1, event2)) %>%
          drop_na(all_of(input$value)) %>%
          mutate(exp = (.data[[input$disc_date]] - bsl_value)/30.4375,
                 stdt = (.data[[input$trt_sdate]] - bsl_value)/30.4375,
                 endt = (.data[[input$trt_edate]] - bsl_value)/30.4375,
                 assdt = (.data[[input$event_date]] - bsl_value)/30.4375) %>%
          filter(assdt >= 0)
        
        data1 <- filter(df2, .data[[input$event]] == event1)
        data2 <- filter(df2, .data[[input$event]] == event2)
        
        if(nrow(data2) != 0) {
          coeff <- max(data1[[input$value]])/max(data2[[input$value]])
        } else {
          coeff <- 1
        }
        
        ymin <- floor(min(data2[[input$value]], data1[[input$value]]))
        if(ymin > 0) {
          ymin <- 0
        }
        
        main <- df2 %>%
          mutate(
            Value = case_when(
              .data[[input$event]] == event1 ~ .data[[input$value]], 
              TRUE ~ .data[[input$value]] * coeff
            ),
            Event = factor(.data[[input$event]], levels = c(event1, event2)),
            Date = as.numeric(assdt)
          )
        
        # Colors from ColorBrewer Set1
        ggplot(main, aes(x = Date, y = Value, color = Event)) +
          geom_line() +
          geom_point(size = 0.6) +
          geom_vline(xintercept = data1$stdt) + 
          geom_vline(xintercept = data1$endt, color = "green") +
          scale_color_manual(name = "", values = c("#E41A1C", "#377EB8")) +
          scale_x_continuous(name = "", limits = c(0 , data1$exp)) +
          scale_y_continuous(name = "", 
                             limits = c(ymin - 1, max(data1[[input$value]]) + 5), 
                             sec.axis = sec_axis( ~ . / coeff, name = "")) +
          theme_light() +
          theme(legend.title = element_text(size = 14),
                legend.text = element_text(size = 12)) +
          labs(title = ids[n])
      }
      
      graphs <- vector("list", length = length(ids))
      for(i in seq_along(ids)) {
        graphs[[i]] <- dual_plot(i)
      }
      graphs
    })
    
    # Arrange plots in a grid
    arrange_plots <- function(plots, nrow, fontsize = 16) {
      ggpubr::ggarrange(plotlist = plots, 
                        ncol = input$graph_col,
                        nrow = nrow,
                        legend = "top", common.legend = TRUE) %>%
        ggpubr::annotate_figure(
          bottom = grid::textGrob("Time (months)", 
                                  gp = grid::gpar(fontsize = fontsize)),
          left = grid::textGrob(paste(input$event1, "level"), 
                                gp = grid::gpar(fontsize = fontsize), rot = 90),
          right = grid::textGrob(paste(input$event2, "level"),
                                 gp = grid::gpar(fontsize = fontsize), rot = 270)
        )
    }
    
    # Number of rows in plot grid
    graph_row <- reactive({
      ceiling(length(dual_y_plot())/input$graph_col)
    })
    
    # Render plot
    output$plot <- renderPlot({
      arrange_plots(dual_y_plot(), nrow = graph_row())
    }, res = 96)
    
    # Enable variable heights for plot
    output$plot_ui <- renderUI({
      req(dual_y_plot())
      req(graph_row())
      height <- graph_row() * input$graph_height
      plotOutput(ns("plot"), height = height) %>% add_spinner()
    })
    
    # Download -----------------------------------------------------------------
    # Create report
    report <- reactive({
      plots <- dual_y_plot()
      req(plots)
      
      rpt <- reporter::create_report(orientation = "landscape", font = "Arial") %>%
        reporter::set_margins(0.5, 0.5, 0.5, 0.5)
      
      # Separate plots into pages
      rows_page <- 3 # rows per page
      n_page <- input$graph_col * rows_page # plots per page
      j <- seq(1, length(plots), n_page) # index of first plot on each page
      
      for(i in j) {
        plots_i <- plots[i:(i + n_page - 1)]
        plots_i <- plots_i[!is.na(plots_i)]
        
        # Full height of 7.5 inches results in extra blank pages in RTF
        # So max height per page is capped at 7.2 inches
        height <- ceiling(length(plots_i)/input$graph_col) * 2.4
        
        plot_page <- arrange_plots(plots_i, rows_page, fontsize = 14) %>%
          reporter::create_plot(height = height, width = 10)
        rpt <- rpt %>%
          reporter::add_content(plot_page)
      }
      rpt
    })
    
    # Download plot
    mod_download_server("download", report,
                        filename = "correlation", format = "graph")
    
  })
}
