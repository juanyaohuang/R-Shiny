# Demographic table

# Module UI function
mod_table_demo_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "File Upload",
      collapsible = TRUE,
      dropdownMenu = reset_dropdown(ns("reset_file")),
      fluidRow(
        column(6, fileInput(ns("file"), "Choose an ADSL file", 
                            accept = file_accept)),
        column(6, pickerInput_vars(ns("sum_vars"), "Summarize Variables"))
      ),
      fluidRow(
        column(3, selectInput(ns("trt"), "Treatment", NULL)),
        column(3, selectInput(ns("trtn"), "Treatment (N) (optional)", "None",
                              selected = "None")),
        div(column(3, awesomeCheckbox(ns("show_total"), "Show Total column")),
            style = "padding-top:32px")
      )
    ),
    
    tags$head(
      tags$script(HTML('
        Shiny.addCustomMessageHandler("handleClick", function(adsl) {
         var a=document.querySelector("#de_demo-dt_table");
         var b=a.querySelectorAll("*");
         //var c=b.querySelector(".dataTables_scroll");
         
          console.log(a)
          console.log(b)
          console.log("this is a test")
          
        })
      '))
    ),
    
    
    table_option_box(ns, mod_sub_tableoption_ui(ns("options"))),
    shinydashboardPlus::box(
      width = 12,
      title = "Demographic Table",
      dropdownMenu = mod_download_ui(ns("download"), format = "table"),
      DT::dataTableOutput(ns("dt_table")) %>% add_spinner()
    ),
    mod_sub_varfilter_ui(ns("filter")),
    shinydashboardPlus::box(
      width = 9,
      title = "Subject Level Table",
      DT::dataTableOutput(ns("sub_table"))
    ),
  )
}

# Module server function
mod_table_demo_server <- function(id) {
  
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
      trt_choices <- grep("^trt01", choices, TRUE, value = TRUE)
      var_select <- c(grep_var("age", choices), grep_var("sex", choices), 
                      grep_var("eth", choices), grep_var("race", choices))
      choice_labels <- var_labels(rv$df_adsl)
      
      updateSelectInput(session, "trt", choices = trt_choices)
      updateSelectInput(session, "trtn", choices = c("None", trt_choices),
                        selected = ifelse_var(".*n$", trt_choices))
      updatePickerInput(session, "sum_vars", choices = choices,
                        selected = var_select,
                        choicesOpt = list(subtext = choice_labels))
    })
    
    # Main data
    main <- reactive({
      req(rv$df_adsl)
      process_file(rv$df_adsl)
    })
    
    # Apply variable filters
    df_filter <- mod_sub_varfilter_server("filter", main)
    
    # Prepare data frame
    demo_df <- reactive({
      req(input$trt %in% names(df_filter()))
      df <- df_filter() %>%
        drop_na(input$trt)
      
      # Order treatments
      df <- order_trt(df, input$trt, input$trtn)
      
      # If box is checked, add total column
      if(input$show_total) {
        total <- df %>%
          mutate(trt = "Total")
        df <- rbind(df, total)
      }
      
      # Replace NA with "Unknown" (categorical only) and add N to each treatment group
      df %>%
        select(trt, input$sum_vars) %>%
        mutate(across(all_of(input$sum_vars), 
                      ~ifelse(is.na(.) & !is.numeric(.), "Unknown", .))) %>%
        group_by(trt) %>%
        mutate(N = n()) %>%
        ungroup()
    })
    
    # Big N for each treatment
    demo_N <- reactive({
      demo_df() %>%
        select(trt, N) %>%
        distinct() %>%
        arrange(trt)
    })
    
    # Demographic table --------------------------------------------------------
    demo_table_summarize <- reactive({
      df <- demo_df()
      req(df)
      char_vars <- df %>%
        select(-trt, -where(is.numeric)) %>%
        names()
      num_vars <- df %>%
        select(-trt, -all_of(char_vars), -N) %>%
        names()
      
      # Summarize character and numeric variables
      char_tbls <- purrr::map(char_vars, ~charfreq_demo(df, "trt", .))
      names(char_tbls) <- char_vars
      num_tbls <- purrr::map(num_vars, ~summarize_stats_demo(df, "trt", .,
                                                             stat_keep = input$table_stats))
      names(num_tbls) <- num_vars
      
      c(char_tbls, num_tbls)
    })
    
    # Apply additional options and combine tables
    demo_table_option <- mod_sub_tableoption_server("options", demo_table_summarize, 
                                                    reactive(input$sum_vars))
    
    # Final table
    demo_table <- reactive({
      tbl <- demo_table_option()
      
      # Add N
      trt_N <- paste0(demo_N()$trt, "\n(N=", demo_N()$N, ")")
      names(tbl) <- c("Variable", trt_N)

      test <- mutate(tbl, ind=ifelse(tbl[, ncol(tbl)]=='', 1, 0))
      test$ind2 <- ave(test$ind,
                       FUN = cumsum)
      name_row <- rename(select(subset(test, ind==1), Variable, ind2), label=Variable)
      test <- select(merge(test, name_row, by='ind2'), -c('ind', 'ind2'))

      ncol <- ncol(test)-1
      nrow <- nrow(test)
      
      treatment <- demo_N()$trt
      
      for(i in 1:nrow){
        for (j in 2:ncol){
          if (grepl('Min', test$Variable[i])==TRUE){
            Min=strsplit(test[i,j], ", ")
            Min <- Min[[1]][1]
            Max=strsplit(test[i,j], ", ")
            Max <- Max[[1]][2]
            cmin=as.character(actionLink(ns(paste0("link_", i, j, '_min')), as.character(Min), class=test$label[i]))
            cmax=as.character(actionLink(ns(paste0("link_", i, j, '_max')), as.character(Max), class=test$label[i]))
            test[i,j] <- paste0(cmin, ', ', cmax)
          }
        }
      }
      #tbl <- select(test, -c('X'))
      test <- mutate(test, Variable=ifelse(Variable==trimws(Variable), Variable, paste0('&nbsp &nbsp &nbsp', Variable)))
      tbl <- test
      write.csv(tbl, 'C:/Users/huangj74/Downloads/demo.csv')
      tbl
    })
    
    

    # Process the click on Min or Max number
    subject_data <- reactiveVal(NULL)
    
    setupLinkHandlers <- function(df, adsl) {
      nrow <- nrow(df)
      showNotification(nrow, duration=1)
      ncol <- ncol(df)
      showNotification(ncol, duration=1)
      

      lapply(seq_len(nrow), function(i) {
        lapply(seq_len(ncol), function(j) {
          removeEvent(event='click', id=paste0("link_", i, j, '_min'))
          removeEvent(event='click', id=paste0("link_", i, j, '_max'))
          
          onevent('click', paste0('link_', i, j, '_min'), function() {
            varname <- df[i, ncol]
            a=paste0('link_', i, j, '_min')
            showNotification(varname, duration=1)
            showNotification(a, duration=1)
            
            adsl <- adsl %>%
              mutate(TRTGRP=.data[[input$trt]], TRTGRPN=.data[[input$trtn]], var=.data[[varname]]) %>%
              subset(TRTGRP!='' & !is.na(TRTGRP)) %>%
              arrange(TRTGRPN)
            
            treatment <- unique(adsl$TRTGRP)
            
            adsl <- adsl %>%
              select(USUBJID, var, TRTGRP) %>%
              subset(TRTGRP==treatment[j-1]) %>%
              subset(!is.na(var)) %>%
              subset(var==min(var))
            
            names(adsl) <- c('USUBJID', varname, 'TRTGRP') 
            subject_data(adsl)
          }, add=FALSE)
          
          onevent('click', paste0('link_', i, j, '_max'), function() {
            varname <- df[i, ncol]
            a=paste0('link_', i, j, '_max')
            showNotification(varname, duration=1)
            showNotification(a, duration=1)
            
            adsl <- adsl %>%
              mutate(TRTGRP=.data[[input$trt]], TRTGRPN=.data[[input$trtn]], var=.data[[varname]]) %>%
              subset(TRTGRP!='' & !is.na(TRTGRP)) %>%
              arrange(TRTGRPN)
            
            treatment <- unique(adsl$TRTGRP)
            
            adsl <- adsl %>%
              select(USUBJID, var, TRTGRP) %>%
              subset(TRTGRP==treatment[j-1]) %>%
              subset(!is.na(var)) %>%
              subset(var==max(var))
            
            names(adsl) <- c('USUBJID', varname, 'TRTGRP') 
            subject_data(adsl)
          }, add=FALSE)
        })
      })
    }
    
    
    # Whenever the table is updated, call this function again to set up event handlers
    observe({
      setupLinkHandlers(demo_table(), df_filter())
    })
    
    
    
    
    
    output$outputTable <- renderTable({
      subject_data()
    })
    
    
    #Drop the last column from demo_table()
    demo_table2 <- reactive({
      df<-demo_table()
      req(df)
      df<-select(df,-c('label'))
      df
    })  
    

    # Demographic gt table
    output$dt_table <- renderDataTable({
      DT::datatable(
        demo_table2(), escape = FALSE, rownames = FALSE,
        options = list(pageLength = 100,
                       columnDefs = list(
                         list(width = '300px', targets = 0)
                       ),
                       preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                       drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
        )
      )
    })
    
    
    output$sub_table <- renderDataTable({
      DT::datatable(
        subject_data()
      )
    })
    
    observe({
        val <- 1
        session$sendCustomMessage("handleClick", list(adsl=val))
    })
    
    
  })
}
