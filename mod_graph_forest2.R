mod_graph_forest2_ui <- function(id) {
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
          column(6, fileInput(ns("forest_efficacy"), "Choose an ADRS/ADTTE", 
                              accept = file_accept))
        ),
        fluidRow(
          column(3, selectInput(ns("forest_trt"), "Treatment", NULL)),
          column(3, pickerInput_vars(ns("forest_subgrp"), "Subgroup"))
        )
    ),
    shinydashboardPlus::box(width = 9,
        title = "Plot Parameter",
        collapsible = TRUE,
        dropdownMenu = reset_dropdown("forest"),
        fluidRow(
          column(3, uiOutput(ns("x_lower"))),
          column(3, uiOutput(ns("x_upper"))),
          column(3, uiOutput(ns("Step"))),
          column(3, numericInput(ns('height'), "Plot Height", 400, min=400, max=2000))
        )
    ),
    box(width = 3,
        title = "Filters",
        fluidRow(column(12, pickerInput_vars(ns("forest_adsl_filter"), "Filter ADSL"))),
        fluidRow(uiOutput(ns("forest_adsl_filter_ui"))),
        fluidRow(column(12, pickerInput_vars(ns("forest_eff_filter"), "Filter Efficacy Data"))),
        fluidRow(uiOutput(ns("forest_eff_filter_ui")))
    ),
    shinydashboardPlus::box(width = 12,
        title = 'Input Your Own Labels',
        fluidRow(
          column(12, uiOutput(ns("label_inputs"))),
          column(3, actionButton(ns("submit_labels"), "Submit Labels"))
        )
    ),
    shinydashboardPlus::box(width = 12,
        title = "Forest Plot",
        plotOutput(ns("forest_plot"))
    )
  ) 
}



mod_graph_forest2_server <- function(id) {
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
        df_forest_efficacy = NULL,
      )
      
      #This is the function to generate the data that is used to draw the plot.
      #This function is for each individual subgroup variable selected.
      #All data of each each individual subgroup will be combined later.
      g<-function(var){
        adsl <- mutate(df_forest_adsl(), CATEGORY=.data[[var]])
        adsl <- mutate(adsl, CATEGORY=ifelse(is.na(CATEGORY), 'Missing', CATEGORY))
        efficacy <- df_forest_efficacy()
        efficacy <- mutate(efficacy, TRTGRP=.data[[input$forest_trt]])
        if ("CNSR" %in% colnames(efficacy)){  
          efficacy <- merge(select(efficacy, SUBJID, PARAM, CNSR, AVAL, TRTGRP), adsl, by="SUBJID")   
        }
        else{efficacy <- merge(select(efficacy, SUBJID, PARAM, AVALC, AVAL, TRTGRP), adsl, by="SUBJID") # Merge efficacy data and adsl datasets by SUBJID
        efficacy$ORR <- ifelse(efficacy$AVALC %in% c("CR", "PR") | efficacy$AVAL==1, 1, 2) # Create ORR variable based on avalc values
        }  
        
        ORR <- function(data){
          trt<-as.data.frame(table(data$ORR, data$TRTGRP))
          
          n1<-trt$Freq[1]
          N1=trt$Freq[1]+trt$Freq[2]
          n2<-trt$Freq[3]
          N2<-trt$Freq[3]+trt$Freq[4]
          
          if (is.na(N2)==FALSE){
            test <- moverci(n1, N1, n2, N2, type="wilson")
            diff <- round(test[2]*100, 1)
            difflcl <- round(test[1]*100, 1)
            diffucl <- round(test[3]*100, 1)
            
            freq <- binom.test(n1, N1, conf.level=0.95) # Perform binomial test to calculate ORR and confidence interval
            rr1 <- round(freq$estimate*100, 1)
            rrLowerCL1 <- round(freq$conf.int[1]*100, 1)
            rrUpperCL1 <- round(freq$conf.int[2]*100, 1)
            
            freq <- binom.test(n2, N2, conf.level=0.95) # Perform binomial test to calculate ORR and confidence interval
            rr2 <- round(freq$estimate*100, 1)
            rrLowerCL2 <- round(freq$conf.int[1]*100, 1)
            rrUpperCL2 <- round(freq$conf.int[2]*100, 1)
            
            result <- data.frame(col1=paste0(N1+N2),
                                 col2=paste0(n1, "(", N1, ")"),
                                 col3=paste0(rr1, " (", rrLowerCL1, '  ', rrUpperCL1, ')'),
                                 col4=paste0(n2, "(", N2, ")"),
                                 col5=paste0(rr2, " (", rrLowerCL2, '  ', rrUpperCL2, ')'),                       
                                 col6=paste0(diff, " (", difflcl, '  ', diffucl, ')'),
                                 diff=diff,
                                 difflcl=difflcl,
                                 diffucl=diffucl,
                                 treatment1=trt$Var2[1],
                                 treatment2=trt$Var2[3]
            )
          }
          else{result <- data.frame(col1=0,
                                    col2='',
                                    col3='',
                                    col4='',
                                    col5='',                       
                                    col6='',
                                    diff=NA,
                                    difflcl=NA,
                                    diffucl=NA,
                                    treatment1='',
                                    treatment2=''
          )
          }
          return(result)
        }
        
        
        KM <- function(inputdata){
          N <- nrow(inputdata)
          fit <- survfit(Surv(AVAL, 1-CNSR) ~ TRTGRP , data = inputdata, conf.type='log-log')
          sumfit<-summary(fit)
          df_sumfit <- as.data.frame(sumfit$table)
          
          if (length(unique(inputdata$TRTGRP))>=2){
            for(i in 1:2) {
              assign(paste0("n", i), df_sumfit[i,1]) #total patients
              assign(paste0("e", i), df_sumfit[i,4]) #event number
            }
            
            for(i in 1:2) {
              assign(paste0('m_',   i), round(df_sumfit[i,7], 1))  #median
              assign(paste0('lcl_', i), round(df_sumfit[i,8], 1))  #0.95LCL
              assign(paste0('ucl_', i), round(df_sumfit[i,9], 1)) #0.95UCL
            }
            
            fit <- coxph(Surv(AVAL, 1-CNSR) ~ TRTGRP, data = inputdata, 
                         ties = "efron", 
                         x = TRUE, 
                         y = TRUE,
                         model = TRUE)
            hr0<-summary(fit)$conf.int[1]
            hrlcl0<-summary(fit)$conf.int[3]
            hrucl0<-summary(fit)$conf.int[4]
            hr <- round(1/hr0, 2)
            hrlcl <- round(1/hrucl0, 2)
            hrucl <- round(1/hrlcl0, 2)
            
            inputdata <- arrange(inputdata, TRTGRP)
            a <- unique(efficacy$TRTGRP)
            
            result <- data.frame(col1=paste0(N),
                                 col2=paste0(e1, "(", n1, ")"),
                                 col3=paste0(m_1, " (", lcl_1, '  ', ucl_1, ')'),
                                 col4=paste0(e2, "(", n2, ")"),
                                 col5=paste0(m_2, " (", lcl_2, '  ', ucl_2, ')'),                       
                                 col6=paste0(hr, " (", hrlcl, '  ', hrucl, ')'),
                                 diff=hr,
                                 difflcl=hrlcl,
                                 diffucl=hrucl,
                                 treatment1=a[1],
                                 treatment2=a[2]
            )
          }
          else {
            result <- data.frame(col1=0,
                                 col2='',
                                 col3='',
                                 col4='',
                                 col5='',                       
                                 col6='',
                                 diff=NA,
                                 difflcl=NA,
                                 diffucl=NA,
                                 treatment1='',
                                 treatment2=''
            )
          }
        }
        # Using 'by()' to apply custom function to each group
        if ("CNSR" %in% colnames(efficacy)){ result_by <- by(efficacy, efficacy$CATEGORY, KM) }
        else{ result_by <- by(efficacy, efficacy$CATEGORY, ORR) }
        
        
        # Convert the output of 'by()' to a data frame
        result_df <- do.call(rbind, as.list(result_by))
        
        value <- efficacy %>%
          arrange(CATEGORY) %>%
          distinct(CATEGORY, .keep_all = F)
        result_df$label <- value$CATEGORY
        result_df <- mutate(result_df, label=paste0('      ', label))
        result_df <- subset(result_df, col1>0) #If a subgroup population contains only 1 treatment group, delete it
        
        if (nrow(result_df)==0){
          result_df <- data.frame(col1=NA,
                                  col2='',
                                  col3='',
                                  col4='There is only one treatment group value within each level of this subgroup variable, please drop and reselect.',
                                  col5='',                       
                                  col6='',
                                  diff=NA,
                                  difflcl=NA,
                                  diffucl=NA,
                                  treatment1='',
                                  treatment2='',
                                  label=''
          )
        }
        
        return(result_df)
      }
      #End of the g function
      
      
      # Forest plot -------------------------------------------------------------
      observe({
        req(input$forest_adsl)
        rv$df_forest_adsl <- upload_file(input$forest_adsl)
        req(input$forest_efficacy)
        rv$df_forest_efficacy <- upload_file(input$forest_efficacy)
      })
      
      # Create forest plot
      # Reset fileInputs
      observeEvent(input$forest_reset, {
        rv$df_forest_adsl <- NULL
        reset("forest_adsl")
        rv$df_forest_efficacy <- NULL
        reset("forest_efficacy")
      })
      
      observe({
        choices <- names(rv$df_forest_adsl)
        updatePickerInput(session, "forest_subgrp", choices = choices, selected = grep_var("sex", choices))
        updatePickerInput(session, "forest_adsl_filter", choices = choices)
      })
      
      observe({
        choices <- names(rv$df_forest_efficacy)
        updatePickerInput(session, "forest_eff_filter", choices = choices)
        updateSelectInput(session, "forest_trt", choices = choices, selected = grep_var("^trt", choices))
      })
      
      
      main1 <- reactive({
        req(rv$df_forest_adsl)
        process_file(rv$df_forest_adsl)
      })
      
      main2 <- reactive({
        req(rv$df_forest_efficacy)
        process_file(rv$df_forest_efficacy)
      })
      
      # Display filter variable selections
      output$forest_adsl_filter_ui <- renderUI({
        req(input$forest_adsl_filter)
        n <- length(input$forest_adsl_filter)
        
        lapply(1:n, function(i) {
          column(12, forest_adsl_filter()[[i]])
        })
      })
      
      # Display filter variable selections
      output$forest_eff_filter_ui <- renderUI({
        req(input$forest_eff_filter)
        n <- length(input$forest_eff_filter)
        
        lapply(1:n, function(i) {
          column(12, forest_eff_filter()[[i]])
        })
      })
      
      
      # Filter value select inputs
      forest_adsl_filter <- eventReactive(input$forest_adsl_filter, {
        params <- input$forest_adsl_filter
        n <- length(params)
        df <- main1()
        input_tagList <- vector("list", n)
        for(i in 1:n) {
          param <- params[i]
          new_input_id <- paste0("val_", param)
          choices <- sort(unique(df[[param]]))
          nonnumeric <- nonnumeric(choices)
          
          # Prevent reset of values
          new_input_value <- choices
          
          # Date variables
          if(is.Date(choices) | is.POSIXt(choices)) {
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
            # Character variables
          } else if(any(nonnumeric) | length(choices) <= 3) {
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
            new_input <- numericRangeInput(ns(new_input_id), param,
                                           value = c(new_input_value[1],
                                                     new_input_value[2]),
                                           min = min(choices), max = max(choices)) %>%
              suppressWarnings()
          }
          input_tagList[[i]] <- new_input
        }
        input_tagList
      })
      
      
      forest_eff_filter <- eventReactive(input$forest_eff_filter, {
        params <- input$forest_eff_filter
        n <- length(params)
        df <- main2()
        input_tagList <- vector("list", n)
        for(i in 1:n) {
          param <- params[i]
          new_input_id <- paste0("val_", param)
          choices <- sort(unique(df[[param]]))
          nonnumeric <- nonnumeric(choices)
          
          # Prevent reset of values
          new_input_value <- choices
          
          # Date variables
          if(is.Date(choices) | is.POSIXt(choices)) {
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
            # Character variables
          } else if(any(nonnumeric) | length(choices) <= 3) {
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
            new_input <- numericRangeInput(ns(new_input_id), param,
                                           value = c(new_input_value[1],
                                                     new_input_value[2]),
                                           min = min(choices), max = max(choices)) %>%
              suppressWarnings()
          }
          input_tagList[[i]] <- new_input
        }
        input_tagList
      })
      
      
      df_forest_adsl <- reactive({
        df <- main1()
        req(df)
        
        # Account for all filter variables selected
        params <- input$forest_adsl_filter
        if(!is.null(params)) {
          n <- length(params)
          df_final <- df
          for(i in 1:n) {
            param <- params[i]
            val <- input[[paste0("val_", param)]]
            df_i <- df
            
            # Date/numeric vs character values
            if(is.Date(val) | is.POSIXt(val) | is.numeric(val)) {
              df_i <- dplyr::filter(df_i, between(.data[[param]],
                                                  val[1], val[2])) %>%
                suppressWarnings()
            } else {
              df_i <- dplyr::filter(df_i, .data[[param]] %in% val)
            }
            df_final <- intersect(df_final, df_i)
          }
          df <- df_final
        }
        # Replace NA with "Unknown" (categorical only)
        df <- df %>%
          mutate(across(all_of(input$forest_subgrp), 
                        ~ifelse(is.na(.) , "Missing", .)))
      })
      
      
      df_forest_efficacy <- reactive({
        df <- main2()
        req(df)
        
        # Account for all filter variables selected
        params <- input$forest_eff_filter
        if(!is.null(params)) {
          n <- length(params)
          df_final <- df
          for(i in 1:n) {
            param <- params[i]
            val <- input[[paste0("val_", param)]]
            df_i <- df
            
            # Date/numeric vs character values
            if(is.Date(val) | is.POSIXt(val) | is.numeric(val)) {
              df_i <- dplyr::filter(df_i, between(.data[[param]],
                                                  val[1], val[2])) %>%
                suppressWarnings()
            } else {
              df_i <- dplyr::filter(df_i, .data[[param]] %in% val)
            }
            df_final <- intersect(df_final, df_i)
          }
          df <- df_final
        }
        df <- df
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
          column(3, textInput(ns(paste0("label_", i)), label = var_name, value = label_value))
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
            input_value <- input[[input_id]]
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
        df_forest_efficacy <- df_forest_efficacy()
        req(df_forest_efficacy)
        
        var_n <- length(input$forest_subgrp)
        for (i in 1:var_n){
          var_i <- input$forest_subgrp[i]
          g_data <- g(var_i)
          g_data$rowname <- var_i
          assign(paste0('g_data_', i), g_data)
        }
        final_tbl <- rbindlist(mget(ls(pattern = "^g_data_")))
      })
      
      
      ##############################################################################################################
      ###Generate Plot Parameter Input Box (X-axis range, step by value) depend on which efficacy data you upload###
      ##############################################################################################################
      output$x_lower <- renderUI({
        efficacy <- df_forest_efficacy()
        req(efficacy)
        if ("CNSR" %in% colnames(efficacy)){ numericInput(ns('x_lower2'), "X-Axis Start", 0,   min=0,    max=100) }
        else{ numericInput(ns('x_lower2'), "X-Axis Start",-100, min=-100, max=0) }
      })
      output$x_upper <- renderUI({
        efficacy <- df_forest_efficacy()
        req(efficacy)
        if ("CNSR" %in% colnames(efficacy)){ numericInput(ns('x_upper2'), "X-Axis End", 2,   min=0,    max=100) }
        else{ numericInput(ns('x_upper2'), "X-Axis End", 100, min=0, max=100) }
      })
      output$Step <- renderUI({
        efficacy <- df_forest_efficacy()
        req(efficacy)
        if ("CNSR" %in% colnames(efficacy)){ numericInput(ns('Step2'), "X-Axis Step", 0.2, min=0.05, max=20) }
        else{ numericInput(ns('Step2'), "X-Axis Step", 20, min=5, max=20) }
      })
      ##Done##
      
      
      forest_plot <- reactive({
        efficacy <- df_forest_efficacy()
        req(efficacy)
        
        req(ggplot_data)
        result_df <- ggplot_data()
        result_df <- mutate(result_df, order=row_number())
        result_df <- mutate(result_df, order=order+1)
        
        result_df <- mutate(result_df, diffucl=ifelse(diffucl==Inf, diff, diffucl))
        result_df <- mutate(result_df, difflcl=ifelse(diffucl==Inf, diff, difflcl))
        max <- max(subset(result_df, !is.na(diffucl))$diffucl)
        min <- min(subset(result_df, !is.na(difflcl))$difflcl)
        
        
        req(generated_dataframe)
        df <- generated_dataframe() #Contains user-defined label

        req(data)
        label_lookup_map <- data() #Contains labels from ADSL
        
        title <- result_df %>%
          mutate(label=rowname) %>%
          arrange(label, order) %>%
          group_by(label) %>%
          filter(row_number()==1) %>%
          ungroup()
        title <- mutate(title, rowname=NA, col1=NA, col2=NA, col3=NA, col4=NA, col5=NA, col6=NA, diff=NA, difflcl=NA, diffucl=NA, treatment1='', treatment2='', order=order-0.5)
        title <- rename(title, col_name=label)
        if (is_empty(df)==TRUE){title <-  merge(title, label_lookup_map, by='col_name')}
        else{title <-  merge(title, df, by='col_name')}
        title <- select(title, -c('col_name'))
        
        
        result_df <- rbind(title, result_df) 
        result_df <- result_df %>%
          arrange(order) %>%
          mutate(order=row_number()) %>%
          mutate(order=order+1)
        result_df$low <-  result_df$order-0.01*(400/input$height)*dim(result_df)[1]
        result_df$high <- result_df$order+0.01*(400/input$height)*dim(result_df)[1]
        
        result_df <- mutate(result_df, low=ifelse(diffucl==diff, order, low))
        result_df <- mutate(result_df, high=ifelse(diffucl==diff, order, high))
        result_df <- mutate(result_df, col6=str_replace(col6, 'Inf','NA'))
        
        x1 <- input$x_lower2
        x2 <- input$x_upper2    
        by <- input$Step2
        if (x2<max){x2=max*1.01}
        if (x1>min){x1=min*0.99}
        if ((x2-x1)/by>40){by=round((x2-x1)/40, 1)}
        
        trt1 <- unique(subset(result_df, !is.na(treatment1) & treatment1!='')$treatment1)
        trt2 <- unique(subset(result_df, !is.na(treatment2) & treatment2!='')$treatment2)

        
        if ("CNSR" %in% colnames(efficacy)){
          ggplot(result_df, aes(y = order)) +
            geom_segment(aes(x = difflcl, xend = diffucl, y = order, yend = order), stat = "identity", position = position_dodge(width = 0),  linewidth = 0.3, alpha = 1) +
            geom_point(aes(x = diff, y = order), shape = 21, fill = "black", size = 3) +
            geom_segment(aes(x = difflcl, xend = difflcl, y = low, yend = high), stat = "identity", position = position_dodge(width = 0), linewidth = 0.3, alpha = 1) + 
            geom_segment(aes(x = diffucl, xend = diffucl, y = low, yend = high), stat = "identity", position = position_dodge(width = 0), linewidth = 0.3, alpha = 1) +
            geom_vline(xintercept = 1, linetype = "dashed") +
            
            annotate("text", x = -5.75*(x2-x1)/2, y = 1, label = "Sub-groups:", hjust = 0, size = 3.0, fontface = "bold") +
            geom_text(aes(label = label, y=order, x=-5.75*(x2-x1)/2), size = 3.0, hjust=0) +
            
            annotate("text", x = -4.00*(x2-x1)/2, y = 1, label = "N", hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col1, y=order, x=-4.00*(x2-x1)/2), size = 3.0) +
            
            
            annotate("text", x = -3.50*(x2-x1)/2, y = 1, label = paste0(trt1,'\n',"Events\n(Subjects)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col2, y=order, x=-3.50*(x2-x1)/2), size = 3.0) +
            annotate("text", x = -2.75*(x2-x1)/2, y = 1, label = paste0(trt1,'\n',"Median\n(95% Exact CI)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col3, y=order, x=-2.75*(x2-x1)/2), size = 3.0) +
            
            annotate("text", x = -2.00*(x2-x1)/2, y = 1, label = paste0(trt2,'\n',"Events\n(Subjects)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col4, y=order, x=-2.00*(x2-x1)/2), size = 3.0) +
            annotate("text", x = -1.25*(x2-x1)/2, y = 1, label = paste0(trt2,'\n',"Median\n(95% Exact CI)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col5, y=order, x=-1.25*(x2-x1)/2), size = 3.0) +
            
            annotate("text", x = -0.5*(x2-x1)/2, y = 1, label = paste0("Unstratified\nHazard Ratio (95% CI)\n", trt1, " vs ", trt2), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col6, y=order, x=-0.5*(x2-x1)/2), size = 3.0) +
            
            scale_x_continuous(
              limits = c(-5.75*(x2-x1)/2, x2), 
              breaks = seq(x1, x2, by = by)) +
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
        }
        else{
          ggplot(result_df, aes(y = order)) +
            geom_segment(aes(x = difflcl, xend = diffucl, y = order, yend = order), stat = "identity", position = position_dodge(width = 0),  linewidth = 0.3, alpha = 1) +
            geom_point(aes(x = diff, y = order), shape = 21, fill = "black", size = 3) +
            geom_segment(aes(x = difflcl, xend = difflcl, y = low, yend = high), stat = "identity", position = position_dodge(width = 0), linewidth = 0.3, alpha = 1) + 
            geom_segment(aes(x = diffucl, xend = diffucl, y = low, yend = high), stat = "identity", position = position_dodge(width = 0), linewidth = 0.3, alpha = 1) +
            geom_vline(xintercept = 0, linetype = "dashed") +
            
            annotate("text", x = -650*(x2-x1)/200, y = 1, label = "Sub-groups:", hjust = 0, size = 3.0, fontface = "bold") +
            geom_text(aes(label = label, y=order, x=-650*(x2-x1)/200), size = 3.0, hjust=0) +
            
            annotate("text", x = -475*(x2-x1)/200, y = 1, label = "N", hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col1, y=order, x=-475*(x2-x1)/200), size = 3.0) +
            
            
            annotate("text", x = -425*(x2-x1)/200, y = 1, label = paste0(trt1,'\n',"Responders\n(Subjects)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col2, y=order, x=-425*(x2-x1)/200), size = 3.0) +
            annotate("text", x = -350*(x2-x1)/200, y = 1, label = paste0(trt1,'\n',"ORR(%)\n(95% Exact CI)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col3, y=order, x=-350*(x2-x1)/200), size = 3.0) +
            
            annotate("text", x = -275*(x2-x1)/200, y = 1, label = paste0(trt2,'\n',"Responders\n(Subjects)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col4, y=order, x=-275*(x2-x1)/200), size = 3.0) +
            annotate("text", x = -200*(x2-x1)/200, y = 1, label = paste0(trt2,'\n',"ORR(%)\n(95% Exact CI)"), hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col5, y=order, x=-200*(x2-x1)/200), size = 3.0) +
            
            
            annotate("text", x = -125*(x2-x1)/200, y = 1, label = "Unweighted\nORR(%)\nDifference (95% CI)", hjust = 0.5, size = 3.0, fontface = "bold") +
            geom_text(aes(label = col6, y=order, x=-125*(x2-x1)/200), size = 3.0) +
            
            scale_x_continuous(
              limits = c(-650*(x2-x1)/200, x2), 
              breaks = seq(x1, x2, by = by)) +
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
        } #End of else
      })
      
      observe({
        output$forest_plot <- renderPlot({
          forest_plot()
        }, height=input$height)
      })
      
    })
}