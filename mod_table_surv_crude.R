mod_table_surv_crude_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "File Upload",
      collapsible = TRUE,
      dropdownMenu = reset_dropdown(ns("reset_file")),
      fluidRow(
        column(6, fileInput(ns("file_adsl"), "Choose an ADSL file",
                            accept = file_accept)),
        column(6, fileInput(ns("file_adtte"), "Choose an ADTTE file",
                            accept = file_accept))
      ),
      # fluidRow(
      #   column(3, selectInput(ns("subj"), "Subject ID", NULL)),
      #   column(3, selectInput(ns("study"), "Study ID", NULL)),
      #   column(3, selectInput(ns("param"), "Parameter", NULL)),
      #   column(3, selectInput(ns("aval"), "Analysis Value", NULL))
      # ),
      # fluidRow(
      #   column(3, selectInput(ns("trt"), "Treatment", NULL)),
      #   column(3, selectInput(ns("trtn"), "Treatment (N) (optional)", "None",
      #                         selected = "None")),
      #   column(3, selectInput(ns("censor"), "Censor", NULL)),
      #   column(3, selectInput(ns("eventdesc"), "Censoring Description", NULL))
      # ),
      fluidRow(
        column(3, selectInput(ns("param"), "Parameter", NULL)),
        column(6, selectInput(ns("param_sel"), "Parameter Select", NULL))
      )
    ),
    mod_sub_varfilter_ui(ns("filter")),
    shinydashboardPlus::box(
      width = 12,
      title = "Survival Table",
      dropdownMenu = mod_download_ui(ns("download"), format = "table"),
      gt::gt_output(ns("table")) %>% add_spinner()
    )
  )
}

# Module server function
mod_table_surv_crude_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues()
    
    # Upload files
    observe({
      req(input$file_adsl)
      rv$df_adsl <- upload_file(input$file_adsl)
    })
    observe({
      req(input$file_adtte)
      rv$df_adtte <- upload_file(input$file_adtte)
    })
    
    # Reset fileInputs
    observeEvent(input$reset_file, {
      rv$df_adsl <- NULL
      rv$df_adtte <- NULL
      shinyjs::reset("file_adsl")
      shinyjs::reset("file_adtte")
    })
    
    # Update variables
    observe({
      # choices_adsl <- names(rv$df_adsl)
      choices_adtte <- names(rv$df_adtte)
      # choices_common <- intersect(choices_adsl, choices_adtte)
      # choices_all <- unique(c(choices_adsl, choices_adtte))
      # trt_choices <- grep("^trt01", choices_all, TRUE, value = TRUE)
      
      # updateSelectInput(session, "subj", choices = choices_common,
      #                   selected = grep_var("subj", choices_common))
      # updateSelectInput(session, "study", choices = choices_adtte,
      #                   selected = grep_var("studyid", choices_adtte))
      updateSelectInput(session, "param", choices = choices_adtte,
                        selected = grep_var("param$", choices_adtte))
      # updateSelectInput(session, "aval", choices = choices_adtte,
      #                   selected = grep_var("aval", choices_adtte))
      # updateSelectInput(session, "trt", choices = trt_choices,
      #                   selected = grep_var("trt01p$", trt_choices))
      # updateSelectInput(session, "trtn", choices = c("None", trt_choices),
      #                   selected = ifelse_var("trt01pn", trt_choices))
      # updateSelectInput(session, "censor", choices = choices_adtte,
      #                   selected = grep_var("cnsr", choices_adtte))
      # updateSelectInput(session, "eventdesc", choices = choices_adtte,
      #                   selected = grep_var("evntdesc", choices_adtte))
    })
    observe({
      choices <- unique(rv$df_adtte[[input$param]])
      updateSelectInput(session, "param_sel", choices = choices)
    })
    
    # Main data
    adsl <- reactive({
      req(rv$df_adsl)
      process_file(rv$df_adsl)
    })
    adtte <- reactive({
      req(rv$df_adtte)
      process_file(rv$df_adtte)
    })
    
    main <- reactive({
      inner_join(adsl(), adtte(), by = intersect(names(adsl()), names(adtte())))
    })
    
    # Apply variable filters
    df_filter <- mod_sub_varfilter_server("filter", main)
    
    # Survival table
    surv_table <- reactive({
      req(input$param_sel)
      adtte1 <- df_filter() %>%
        filter(.data[[input$param]] == input$param_sel) %>%
        group_by(TRT01PN) %>%
        mutate(N = n())
      
      # Status -----------------------------------------------------------------------
      # Number and percentage of died/censored
      cnsr <- adtte1 %>%
        group_by(TRT01PN, CNSR) %>%
        summarise(
          n = n_distinct(SUBJID),
          pct = n / unique(N) * 100
        ) %>%
        pivot_longer(cols = c(n, pct)) %>%
        unite(temp, TRT01PN, name) %>%
        pivot_wider(names_from = temp, values_from = value, values_fill = 0) %>%
        select(-CNSR) %>%
        mutate(label = c("  Event (%)", "  Censored (%)"), .before = 1) %>%
        mutate(across(ends_with("pct"),
                      ~paste0("(", sprintf("%.1f", .), ")"))) %>%
        unite(`0`, starts_with("0"), sep = " ") %>%
        unite(`1`, starts_with("1"), sep = " ")
      
      
      
      # Time in Months ---------------------------------------------------------
      fit <- survival::survfit(
        survival::Surv(adtte1$AVAL, adtte1$CNSR != 1) ~ adtte1$TRT01PN,
        conf.type = "log-log"
      )
      est_os_trt <- quantile(fit)
      
      # Median 95% CI
      median <- data.frame(
        label = c(
          "  25th Percentile (95% CI) ",
          "  25th Percentile (95% CI) ",
          "  Median (95% CI) ",
          "  Median (95% CI) ",
          "  75th Percentile (95% CI) ",
          "  75th Percentile (95% CI) "
        ),
        value = paste0(
          sprintf("%.1f", est_os_trt$quantile), " (",
          sprintf("%.1f", est_os_trt$lower), ", ",
          sprintf("%.1f", est_os_trt$upper), ")"
        ),
        trt01pn = rep(c(0, 1), 3)
      ) %>%
        pivot_wider(names_from = trt01pn, values_from = value, values_fill = "")
      
      adtte_filter2 <- adtte1 %>%
        mutate(CNSR = ifelse(CNSR == 0, 1, 0))
      
      # Cox model
      model <- summary(survival::coxph(survival::Surv(AVAL, CNSR) ~ TRT01PN,
                                       ties = "efron",
                                       data = adtte_filter2))
      
      lsmests <- data.frame(model$conf.int) %>%
        rename(ExpEstimate = exp.coef., LowerExp = lower..95, UpperExp = upper..95) %>%
        select(-exp..coef.) %>%
        mutate(
          Probz = (data.frame(coefficients(model)))$Pr...z..,
          across(everything(), ~ sprintf("%.4f", .))
        )
      lsmests$Probz[lsmests$Probz <= 0.001] <- "<0.001"
      
      # HR and p-value
      hr <- data.frame(
        label = c("  HR (95% CI) ", "  P-value "),
        X0 = c("", ""),
        X1 = c(paste0(
          sprintf("%.2f", as.numeric(lsmests$ExpEstimate)), " (", sprintf(
            "%.2f",
            as.numeric(lsmests$LowerExp)
          ), ", ", sprintf(
            "%.2f",
            as.numeric(lsmests$UpperExp)
          ), ")"
        ),
        lsmests$Probz)
      ) %>%
        rename("0" = "X0",
               "1" = "X1")
      
      # Probability of OS % (95% CI) -------------------------------------------
      surv_out <- summary(fit)
      
      surv_out <- data.frame(
        trt01pn = surv_out$strata,
        aval = surv_out$time,
        censor = surv_out$n.censor,
        survival = surv_out$surv,
        sdf_lcl = surv_out$lower,
        sdf_ucl = surv_out$upper
      ) %>%
        mutate(trt01pn = ifelse(trt01pn == "adtte1$TRT01PN=0", 0, 1),
               month = trunc(aval))
      
      # Probability of OS % for given months
      month_fn <- function(months, label) {
        month_0 <- surv_out %>%
          filter(month < months, trt01pn == 0)
        r1 <- tail(month_0, n = 1)
        
        month_1 <- surv_out %>%
          filter(month < months, trt01pn == 1)
        r2 <- tail(month_1, n = 1)
        
        rbind(r1, r2) %>%
          mutate(
            label = label,
            value = paste0(
              sprintf("%.1f", survival * 100), " (",
              sprintf("%.1f", sdf_lcl * 100), ", ",
              sprintf("%.1f", sdf_ucl * 100), ")"
            )
          ) %>%
          select(trt01pn, label, value)
      }
      
      month6 <- month_fn(6, "  At 6 Months")
      month12 <- month_fn(12, "  At 12 Months")
      month18 <- month_fn(18, "  At 18 Months")
      month24 <- month_fn(24, "  At 24 Months")
      
      prob_at_month <- rbind(month6, month12, month18, month24) %>%
        pivot_wider(names_from = trt01pn, values_from = value, values_fill = "")
      
      
      # Follow-Up (months) -----------------------------------------------------
      fit2 <- survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01PN,
                                conf.type = "log-log", data = adtte_filter2)
      est_flup_trt <- quantile(fit2)
      
      # Follow-up median
      flup_median <- data.frame(
        label = c("  Median (95% CI) ", "  Median (95% CI) "),
        value = paste0(
          sprintf("%.2f", est_flup_trt$quantile[, 2]), " (",
          sprintf("%.2f", est_flup_trt$lower[, 2]), ", ",
          sprintf("%.2f", est_flup_trt$upper[, 2]), ")"
        ),
        trt01pn = c(0, 1)
      ) %>%
        pivot_wider(names_from = trt01pn, values_from = "value")
      
      surv_out2 <- summary(survival::survfit(survival::Surv(AVAL, CNSR == 1) ~ TRT01PN,
                                             conf.type = "log-log",
                                             data = adtte_filter2),
                           censored = TRUE)
      
      surv_out2 <- data.frame(
        trt01pn = surv_out2$strata,
        aval = surv_out2$time,
        censor = surv_out2$n.censor,
        survival = surv_out2$surv,
        sdf_lcl = surv_out2$lower,
        sdf_ucl = surv_out2$upper
      )
      
      # Follow-up min and max
      flup_minmax <- surv_out2 %>%
        filter(aval > 0) %>%
        mutate(trt01pn = ifelse(trt01pn == "TRT01PN=0", 0, 1)) %>%
        group_by(trt01pn) %>%
        summarize(min = sprintf("%.2f", min(aval)),
                  max = sprintf("%.2f", max(aval))) %>%
        mutate(
          label = "   Min, Max",
          value = paste0(min, ", ", max)
        ) %>%
        select(-min, -max) %>%
        pivot_wider(names_from = trt01pn, values_from = "value")
      
      # Combined data frame ----------------------------------------------------
      group <- adsl() %>%
        select(TRT01P,TRT01PN)%>%
        na.omit()%>%
        distinct(TRT01P,TRT01PN)
      
      cnsr <- cnsr %>%
        mutate(seq = 10)
      median <- median %>%
        mutate(seq = 20)
      hr <- hr %>%
        mutate(seq = 30)
      prob_at_month <- prob_at_month %>%
        mutate(seq = 40)
      followup <- bind_rows(list(flup_median, flup_minmax)) %>%
        mutate(seq = 50)
      
      final <- rbind(cnsr, median, hr,prob_at_month,followup)
      
      new_colnames <-
        sapply(colnames(final),
               function(old_col)
               { new_col <- group$TRT01P[group$TRT01PN == old_col]
               return(ifelse(length(new_col)==0,old_col,new_col))
               })
      
      colnames(final) <- new_colnames
      
      final <- add_row(final, label = "Status" , seq = 1)
      final <- add_row(final, label = "Time in Months" , seq = 11)
      final <- add_row(final, label = "Probability of PFS % (95% CI)" , seq = 31)
      final <- add_row(final, label = "Follow-Up (months)" , seq = 41)
      
      
      final[is.na(final)] <- ""
      
      final <- final %>%
        arrange(seq)%>%
        select(-seq)
      
    })
    
    # Survival gt table
    output$table <- gt::render_gt({
      gt_table_format(surv_table()) %>%
        gt::cols_label(label = gt::html("Survival"))
    })
    
    # Download -----------------------------------------------------------------
    # Add report format
    report <- reactive({
      surv_table() %>%
        rename("Survival" = label) %>%
        reporter_table()
    })
    
    mod_download_server("download", report,
                        filename = "survival", format = "table")
    
  })
}