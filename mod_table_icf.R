# Efficacy table: Summary by Visit (ICF)

# Module UI function
mod_table_icf_ui <- function(id) {
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
        column(6, fileInput(ns("file_eff"), "Choose an Efficacy file", 
                            accept = file_accept))
      ),
      fluidRow(
        column(6, fileInput(ns("file_3"), "Choose a third file (optional)", 
                            accept = file_accept))
      ),
      fluidRow(
        column(3, selectInput(ns("subj"), "Subject ID", NULL)),
        column(3, selectInput(ns("param"), "Parameter", NULL)),
        column(3, selectInput(ns("aval"), "Analysis Value", NULL)),
        column(3, selectInput(ns("visit"), "Analysis Visit", NULL))
      ),
      fluidRow(
        column(3, selectInput(ns("trt"), "Treatment", NULL)),
        column(3, selectInput(ns("trtn"), "Treatment (N) (optional)", "None",
                              selected = "None")),
        column(3, selectInput(ns("subgroup"), "Subgroup (optional)", "None",
                              selected = "None"))
      ),
      hr(),
      fluidRow(
        column(6, selectInput(ns("param_sel"), "Parameter Select", NULL)),
        column(3, selectInput(ns("param_type"), "Variable Type", 
                              c("Quantitative", "Categorical"))),
        column(3, pickerInput_vars(ns("studyid"), "Study IDs (optional)"))
      )
    ),
    table_option_box(ns, uiOutput(ns("opt_subgroup_ui"))),
    shinydashboardPlus::box(
      width = 9, 
      title = "Efficacy Table",
      dropdownMenu = mod_download_ui(ns("download"), format = "table"),
      gt::gt_output(ns("table")) %>% add_spinner()
    ),
    mod_sub_varfilter_ui(ns("filter"))
  )
}

# Module server function
mod_table_icf_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Upload files
    observe({
      req(input$file_adsl)
      rv$df_adsl <- upload_file(input$file_adsl)
    })
    observe({
      req(input$file_eff)
      rv$df_eff <- upload_file(input$file_eff)
    })
    observe({
      req(input$file_3)
      rv$df_3 <- upload_file(input$file_3)
    })
    
    # Reset fileInput
    observeEvent(input$reset_file, {
      rv$df_adsl <- NULL
      rv$df_eff <- NULL
      rv$df_3 <- NULL
      shinyjs::reset("file_adsl")
      shinyjs::reset("file_eff")
      shinyjs::reset("file_3")
    })
    
    # Update variables
    observe({
      choices1 <- names(rv$df_adsl)
      choices2 <- names(rv$df_eff)
      choices_common <- intersect(choices1, choices2)
      choices_all <- unique(c(choices1, choices2))
      
      if(!is.null(rv$df_3)) {
        choices3 <- names(rv$df_3)
        choices_common <- intersect(choices_common, choices3)
        choices_all <- unique(c(choices_all, choices3))
      }
      
      trt_choices <- grep("^trt01", choices_all, TRUE, value = TRUE)
      study_choices <- grep("^stdy", choices_all, TRUE, value = TRUE)
      
      updateSelectInput(session, "subj", choices = choices_common,
                        selected = grep_var("subj", choices_common))
      updateSelectInput(session, "param", choices = choices_all,
                        selected = grep_var("param", choices_all))
      updateSelectInput(session, "aval", choices = choices_all,
                        selected = grep_var("aval", choices_all))
      updateSelectInput(session, "visit", choices = choices_all,
                        selected = grep_var("visit", choices_all))
      updateSelectInput(session, "trt", choices = trt_choices)
      updateSelectInput(session, "trtn", choices = c("None", trt_choices),
                        selected = ifelse_var(".*n$", trt_choices))
      updateSelectInput(session, "subgroup", choices = c("None", choices_all))
      updatePickerInput(session, "studyid", choices = study_choices)
    })
    observe({
      choices <- unique(rv$df_eff[[input$param]])
      updateSelectInput(session, "param_sel", choices = choices)
    })
    
    # Main data
    main <- reactive({
      req(rv$df_adsl)
      req(rv$df_eff)
      req(input$subj %in% names(rv$df_adsl))
      req(input$subj %in% names(rv$df_eff))
      df1 <- process_file(rv$df_adsl)
      df2 <- process_file(rv$df_eff)
      df <- inner_join(df1, df2, by = input$subj, suffix = c("", ".y")) %>%
        select(-ends_with(".y"))
      
      if(!is.null(rv$df_3)) {
        req(input$subj %in% names(rv$df_3))
        df3 <- process_file(rv$df_3)
        df <- inner_join(df, df3, by = input$subj, suffix = c("", ".y")) %>%
          select(-ends_with(".y"))
      }
      df
    })
    
    # Apply variable filters
    df_filter <- mod_sub_varfilter_server("filter", main)
    
    # Prepare data frame
    icf_df <- reactive({
      req(all(c(input$subj, input$param, input$aval,
                input$visit, input$trt) %in% names(df_filter())))
      req(input$param_sel)
      df <- df_filter() %>%
        drop_na(input$trt)
      
      # Order treatments
      df <- order_trt(df, input$trt, input$trtn)
      
      # Study ID filter
      # Keep row if "Y" is present in any of selected columns
      # (remove rows with only NA in selected columns)
      if(!is.null(input$studyid)) {
        df <- df %>%
          unite(col_keep, input$studyid, remove = FALSE, na.rm = TRUE) %>%
          filter(col_keep != "") %>%
          select(-col_keep)
      }
      
      # Pivot data and add N to each treatment group
      df %>%
        filter(.data[[input$param]] == input$param_sel) %>%
        select(all_of(c(input$subj, input$aval, input$visit)), 
               trt, any_of(input$subgroup)) %>%
        filter(.data[[input$visit]] != "") %>%
        distinct() %>%
        pivot_wider(names_from = input$visit, names_prefix = "visit_", 
                    values_from = input$aval) %>%
        select(-all_of(input$subj)) %>%
        group_by(trt) %>%
        mutate(N = n()) %>%
        ungroup()
    })
    
    # Big N for each treatment
    icf_N <- reactive({
      icf_df() %>%
        select(trt, N) %>%
        distinct() %>%
        arrange(trt)
    })
    
    # Get original visit variables
    orig_visit_vars <- reactive({
      df <- icf_df()
      req(df)
      visit_vars <- grep("^visit_", names(df), value = TRUE)
      sub("^visit_", "", visit_vars)
    })
    
    # Efficacy table -----------------------------------------------------------
    icf_table <- reactive({
      df <- icf_df()
      req(df)
      
      # Create ICF summary
      icf_summary <- function(data, group_var = NULL) {
        visit_vars <- grep("^visit_", names(data), value = TRUE)
        visit_vars_orig <- sub("^visit_", "", visit_vars)
        
        if(input$param_type == "Quantitative") {
          visit_tbls <- purrr::map2(visit_vars, visit_vars_orig,
                                    ~summarize_stats_demo(data, "trt", .x, .y,
                                                          stat_keep = input$table_stats))
        } else {
          visit_tbls <- purrr::map2(visit_vars, visit_vars_orig,
                                    ~charfreq_demo(data, "trt", .x, .y))
        }
        names(visit_tbls) <- visit_vars_orig
        
        
        # Combine tables
        final_tbl <- do.call(rbind, visit_tbls) %>%
          tibble::rownames_to_column() %>%
          select(-rowname)
        
        # Add N
        trt_N <- paste0(icf_N()$trt, "\n(N=", icf_N()$N, ")")
        names(final_tbl) <- c("Variable", trt_N)
        
        if(!is.null(group_var)) {
          final_tbl <- final_tbl %>%
            mutate(group = group_var)
        }
        final_tbl
      }
      
      if(input$subgroup == "None") {
        icf_summary(df)
      } else {
        # Separate subgroups
        groups <- unique(df[[input$subgroup]])
        final <- vector("list", length(groups))
        
        for(i in seq_along(groups)) {
          df_i <- filter(df, .data[[input$subgroup]] == groups[i])
          final[[i]] <- icf_summary(df_i, groups[i])
        }
        names(final) <- groups
        final
      }
    })
    
    # Extract new labels for subgroups
    subgroup_labels <- reactive({
      df <- icf_df()
      req(df)
      groups <- unique(df[[input$subgroup]])
      
      if(!is.null(input[[paste0("group_label_", groups[1])]])) {
        labels <- vector(length = length(groups))
        for (i in seq_along(groups)) {
          group_i <- groups[i]
          label_i <- input[[paste0("group_label_", group_i)]]
          labels[i] <- ifelse(label_i == "", group_i, label_i)
        }
      } else {
        labels <- groups
      }
      labels
    })
    
    # Order of subgroups
    subgroup_order <- reactive({
      df <- icf_df()
      req(df)
      groups <- unique(df[[input$subgroup]])
      if(!is.null(input[[paste0("group_position_", groups[1])]])) {
        pos <- vector(length = length(groups))
        for (i in seq_along(groups)) {
          group_i <- groups[i]
          pos_i <- input[[paste0("group_position_", group_i)]]
          pos[i] <- pos_i
        }
      } else {
        pos <- 1:length(groups)
      }
      labels <- subgroup_labels()
      req(length(labels) == length(pos))
      data.frame(group = labels, pos = pos) %>%
        arrange(pos)
    })
    
    # Efficacy gt table
    output$table <- gt::render_gt({
      tbl <- icf_table()
      if(input$subgroup == "None") {
        gtt <- gt::gt(tbl)
      } else {
        labels <- subgroup_labels()
        order <- subgroup_order()$group
        n <- purrr::map_dbl(tbl, nrow)
        tbl <- do.call("rbind", tbl) %>%
          select(-any_of("group"))
        
        gtt <- gt::gt(tbl)
        start <- 1
        
        # Relabel and order subgroup
        for(i in seq_along(labels)) {
          gtt <- gtt %>%
            gt::tab_row_group(
              label = labels[i],
              rows = start:(start + n[i] - 1)
            )
          start <- start + n[i]
        }
        gtt <- gtt %>%
          gt::row_group_order(order)
      }
      gt_table_format(gtt, tbl,
                      input$table_title, input$table_footnote)
    })
    
    # Additional options -------------------------------------------------------
    opt_subgroup <- reactive({
      groups <- unique(df_filter()[[input$subgroup]])
      n <- length(groups)
      input_tagList <- vector("list", n)
      for(i in 1:n) {
        var_i <- groups[i]
        new_input <- fluidRow(
          column(2, p(var_i)),
          column(2, numericInput(ns(paste0("group_position_", var_i)), NULL, 
                                 value = i, min = 1, max = n)),
          column(8, textInput(ns(paste0("group_label_", var_i)), NULL)),
          # Match margins of other options
          style = "margin-left:0px;margin-right:0px"
        )
        input_tagList[[i]] <- new_input
      }
      input_tagList
    })
    
    output$opt_subgroup_ui <- renderUI({
      if(input$subgroup == "None") {
        NULL
      } else {
        div(
          p(strong("Subgroup"), align = "center"),
          fluidRow(
            column(2, p(strong("Variable"))),
            column(2, p(strong("Position"))),
            column(8, p(strong("Label"))),
            # Match margins of option_inputs
            style = "margin-left:0px;margin-right:0px"
          ),
          opt_subgroup(),
          hr(style = "border-top:1px solid #777; margin-bottom:10px")
        )
      }
    })
    
    # Download ---------------------------------------------------------------------
    # Add report format
    report <- reactive({
      df <- icf_table()
      req(df)
      if(input$subgroup == "None") {
        group_var <- NULL
      } else {
        df <- do.call("rbind", df)
        groups <- unique(df$group)
        labels <- subgroup_labels()
        recode <- data.frame(group = groups, new_group = labels)
        order <- subgroup_order()$group
        
        # Relabel and order subgroup
        df <- df %>%
          left_join(recode, by = "group") %>%
          mutate(group = factor(new_group, levels = order)) %>%
          select(-new_group) %>%
          arrange(group)
        group_var <- "group"
      }
      report_table(df, input$table_title, input$table_footnote, group_var)
    })
    
    mod_download_server("download", report,
                        filename = "efficacy", format = "table")
    
  })
}
