# BDS structured data, mainly lab (lb) files

# Module UI function
mod_main_lab_ui <- function(id, is.gen = FALSE) {
  ns <- NS(id)
  
  # Different labels between RWE and Data Exploration sections
  if(is.gen) {
    file1 <- "Choose a SAS data, Excel, or CSV file"
    file2 <- "Second file to merge (optional)"
    file3 <- "Third file to merge (optional)"
  } else {
    file1 <- "Choose an LB file"
    file2 <- "Choose a DM file (optional)"
    file3 <- "Choose an unfilter selection file (optional)"
  }
  
  tabItem(
    id,
    tabsetPanel(
      
      tabPanel(
        "Data Source",
        shinydashboardPlus::box(
          width = 12,
          title = "File Upload",
          collapsible = TRUE,
          dropdownMenu = reset_dropdown(ns("reset_file")),
          fluidRow(
            column(6, fileInput(ns("file_lb"), file1, accept = file_accept)),
            column(6, fileInput(ns("file_dm"), file2, accept = file_accept))
          ),
          fluidRow(
            column(6, fileInput(ns("file_us"), file3, accept = file_accept)),
            column(6, pickerInput_vars(ns("columns"), "Column select"))
          ),
          fluidRow(
            column(3, selectInput(ns("subj"), "Subject ID", NULL)),
            conditionalPanel(
              "input.param_radio != 'PARAMCD'", ns = ns,
              column(3, selectInput(ns("param"), "Parameter", NULL))
            ),
            conditionalPanel(
              "input.param_radio != 'PARAM'", ns = ns,
              column(3, selectInput(ns("paramcd"), "Parameter (CD)", NULL))
            ),
            column(3, selectInput(ns("aval"), "Analysis Value", NULL))
          ),
          fluidRow(
            column(12, radioButtons(ns("param_radio"), "Parameter to display", 
                                    c("PARAM", "PARAMCD", "PARAM (PARAMCD)"),
                                    inline = TRUE))
          ),
          hr(),
          p(strong("Optional"), align = "center"),
          fluidRow(
            column(3, selectInput(ns("unit"), "Unit", "None",
                                  selected = "None")),
            column(3, selectInput(ns("vendor"), "Vendor", "None",
                                  selected = "None"))
          ),
          fluidRow(
            column(3, selectInput(ns("date"), "Date", "None")),
            column(3, radioGroupButtons(
              ns("date_ineq"), "Date inequality",
              c("\U2264", "<"), justified = TRUE)
            ),
            column(3, selectInput(ns("t0"), "Cutoff date", "None",
                                  selected = "None")),
            column(3, numericInput(ns("date_diff"),
                                   help_link("Date difference limit",
                                             ns("date_diff_help")),
                                   NULL, min = 0, max = Inf))
          ),
          shinyBS::bsPopover(
            ns("date_diff_help"), NULL,
            paste("Maximum number of days allowed between",
                  "<b>Date</b> and <b>Cutoff date</b>")
          )
        ),
        shinydashboardPlus::box(
          width = 12, 
          title = "Subject Data Listing",
          dropdownMenu = mod_download_ui(ns("download_data"), format = "data"),
          column(12, align = "center", htmlOutput(ns("subjects_data"))),
          DTOutput_div(ns("table_data")))
      ),
      
      # Missing Percentage -----------------------------------------------------
      tabPanel(
        "Missing Percentage",
        shinydashboardPlus::box(
          width = 12, 
          title = "Missing Percentage",
          dropdownMenu = mod_download_ui(ns("download_missing"), format = "data"),
          DTOutput_div(ns("table_missing")),
          div(id = ns("multiple_vendors"),
              br(),
              div(id = ns("diff_regions"),
                  materialSwitch(ns("combine_vendors"),
                                 strong("Combine regions"), 
                                 status = "primary",
                                 right = TRUE)
              ),
              uiOutput(ns("missing_pie_ui"))
          )
        )
      ),
      
      # Units ------------------------------------------------------------------
      tabPanel(
        "Assessment Units",
        shinydashboardPlus::box(
          width = 12, 
          title = "Assessment Units",
          dropdownMenu = mod_download_ui(ns("download_units"), format = "data"),
          fluidRow(
            column(12, align = "center",
                   a(id = ns("toggle_unit_inputs"), 
                     "Show conversion options", href = "#"))
          ),
          shinyjs::hidden(
            div(id = ns("unit_inputs"),
                fluidRow(
                  column(3, selectInput(ns("unit_param"), "Parameter", NULL)),
                  column(3, selectInput(ns("unit_old"), "Old unit", NULL)),
                  column(3, selectInput(ns("unit_new"), "New unit", NULL)),
                  column(3, searchInput(
                    ns("unit_convert"), 
                    help_link("Conversion expression",
                              ns("unit_convert_help")),
                    btnSearch = shiny::icon("arrow-right"))
                  )
                ),
                shinyBS::bsPopover(
                  ns("unit_convert_help"), NULL,
                  paste('Enter an expression that begins with an arithmetic',
                        'operator (e.g. "+4") or one that contains "x"',
                        '(e.g. "x*100") for <b>numeric conversion</b>.<br>',
                        'Other text (e.g. "25", "YES") will result in', 
                        '<b>replacement</b>.<br>',
                        'Leave <b>blank</b> to only change unit.')
                )
            )
          ),
          DTOutput_div(ns("table_units"))
        )
      ),
      
      # Statistical Summary ----------------------------------------------------
      mod_sub_statsummary_ui(ns("summary"))
      
    )
  )
}

# Module server function
mod_main_lab_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    
    # Upload files
    observe({
      req(input$file_lb)
      rv$df_lb <- upload_file(input$file_lb)
    })
    observe({
      req(input$file_dm)
      rv$df_dm <- upload_file(input$file_dm)
    })
    observe({
      req(input$file_us)
      rv$df_us <- upload_file(input$file_us)
    })
    
    # Reset fileInputs
    observeEvent(input$reset_file, {
      rv$df_lb <- NULL
      rv$df_dm <- NULL
      rv$df_us <- NULL
      shinyjs::reset("file_lb")
      shinyjs::reset("file_dm")
      shinyjs::reset("file_us")
    })
    
    # Get all variable names
    all_var_names <- reactive({
      all_names <- names(rv$df_lb)
      if(!is.null(rv$df_dm)) {
        all_names <- c(all_names, names(rv$df_dm))
      }
      if(!is.null(rv$df_us)) {
        all_names <- c(all_names, names(rv$df_us))
      }
      unique(all_names)
    })
    
    # Get all date variable names
    date_names <- reactive({
      date_names <- names(rv$df_lb)[purrr::map_lgl(rv$df_lb, lubridate::is.Date)]
      if(!is.null(rv$df_dm)) {
        date_names <- c(date_names, names(rv$df_dm)[purrr::map_lgl(rv$df_dm, lubridate::is.Date)])
      }
      if(!is.null(rv$df_us)) {
        date_names <- c(date_names, names(rv$df_us)[purrr::map_lgl(rv$df_us, lubridate::is.Date)])
      }
      unique(date_names)
    })
    
    # Ensure Subject ID variable is present in all uploaded files
    subj_choices <- reactive({
      all_names <- names(rv$df_lb)
      if(!is.null(rv$df_dm)) {
        all_names <- intersect(all_names, names(rv$df_dm))
      }
      if(!is.null(rv$df_us)) {
        all_names <- intersect(all_names, names(rv$df_us))
      }
      unique(all_names)
    })
    
    # Update variables
    observe({
      all_names <- all_var_names()
      updateSelectInput(session, "subj", choices = subj_choices(),
                        selected = grep_var("subjid", subj_choices()))
      updateSelectInput(session, "param", choices = all_names,
                        selected = grep_var("param", all_names))
      updateSelectInput(session, "paramcd", choices = all_names,
                        selected = grep_var("paramcd", all_names))
      updateSelectInput(session, "aval", choices = all_names,
                        selected = grep_var("aval", all_names))
      if(grepl_var("^unit$", all_names)) {
        unit_select <- grep_var("^unit$", all_names)
      } else if(grepl_var("std_unit", all_names)) {
        unit_select <- grep_var("std_unit", all_names)
      } else if(grepl_var("unit", all_names)) {
        unit_select <- grep_var("unit", all_names)
      } else {
        unit_select <- "None"
      }
      updateSelectInput(session, "unit", choices = c("None", all_names),
                        selected = unit_select)
      updateSelectInput(session, "vendor", choices = c("None", all_names),
                        selected = ifelse_var("source", all_names))
      # Date variables
      date_names <- date_names()
      updateSelectInput(session, "date", choices = c("None", date_names),
                        selected = ifelse_var("avaldt", date_names))
      updateSelectInput(session, "t0", choices = c("None", date_names),
                        selected = ifelse_var("t0", date_names))
    })
    
    # Determine if date range input is valid
    observeEvent(input$date_diff, {
      if(is.na(input$date_diff)) {
        rv$date_diff_filter <- FALSE
      } else if(grepl("\\.|-", input$date_diff)) {
        rv$date_diff_filter <- FALSE
      } else {
        rv$date_diff_filter <- TRUE
      }
    })
    
    # Are both date inputs selected?
    date_option <- reactive(all(input$date != "None", input$t0 != "None"))
    
    # Process and combine data frames
    observe({
      req(all(c(input$subj, input$aval) %in% all_var_names()))
      df <- process_file(rv$df_lb, input$subj)
      subj <- input$subj
      
      # Merge files
      if(!is.null(rv$df_dm)) {
        df_dm <- process_file(rv$df_dm, input$subj)
        df <- left_join(df, df_dm, by = subj, suffix = c("", ".y")) %>%
          select(-ends_with(".y"))
      }
      if(!is.null(rv$df_us)) {
        df_us <- process_file(rv$df_us, input$subj)
        df <- left_join(df, df_us, by = subj, suffix = c("", ".y")) %>%
          select(-ends_with(".y"))
      }
      df <- distinct(df, .keep_all = TRUE)
      
      # Select Parameter display
      if(input$param_radio == "PARAM") {
        req(input$param %in% all_var_names())
        df <- df %>%
          mutate(param_full = .data[[input$param]])
      }
      else if(input$param_radio == "PARAMCD") {
        req(input$paramcd %in% all_var_names())
        df <- df %>%
          mutate(param_full = .data[[input$paramcd]])
      }
      else if(input$param_radio == "PARAM (PARAMCD)") {
        req(all(c(input$param, input$paramcd) %in% all_var_names()))
        df <- df %>%
          mutate(param_full = paste0(.data[[input$param]], " (", 
                                     .data[[input$paramcd]], ")"))
      }
      
      # Replace missing unit with "NR"
      if(input$unit != "None") {
        req(input$unit %in% names(df))
        df <- df %>%
          mutate(across(all_of(input$unit), ~ifelse(is.na(.x), "NR", .x)))
      }
      
      # Filter dates
      if(date_option()) {
        df <- df %>%
          mutate(diff = .data[[input$t0]] - .data[[input$date]]) %>%
          group_by(.data[[input$subj]], param_full)  %>%
          filter(diff == min(diff[diff >= 0], na.rm = TRUE)) %>%
          suppressWarnings()
        
        # Further filter based on date diff limit
        if(rv$date_diff_filter) {
          if(input$lb_date_ineq == "<") {
            df <- filter(df, diff < input$date_diff)
          } else {
            df <- filter(df, diff <= input$date_diff)
          }
        }
        # Remove duplicate entries and ungroup
        df <- df %>%
          slice_tail() %>%
          ungroup()
      }
      
      # Convert values if all are numeric
      nonnumeric <- nonnumeric(df[[input$aval]][!is.na(df[[input$aval]])])
      all_numeric <- !any(nonnumeric)
      if(all_numeric) {
        df <- mutate(df, across(all_of(input$aval), as.numeric))
      }
      
      rv$df_lab_comb <- df
    })
    
    # Output number of subjects
    output$subjects_data <- renderUI({
      req(rv$df_lab_comb)
      req(input$subj)
      subjects_HTML(subjects_fn(rv$df_lab_comb, input$subj))
    })
    
    # Column names
    observe({
      req(rv$df_lab_comb)
      df <- select(rv$df_lab_comb, -any_of(c("diff", "param_full")))
      choices <- names(df)
      choice_labels <- var_labels(df)
      updatePickerInput(session, "columns", choices = choices, selected = choices,
                        choicesOpt = list(subtext = choice_labels))
    })
    
    # Data to display
    data_display <- reactive({
      df <- rv$df_lab_comb
      req(df)
      df %>%
        select(-any_of(c("diff", "param_full"))) %>%
        select(any_of(input$columns))
    })
    
    # Main data table
    output$table_data <- DT::renderDT({
      data_display() %>%
        data_table_fn()
    })
    
    # Missing entries ----------------------------------------------------------
    df_missing <- reactive({
      df <- rv$df_lab_comb
      req(df)
      n_subj <- subjects_fn(df, input$subj)
      df %>%
        distinct(param_full, .data[[input$subj]]) %>%
        group_by(param_full) %>%
        summarize("# Missing" = n_subj - n(),
                  N = n_subj,
                  "% Missing" = 1 - n()/n_subj) %>%
        rename("Parameter" = param_full)
    })
    
    # Missing entry table
    output$table_missing <- DT::renderDT({
      dt <- DT::datatable(df_missing())
      miss_table(dt, 4)
    })
    
    # Show/hide missing entry vendor pie charts
    observe({
      cond <- n_distinct(rv$df_lab_comb[[input$vendor]]) > 1
      shinyjs::toggle("multiple_vendors", condition = cond)
    })
    
    # Show/hide combine regions toggle
    observe({
      df <- rv$df_lab_comb
      vendor_region <- sub("_.*", "", df[[input$vendor]])
      cond <- n_distinct(df[[input$vendor]]) != n_distinct(vendor_region)
      shinyjs::toggle("diff_regions", condition = cond)
    })
    
    # Data frame of missing entries by vendor
    df_missing_vendor <- reactive({
      df <- rv$df_lab_comb
      req(df)
      df <- df %>%
        distinct(param_full, .data[[input$subj]], .keep_all = TRUE)
      if(input$combine_vendors) {
        df <- mutate(df, across(all_of(input$vendor), ~sub("_.*", "", .)))
      }
      df1 <- df %>%
        group_by(.data[[input$vendor]]) %>%
        summarize(total = n_distinct(.data[[input$subj]]))
      df2 <- df %>%
        group_by(param_full, .data[[input$vendor]]) %>%
        summarize(entries = n(),
                  .groups = "drop_last")
      combos <- expand.grid(param = unique(df[["param_full"]]), 
                            vendor = unique(df[[input$vendor]]))
      names(combos)[c(1, 2)] <- c("param_full", input$vendor)
      
      full_join(combos, df1, by = input$vendor) %>%
        full_join(df2, by = c("param_full", input$vendor)) %>%
        replace_na(list(entries = 0)) %>%
        group_by(param_full, .data[[input$vendor]]) %>%
        summarize("n_missing" = total - entries,
                  .groups = "drop_last")
    })
    
    # Missing entry pie charts (by vendor)
    output$missing_vendor_pie <- plotly::renderPlotly({
      custom_pie(df_missing_vendor(), 
                 df_missing()[[1]], 
                 label = input$vendor, 
                 value = "n_missing", 
                 title_text = "% Missing by Vendor", 
                 show_legend = TRUE,
                 legend_title = "<b>Vendor</b>", 
                 download_text = "-missing_entry_chart")
    })
    
    # Render missing entry pie charts
    output$missing_pie_ui <- renderUI({
      height <- ceiling(nrow(df_missing())/3) * 500
      plotly::plotlyOutput(ns("missing_vendor_pie"), height = height)
    })
    
    # Units --------------------------------------------------------------------
    df_units <- reactive({
      req(input$unit != "None")
      df <- rv$df_lab_comb
      req(df)
      req(input$unit %in% names(df))
      df %>%
        select(param_full, all_of(input$unit)) %>%
        distinct(param_full, .data[[input$unit]]) %>%
        arrange(param_full, .data[[input$unit]]) %>%
        rename("Parameter" = param_full,
               Unit = input$unit)
    })
    
    # Units table
    output$table_units <- DT::renderDT({
      DT::datatable(df_units(),
                    extensions = "RowGroup",
                    options = list(rowGroup = list(dataSrc = 1)))
    })
    
    # Show/hide unit conversion options
    shinyjs::onclick("toggle_unit_inputs", 
                     shinyjs::toggle("unit_inputs", anim = TRUE))
    
    # Parameters in the data
    df_params <- reactive(sort(unique(rv$df_lab_comb$param_full)))
    
    # Update parameter select
    observe({
      df_params <- df_params()
      selected_param <- ""
      # Prevent reset of parameter
      if(input$unit_param %in% df_params) selected_param <- input$unit_param
      updateSelectInput(session, "unit_param", choices = df_params,
                        selected = selected_param)
    })
    
    # Update unit selects
    observe({
      req(input$unit_param)
      df <- rv$df_lab_comb %>%
        filter(param_full == input$unit_param)
      units <- sort(unique(df[[input$unit]]))
      selected_old <- ""
      selected_new <- ""
      # Prevent reset of units
      if(input$unit_old %in% units) selected_old <- input$unit_old
      if(input$unit_new %in% units) selected_new <- input$unit_new
      updateSelectInput(session, "unit_old", choices = units,
                        selected = selected_old)
      updateSelectInput(session, "unit_new", choices = units,
                        selected = selected_new)
    })
    
    # Unit conversion
    observeEvent(input$unit_convert_search, {
      df <- rv$df_lab_comb
      all_units <- df[(df$param_full == input$unit_param), ][[input$unit]]
      req(all(c(input$unit_old, input$unit_new) %in% all_units))
      
      cond <- (df$param_full == input$unit_param & 
                 df[[input$unit]] == input$unit_old)
      unit_convert <- input$unit_convert
      value_type <- "none"
      nonnumeric <- nonnumeric(df[cond, ][[input$aval]])
      
      if(unit_convert == "") {
        value <- expression("x")
      } else if(grepl("^[-\\.+\\*\\/^\\()x[:digit:]]*$", unit_convert)) {
        
        # Append x if starting with arithmetic operator
        if(!any(nonnumeric) && !grepl("x", unit_convert) && 
           grepl("^[-+/*]", unit_convert)) {
          unit_convert <- paste0("x", unit_convert)
        }
        value_type <- "expression"
        if(any(nonnumeric) || grepl("\\.x|x\\.", unit_convert) || 
           grepl("[-+\\*\\/^]{2}", unit_convert)) {
          value <- unit_convert
        } else {
          # Insert * symbol if not included
          times_pat <- "x\\d|\\dx"
          if(grepl(times_pat, unit_convert)) {
            y <- unit_convert
            pos <- as.vector(gregexpr(times_pat, unit_convert)[[1]])
            for(i in 1:length(pos)) {
              y <- gsub(paste0("^(.{", pos[i], "})(.*)$"),
                        paste0("\\1\\*\\2"),
                        y)
              pos <- pos + 1
            }
            value <- parse(text = y)
          } else {
            value <- parse(text = unit_convert)
          }
        }
      } else {
        value <- unit_convert
      }
      df2 <- df %>%
        mutate(across(all_of(input$unit), 
                      ~ifelse(cond, input$unit_new, .x)))
      # Unit conversion for numeric values stored as characters
      convert_char <- function(x, condition) {
        if(condition) {
          x <- as.numeric(x)
          return(eval(value))
        } else {
          return(x)
        }
      }
      # Convert numeric character values
      if(is.character(df[[input$aval]]) && !any(nonnumeric) && 
         value_type == "expression") {
        df2 <- df2 %>%
          mutate(across(all_of(input$aval), 
                        ~purrr::map2_chr(.x, cond, convert_char)))
      } else {
        df2 <- df2 %>%
          mutate(across(all_of(input$aval),
                        function(x) ifelse(cond, eval(value), x)))
      }
      rv$df_lab_comb <- df2
    })
    
    # Statistical summary ------------------------------------------------------
    df_summary_prep <- reactive({
      req(rv$df_lab_comb)
      df <- rv$df_lab_comb %>%
        rename(Value = input$aval)
      
      if(input$unit == "None") {
        df <- df %>%
          rename(Variable = param_full)
      } else {
        req(input$unit %in% names(df))
        df <- df %>%
          rename(Unit_ = input$unit) %>%
          mutate(Variable = paste0(param_full, " (", Unit_, ")"))
      }
      df
    })
    
    # Label of parameter (+ unit) variable
    var_name <- reactive({
      if(input$unit == "None") {
        "Parameter"
      } else {
        "Parameter (Unit)"
      }
    })
    
    mod_sub_statsummary_server("summary", df_summary_prep,
                            is.bds = TRUE, var_name = var_name)
    
    # Download -----------------------------------------------------------------
    
    # Uploaded data
    mod_download_server("download_data", data_display,
                        filename = "data", format = "data")
    # Missing entries data
    mod_download_server("download_missing", df_missing,
                        filename = "missing", format = "data")
    # Units data
    mod_download_server("download_units", df_units,
                        filename = "units", format = "data")
    
  })
}
