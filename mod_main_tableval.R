# Table validation: compare sas7bdat with RTF files

# Module UI function
mod_main_tableval_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "File Upload",
      collapsible = TRUE,
      dropdownMenu = reset_dropdown(ns("reset_file")),
      fluidRow(
        column(12, align = "center",
               div(actionLink(ns("show_guide"), "Display user guide"),
                   style = "padding-bottom:10px"))
      ),
      fluidRow(
        column(6, fileInput(ns("file_adsl"), "Choose an ADSL file",
                            accept = ".sas7bdat")),
        column(6, fileInput(ns("file_rtf"), "Choose the RTF tables to validate",
                            multiple = TRUE, accept = ".rtf"))
      ),
      fluidRow(
        column(6, fileInput(ns("file_adrs"), "Choose an ADRS file",
                            accept = ".sas7bdat")),
        column(6, fileInput(ns("file_adtte"), "Choose an ADTTE file",
                            accept = ".sas7bdat"))
      ),
      fluidRow(
        column(12, align = "center",
               shinyjs::disabled(actionButton(ns("run_button"),
                                              "Start Validation",
                                              class = "btn-primary")))
      )
    ),
    shinydashboardPlus::box(
      width = 12,
      title = "Comparison",
      dropdownMenu = mod_download_ui(ns("download"), format = "table"),
      DT::DTOutput(ns("table_overview"))
    )
  )
}

# Module server function
mod_main_tableval_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues()
    
    # Source files
    source_files <- list.files("./extra/tables", pattern = "\\.R$",
                               full.names = TRUE)
    for (i in seq_along(source_files)) {
      source(source_files[i], local = TRUE)
    }
    
    # Valid table names
    valid_table_names <- c(
      paste0("disposition_", c("eff", "lk", "eu")),
      paste0("baseline_demo_", c("eff", "lk", "eff_trans",
                                 "eff_diag", "eff_bridge", "qcc_region",
                                 "qcc_ecog", "qcc_dss", "qcc_dasr")),
      paste0("follow_up_", c("eff", "lk", "eff_trans",
                             "eff_diag", "eff_bridge", "region",
                             "eff_ecog", "eff_dss")),
      paste0("orr_crude_", c("eff", "lk", "eff_trans",
                             "eff_diag", "eff_bridge", "eff_eu",
                             "eff_ecog", "eff_dss")),
      paste0("crr_crude_", c("eff", "lk", "eff_trans",
                             "eff_diag", "eff_bridge", "eff_eu",
                             "eff_ecog", "eff_dss")),
      paste0("os_crude_", c("eff", "lk", "eff_trans",
                            "eff_diag", "eff_bridge", "eff_no_bridge",
                            "eff_eu", "eff_ecog", "eff_dss"))
    )
    
    # Show user guide as modal dialog
    observeEvent(input$show_guide, {
      showModal(modalDialog(
        p("The following tables are currently able to be validated:"),
        p(strong("Disposition:"), paste(
          paste0("disposition_", c("eff", "lk", "eu")), 
          collapse = ", ")),
        p(strong("Baseline Demographics:"), paste(
          paste0("baseline_demo_", c("eff", "lk", "eff_trans",
                                     "eff_diag", "eff_bridge", "qcc_region",
                                     "qcc_ecog", "qcc_dss", "qcc_dasr")),
          collapse = ", ")),
        p(strong("Follow-up:"), paste(
          paste0("follow_up_", c("eff", "lk", "eff_trans",
                                 "eff_diag", "eff_bridge", "region",
                                 "eff_ecog", "eff_dss")),
          collapse = ", ")),
        p(strong("Crude Overall Response Rate:"), paste(
          paste0("orr_crude_", c("eff", "lk", "eff_trans",
                                 "eff_diag", "eff_bridge", "eff_eu",
                                 "eff_ecog", "eff_dss")),
          collapse = ", "),
          strong("(requires ADRS)")),
        p(strong("Crude Complete Response Rate:"), paste(
          paste0("crr_crude_", c("eff", "lk", "eff_trans",
                                 "eff_diag", "eff_bridge", "eff_eu",
                                 "eff_ecog", "eff_dss")),
          collapse = ", "),
          strong("(requires ADRS)")),
        p(strong("Crude Overall Survival:"), paste(
          paste0("os_crude_", c("eff", "lk", "eff_trans",
                                "eff_diag", "eff_bridge", "eff_no_bridge",
                                "eff_eu", "eff_ecog", "eff_dss")),
          collapse = ", "),
          strong("(requires ADTTE)")),
        p(strong("All tables require an ADSL file to be uploaded.")),
        title = "Table Validation Guide",
        footer = modalButton("Close"),
        size = "l",
        easyClose = TRUE
      ))
    })
    
    # Upload files -------------------------------------------------------------
    observe({
      req(input$file_adsl)
      rv$df_adsl <- upload_file(input$file_adsl)
    })
    observe({
      req(input$file_adrs)
      rv$df_adrs <- upload_file(input$file_adrs)
    })
    observe({
      req(input$file_adtte)
      rv$df_adtte <- upload_file(input$file_adtte)
    })
    
    # Comparison RTF
    observe({
      inFile <- input$file_rtf
      req(inFile)
      inFile <- arrange(inFile, name)
      if(!is.null(inFile)) {
        rtf_list <- list()
        for(i in 1:nrow(inFile)) {
          # Skip if file name is not in valid table list
          if(tools::file_path_sans_ext(inFile[[i, "name"]]) %in% valid_table_names) {
            file_i <- upload_rtf(inFile[[i, "datapath"]])
            rtf_list[[i]] <- file_i
          } else{
            rtf_list[[i]] <- NULL
          }
        }
        rv$rtf_list <- purrr::compact(rtf_list)
      }
    })
    
    # Upload and parse RTF files
    upload_rtf <- function(file) {
      req(file)
      ext <- tools::file_ext(file)
      validate(
        need(ext == "rtf", "Please upload an RTF file.")
      )
      striprtf::read_rtf(file, row_start = "")
    }
    
    # Reset fileInputs
    observeEvent(input$reset_file, {
      rv$df_adsl <- NULL
      rv$df_adrs <- NULL
      rv$df_adtte <- NULL
      rv$rtf_list <- NULL
      shinyjs::reset("file_adsl")
      shinyjs::reset("file_adrs")
      shinyjs::reset("file_adtte")
      shinyjs::reset("file_rtf")
    })
    
    # Enable run button
    observe({
      condition <- !is.null(rv$df_adsl) & !is.null(rv$rtf_list)
      shinyjs::toggleState("run_button", condition)
    })
    
    # Shows an alert message to the user with the given text
    custom_alert <- function(text) {
      sendSweetAlert(
        session,
        title = "Error",
        text = text,
        type = "warning",
        btn_labels = NA,
        width = 300
      )
    }
    
    # Helper functions ---------------------------------------------------------
    
    # Summarize n and percentage, then pivot the data
    summarize_pivot <- function(df, var, fill = NA) {
      req("USUBJID" %in% names(df))
      df %>%
        summarize(
          n = n_distinct(USUBJID),
          pct = round2(n / unique(N) * 100, 1)
        ) %>%
        pivot_longer(cols = c(n, pct)) %>%
        unite(temp, all_of(var), name) %>%
        pivot_wider(names_from = temp, values_from = value, values_fill = fill)
    }
    
    # Implement standard rounding of 5
    round2 <- function(x, n) {
      posneg <- sign(x)
      z <- abs(x) * 10^n
      z <- z + 0.5 + sqrt(.Machine$double.eps)
      z <- trunc(z)
      z <- z / 10^n
      z * posneg
    }
    
    # Calculate summary statistics
    summarize_stats <- function(df, group, var, header) {
      df <- df %>%
        group_by(.data[[group]]) %>%
        summarize(n = sum(!is.na(.data[[var]])), 
                  d_mean = mean(.data[[var]], na.rm = TRUE),
                  sd = sprintf("%.2f", sd(.data[[var]], na.rm = TRUE)), 
                  d_max = max(.data[[var]], na.rm = TRUE),
                  d_q3 = quantile(.data[[var]], probs = 0.75, type = 2, na.rm = TRUE), 
                  d_median = median(.data[[var]], na.rm = TRUE),
                  d_q1 = quantile(.data[[var]], probs = 0.25, type = 2, na.rm = TRUE), 
                  d_min = min(.data[[var]], na.rm = TRUE)) %>%
        mutate(d_max = na_if(d_max, -Inf),
               d_min = na_if(d_min, Inf),
               across(starts_with("d"), ~sprintf("%.1f", round2(., 1))),
               d_mean = ifelse(sd == "NA", "NA, NA", paste0(d_mean, "(", sd, ")")),
               d_q1 = paste0(d_q1, ", ", d_q3),
               d_min = paste0(d_min, ", ", d_max),) %>%
        select(-sd, -d_max, -d_q3) %>%
        rename(meansd = d_mean, 
               median = d_median, 
               q1q3 = d_q1, 
               range = d_min) %>%
        t() %>%
        data.frame() %>%
        suppressWarnings()
      df <- mutate(df, COL_1 = rownames(df), .before = 1)
      rownames(df) <- NULL
      df[1, ] <- c(header, rep("", ncol(df) - 1))
      df
    }
    
    # Overview table columns ---------------------------------------------------
    
    # Information from RTF tables
    table_info <- reactive({
      tables <- rv$rtf_list
      n <- length(tables)
      info <- data.frame(matrix(nrow = n, ncol = 3))
      for(i in 1:n) {
        t_i <- tables[[i]]
        
        # Extract Table Number, Title, and Run Date
        number <- grep("Table", t_i, value = TRUE) %>%
          sub(".*\n(?=Table)", "", ., perl = TRUE) %>% # All before "Table"
          sub("\n.*", "", .) # All after Table Number
        title <- grep("Table", t_i, value = TRUE) %>%
          sub(".*\\.\\d+\n", "", .) %>% # All before Table Number (inclusive)
          sub("\\s*\\|.*", "", .) # Ending spaces and vertical bar
        run_date <- grep("Run Date", t_i, ignore.case = TRUE, value = TRUE) %>%
          sub(".*Date: ", "", ignore.case = TRUE, .) %>% # All before "Date :" (inclusive)
          sub(" .*", "", .) %>% # All after Date
          as.Date("%d%b%Y") %>% # Convert string to Date
          as.character() # Convert to character string
        
        info[i, ] <- c(number, title, run_date)
      }
      info
    })
    
    # Vector of table names that are also valid
    table_names <- reactive({
      sort(intersect(input$file_rtf$name, paste0(valid_table_names, ".rtf")))
    })
    
    # List of primary tables
    primary_tables <- reactive({
      rtf_tables <- rv$rtf_list
      n <- length(rtf_tables)
      tbls <- vector("list", n)
      for(i in 1:n) {
        t_i <- rtf_tables[[i]]
        n_col <- stringr::str_count(t_i[4], "\\|")
        t_i2 <- t_i %>%
          tibble() %>%
          separate(1, paste0("V", 1:n_col), "\\|") %>%
          `[`(3:(length(t_i) - 2), ) %>%
          suppressWarnings()
        # names(t_i2) <- as.character(t_i2[1, ])
        names(t_i2) <- paste0("COL_", 1:ncol(t_i2))
        tbls[[i]] <- t_i2[-1, ] %>%
          filter(if_all(1, ~ !(. %in% c("", " ")))) # Remove rows with empty columns
        updateProgressBar(session, "file_progress", 
                          value = i, total = 2 * n)
      }
      table_names <- tools::file_path_sans_ext(table_names())
      names(tbls) <- table_names
      tbls
    })
    
    # List of validation tables
    validation_tables <- reactive({
      table_names <- tools::file_path_sans_ext(table_names())
      n <- length(table_names)
      tbls <- vector("list", n)
      tables <- sort(c("disposition", "baseline_demo", "follow_up", 
                       "orr_crude", "crr_crude", "os_crude"))
      n_prev <- 1
      n_next <- 0
      
      # Loop through each table type
      for(j in 1:length(tables)) {
        table <- tables[j]
        subtypes <- sub(paste0(table, "_"), "", grep(table, table_names, value = TRUE))
        n_cur <- length(subtypes)
        if(n_cur > 0) {
          n_next <- n_next + n_cur
          
          # Loop through each table subtype
          for(i in n_prev:n_next) {
            if(table %in% c("orr_crude", "crr_crude")) {
              tbls[[i]] <- rr_crude_tbl(subtypes[i - n_prev + 1], table)
            } else {
              tbls[[i]] <- do.call(paste0(table, "_tbl"), list(subtypes[i - n_prev + 1]))
            }
            updateProgressBar(session, "file_progress", 
                              value = n + i, total = 2 * n, 
                              title = "Creating validation tables")
          }
          n_prev <- n_prev + n_cur
        }
      }
      names(tbls) <- table_names
      
      # Apply same format to every table
      format_validation <- function(df, name) {
        names(df) <- paste0("COL_", 1:ncol(df))
        if(grepl("orr|crr", name)) {
          df
        } else {
          num_cols <- names(df)[-1]
          # Add spaces between open parentheses and numbers
          df %>%
            mutate(across(all_of(num_cols), 
                          ~ifelse(. == "", ., 
                                  gsub("\\((?=\\d{2}\\.)", "( ", ., perl = TRUE))),
                   across(all_of(num_cols), 
                          ~ifelse(. == "", ., 
                                  gsub("\\((?=\\d{1}\\.)", "(  ", ., perl = TRUE))),
                   across(everything(), ~ ifelse(is.na(.), "NA", .)))
        }
      }
      purrr::map2(tbls, names(tbls), format_validation)
    })
    
    # Compare primary and validation tables
    diffdf_compare <- function(primary, validation) {
      tbl <- primary %>%
        mutate(across(everything(), ~gsub("\\s+", "", .)))
      # names(tbl) <- paste0("COL_", 1:ncol(tbl))
      v_tbl <- validation %>%
        mutate(across(everything(), ~gsub("\\s+", "", .)),
               across(everything(), ~ ifelse(is.na(.), "NA", .)))
      # names(v_tbl) <- paste0("COL_", 1:ncol(v_tbl))
      # Don't compare first column
      diffdf::diffdf(tbl[, -1], v_tbl[, -1], suppress_warnings = TRUE)
    }
    
    # Find passing status of validation
    pass_status <- reactive({
      diffs <- purrr::map2(primary_tables(), validation_tables(), diffdf_compare)
      # If the diffdf message is "No issues were found!", the list has length 0
      purrr::map_lgl(diffs, ~length(.) == 0)
    })
    
    # Combine into data frame --------------------------------------------------
    complete_val_table <- eventReactive(input$run_button, {
      req(rv$rtf_list)
      n <- length(rv$rtf_list)
      
      # Ensure all necessary files are uploaded
      if(any(grepl("os_crude", table_names())) & is.null(rv$df_adtte)) {
        custom_alert("ADTTE file must be uploaded to validate OS tables.")
        req(rv$df_adtte)
      }
      if(any(grepl("rr_crude", table_names())) & is.null(rv$df_adrs)) {
        custom_alert("ADRS file must be uploaded to validate ORR and CRR tables.")
        req(rv$df_adrs)
      }
      
      # Initialize progress bar
      progressSweetAlert(session, "file_progress", 
                         value = 0, total = 2 * n,
                         display_pct = TRUE, title = "Reading files")
      
      df <- data.frame(table_info()[, 1:2], 
                       table_names(),
                       pass_status(),
                       table_info()[, 3])
      names(df) <- c("Table Number", "Table Title",
                     "Primary Output Table",
                     "Passing Status",
                     "Run Date")
      
      # Close progress bar
      closeSweetAlert()
      df
    })
    
    # Output overview table
    output$table_overview <- DT::renderDT({
      df <- complete_val_table()
      
      # Add pass status icons
      df[, 4] <- ifelse(
        df[, 4],
        as.character(icon("ok", class = "color-ok", lib = "glyphicon")),
        as.character(icon("remove", class = "color-remove", lib = "glyphicon"))
      )
      DT::datatable(df,
                    rownames = FALSE,
                    filter = "top",
                    escape = FALSE, 
                    selection = "single") %>%
        DT::formatStyle(4, textAlign = "center")
    })
    
    # Appearance of gt table
    gt_tableval <- function(df) {
      gt::gt(df, rownames_to_stub = TRUE) %>%
        gt::cols_align(
          align = "right",
          columns = names(df)[-1]
        )
    }
    
    # Comparison modal dialog --------------------------------------------------
    observeEvent(input$table_overview_cell_clicked, {
      info <- input$table_overview_cell_clicked
      
      # Show modal only if "Passing Status" cell is chosen
      req(info)
      req(info$col == 3)
      
      # Tables from selected row
      p_table <- primary_tables()[[info$row]]
      v_table <- validation_tables()[[info$row]]
      
      # Diffdf differences
      table_comp <- diffdf_compare(p_table, v_table)
      
      # Apply gt formatting
      gt_p_table <- gt_tableval(p_table)
      gt_v_table <- gt_tableval(v_table)
      
      # Find locations of differences in validation table
      if(length(table_comp) != 0) {
        vardiff <- table_comp[grepl("VarDiff", names(table_comp))]
        diff_loc <- data.frame(matrix(nrow = 0, ncol = 2))
        names(diff_loc) <- c("VARIABLE", "..ROWNUMBER..")
        for(i in 1:length(vardiff)) {
          table_comp_i <- vardiff[[i]][, 1:2]
          diff_loc <- rbind(diff_loc, table_comp_i)
        }
        
        # Highlight cells with differences
        gt_v_table <- gt_v_table %>%
          gt::tab_style(
            style = gt::cell_fill(color = "yellow"),
            locations = gt::cells_body(
              columns = diff_loc[[1]],
              rows = diff_loc[[2]]
            )
          )
      }
      # Show modal dialog
      showModal(modalDialog(
        fluidRow(column(12, renderPrint(table_comp))),
        fluidRow(column(6, align = "center", p(strong("Primary Table"))),
                 column(6, align = "center", p(strong("Validation Table")))),
        fluidRow(column(6, gt::render_gt(gt_p_table)),
                 column(6, gt::render_gt(gt_v_table))),
        title = "Differences",
        footer = modalButton("Close"),
        size = "xl",
        easyClose = TRUE
      ))
    })
    
    # Download -----------------------------------------------------------------
    # With report format
    report <- reactive({
      cols <- names(complete_val_table())
      tbl <- complete_val_table() %>%
        reporter::create_table(width = 10) %>%
        reporter::define(cols[4], standard_eval = TRUE, align = "left")
      reporter::create_report(font = "Arial") %>%
        reporter::set_margins(0.5, 0.5, 0.5, 0.5) %>%
        reporter::add_content(tbl)
    })
    
    mod_download_server("download", report,
                        filename = "validation", format = "table")
  })
}
