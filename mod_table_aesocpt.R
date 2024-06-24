# Safety table: AE_SOC_PT

# Module UI function
mod_table_aesocpt_ui <- function(id) {
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
        column(6, fileInput(ns("file_adae"), "Choose an ADAE file", 
                            accept = file_accept)),
      ),
      fluidRow(
        column(3, selectInput(ns("subj"), "Subject ID", NULL)),
        column(3, selectInput(ns("bodsys"), "System Organ Class", NULL)),
        column(3, selectInput(ns("decod"), "Dictionary Term", NULL))
      ),
      fluidRow(
        column(3, selectInput(ns("trt"), "Treatment", NULL)),
        column(3, selectInput(ns("trtn"), "Treatment (N) (optional)", "None",
                              selected = "None")),
        column(6, pickerInput_vars(ns("trt_sel"), "Treatment Select"))
      )
    ),
    shinydashboardPlus::box(
      width = 12,
      title = "Additional Options",
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(12,
               p(strong("Table"), align = "center"),
               textAreaInput(ns("table_title"), "Titles",
                             resize = "vertical"),
               textAreaInput(ns("table_footnote"), "Footnotes",
                             rows = 4,
                             resize = "vertical")
        )
      )
    ),
    shinydashboardPlus::box(
      width = 9,
      title = "Safety Table",
      dropdownMenu = mod_download_ui(ns("download"), format = "table"),
      gt::gt_output(ns("table")) %>% add_spinner()
    ),
    mod_sub_varfilter_ui(ns("filter"))
  )
}

# Module server function
mod_table_aesocpt_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues()
    
    # Upload files
    observe({
      req(input$file_adsl)
      rv$df_adsl <- upload_file(input$file_adsl)
    })
    observe({
      req(input$file_adae)
      rv$df_adae <- upload_file(input$file_adae)
    })
    
    # Reset fileInput
    observeEvent(input$reset_file, {
      rv$df_adsl <- NULL
      rv$df_adae <- NULL
      shinyjs::reset("file_adsl")
      shinyjs::reset("file_adae")
    })
    
    # Update variables
    observe({
      choices_adsl <- names(rv$df_adsl)
      choices_adae <- names(rv$df_adae)
      choices <- intersect(choices_adsl, choices_adae)
      trt_choices <- grep("^trt01", choices, TRUE, value = TRUE)
      
      updateSelectInput(session, "subj", choices = choices,
                        selected = grep_var("subj", choices))
      updateSelectInput(session, "bodsys", choices = choices_adae,
                        selected = grep_var("aebodsys", choices_adae))
      updateSelectInput(session, "decod", choices = choices_adae,
                        selected = grep_var("aedecod", choices_adae))
      updateSelectInput(session, "trt", choices = trt_choices, 
                        selected = grep_var("trt01a", trt_choices))
      updateSelectInput(session, "trtn", choices = c("None", trt_choices),
                        selected = ifelse_var(".*n$", trt_choices))
    })
    observe({
      req(input$trt)
      choices <- sort(unique(rv$df_adsl[[input$trt]]))
      updatePickerInput(
        session, "trt_sel", choices = choices, 
        selected = setdiff(choices, c("NOT TREATED", "SCREEN FAILURE", "NOT ASSIGNED"))
      )
    })
    
    # Main data
    main <- reactive({
      req(rv$df_adsl)
      req(rv$df_adae)
      req(input$subj %in% names(rv$df_adsl))
      req(input$subj %in% names(rv$df_adae))
      adsl <- process_file(rv$df_adsl)
      adae <- process_file(rv$df_adae)
      inner_join(adsl, adae, by = input$subj, suffix = c("", ".y"), 
                 multiple = "all") %>%
        select(-ends_with(".y"))
    })
    
    # Apply variable filters
    df_filter <- mod_sub_varfilter_server("filter", main)
    
    # Prepare data frame
    ae_df <- reactive({
      req(all(c(input$subj, input$bodsys,
                input$decod, input$trt) %in% names(df_filter())))
      df <- df_filter() %>%
        drop_na(input$trt)
      
      # Order treatments
      df <- order_trt(df, input$trt, input$trtn)
      
      df %>%
        select(trt, all_of(c(input$subj, input$bodsys, input$decod))) %>%
        filter(trt %in% input$trt_sel)
    })
    
    # Big N for each treatment
    ae_N <- reactive({
      req(input$trt)
      rv$df_adsl %>%
        group_by(.data[[input$trt]]) %>%
        summarize(tot = n()) %>%
        rename(trt = input$trt)
    })
    
    # AE_PT_SOC table ----------------------------------------------------------
    ae_table <- reactive({
      main <- ae_df()
      
      # AE SOC summary
      ## 1. total number by subjects, AESOC:
      aesoc0 <- main %>%
        group_by(trt, .data[[input$subj]], 
                 .data[[input$bodsys]]) %>%
        summarize(N = n())
      aesoc1 <- aesoc0 %>%
        group_by(trt, .data[[input$bodsys]]) %>%
        summarize(N = n())
      aesoc2 <- aesoc1 %>% 
        group_by(trt) %>%
        arrange(trt, desc(N), .by_group = TRUE) %>%
        mutate(group = .data[[input$bodsys]], grpxord = row_number())
      
      bign <- ae_N()
      
      # Choose the largest population
      maxvar <- bign %>% 
        filter(tot == max(bign$tot))
      seq4soc <- aesoc2 %>% 
        filter(trt == maxvar$trt) %>%
        ungroup() %>%
        select(input$bodsys, grpxord)
      aesoc <- aesoc2 %>%
        select(-grpxord) %>% 
        left_join(seq4soc, by = input$bodsys) %>%
        mutate(grpxord_ = -1)
      
      ## 2. Total number by AESOC:
      aesoccnt0 <- main %>% 
        group_by(trt, .data[[input$bodsys]]) %>% 
        summarize(N = n())
      aesoccnt <- aesoccnt0 %>% 
        left_join(seq4soc, by = input$bodsys) %>% 
        mutate(grpxord_ = 0)
      
      # AE PT summary
      aept0 <- main %>% 
        group_by(trt, .data[[input$subj]], 
                 .data[[input$bodsys]], .data[[input$decod]]) %>%
        rename(group = input$decod) %>%
        summarize(N = n())
      aept1 <- aept0 %>% 
        group_by(trt, .data[[input$bodsys]], 
                 group) %>% 
        summarize(N = n())
      aept <- left_join(aept1, seq4soc, by = input$bodsys)
      seq4pt <- aept %>% 
        filter(trt == maxvar$trt) %>% 
        arrange(grpxord, desc(N)) %>%
        mutate(grpxord1 = row_number()) %>%
        ungroup() %>%
        select(group, grpxord1)
      
      # Merge SOC number
      fin0 <- rbind(aesoc, aesoccnt, aept) %>% 
        group_by(trt, grpxord) %>% 
        arrange(trt, grpxord, desc(N), .by_group = TRUE)
      
      # AE summary table
      left_join(fin0, seq4pt, by = "group") %>% 
        left_join(bign, by = "trt") %>%
        ungroup() %>%
        mutate(across(grpxord_, as.integer)) %>%
        mutate(
          pcnt = case_when(
            is.na(group) == FALSE ~ round(N * 100 / tot, digits = 1),
            TRUE ~ 0
          ),
          val = case_when(
            is.na(group) == FALSE ~ paste0(N, " (", format(pcnt, digits = 1, nsmall = 1), ")"),
            TRUE ~ paste0(N, "      ")
          ),
          col1 = case_when(
            group == .data[[input$bodsys]] ~ .data[[input$bodsys]],
            is.na(group) == TRUE ~ paste0("  Overall total number of ", .data[[input$bodsys]]),
            TRUE ~ paste0("  ", group)
          ),
          grpxord2 = case_when(grpxord1 >= 1 ~ grpxord1,
                               TRUE ~ grpxord_)
        ) %>%
        select(col1, trt, val, grpxord, grpxord2) %>% 
        group_by(grpxord, grpxord2, col1) %>%
        pivot_wider(names_from = trt, values_from = val, names_glue = "{trt}\nn (%)") %>% 
        ungroup() %>%
        arrange(grpxord, grpxord2) %>%
        select(-grpxord, -grpxord2)
    })
    
    # AE_PT_SOC gt table
    output$table <- gt::render_gt({
      gt_table_format(gt::gt(ae_table()), ae_table(),
                      input$table_title, input$table_footnote) %>%
        gt::tab_style(
          gt::cell_text(whitespace = "pre-wrap"),
          list(gt::cells_body(), gt::cells_column_labels())
        ) %>%
        gt::cols_label(col1 = gt::html("System Organ Class<br>  Preferred Term"))
    })
    
    # Download -----------------------------------------------------------------
    # Add report format
    report <- reactive({
      df <- ae_table()
      
      # Ensure field width is consistent
      max_width <- apply(sapply(df[, -1], nchar, keepNA = FALSE), 2, max)
      df[, -1] <- purrr::map2(df[, -1], max_width, ~formatC(.x, .y))
      
      df %>%
        rename("System Organ Class\n  Preferred Term" = col1) %>%
        report_table(input$table_title, input$table_footnote)
    })
    
    mod_download_server("download", report,
                        filename = "aesocpt", format = "table")
    
  })
}
