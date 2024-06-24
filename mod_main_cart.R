# CAR-T Registry studies: reports and patient profiles

# Module UI function
mod_main_cart_ui <- function(id) {
  ns <- NS(id)
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
            column(6, fileInput(ns("file_main"), "Main data (.sas7bdat)", 
                                accept = ".sas7bdat")),
            column(6, fileInput(ns("file_dict"), "Data Dictionary (Excel)", 
                                accept = c(".xls", ".xlsx")))
          ),
          fluidRow(
            column(6, fileInput(ns("file_oos"), 
                                "Out-of-Spec and Join ID Information (.csv or Excel)", 
                                accept = c(".xls", ".xlsx", ".csv"))),
            column(6, pickerInput_vars(ns("columns"), "Column select"))
          ),
          fluidRow(
            column(3, pickerInput(ns("subj"), "Subject ID", NULL,
                                  options = list(virtualScroll = TRUE))),
            column(3, pickerInput(ns("inf"), "Infusion Number", NULL,
                                  options = list(virtualScroll = TRUE))),
            column(3, pickerInput(ns("br_time"), 
                                  "Time from CT to Best Response", NULL,
                                  options = list(virtualScroll = TRUE))),
            column(3, pickerInput(ns("br_val"), 
                                  "Best Response since CT Infusion", NULL,
                                  options = list(virtualScroll = TRUE)))
          ),
          fluidRow(
            column(12, align = "center", 
                   radioGroupButtons(ns("study"), "Study",
                                     choices = c("BB2121", "JCAR017")))
          )
        ),
        tabBox(
          width = 12,
          tabPanel("Main", 
                   column(12, align = "center", htmlOutput(ns("subjects_data"))),
                   DTOutput_div(ns("table_main"))
          ),
          tabPanel("Data Dictionary", DTOutput_div(ns("table_dict"))),
          tabPanel("OOS/Join ID", DTOutput_div(ns("table_oos")))
        )
      ),
      
      # Report -----------------------------------------------------------------
      tabPanel(
        "Report",
        shinydashboardPlus::box(
          width = 12,
          title = "Report",
          dropdownMenu = shinydashboardPlus::boxDropdown(
            icon = shiny::icon("download"),
            shinydashboardPlus::boxDropdownItem(downloadLink(ns("download_report"), "PDF"))
          ),
          DT::DTOutput(ns("report_table")) %>% add_spinner(),
          br(),
          plotly::plotlyOutput(ns("report_plot"), height = "800px") %>% add_spinner()
        )
      ),
      
      # Subject Info -----------------------------------------------------------
      tabPanel(
        "Subject Info",
        shinydashboardPlus::box(
          width = 12,
          title = "Subject Info",
          dropdownMenu = shinydashboardPlus::boxDropdown(
            icon = shiny::icon("download"),
            shinydashboardPlus::boxDropdownItem(downloadLink(ns("download_info"), "PDF"))
          ),
          fluidRow(
            column(4, selectizeInput(ns("profile_subj"), "Subject ID", NULL)),
            column(4, selectInput(ns("profile_inf"), "Infusion Number", NULL))
          ),
          column(12, align = "center", htmlOutput(ns("subject_profile"))),
          fluidRow(
            column(5, DT::DTOutput(ns("patient_profile")) %>% add_spinner()),
            column(7, DT::DTOutput(ns("profile_event_table")) %>% add_spinner())
          ),
          br(),
          uiOutput(ns("profile_event_plot_ui")) %>% add_spinner()
        )
      ),
      
      # EAP Profile ------------------------------------------------------------
      tabPanel(
        "EAP Profile",
        shinydashboardPlus::box(
          width = 12,
          title = "EAP Patient Profile",
          sidebar = shinydashboardPlus::boxSidebar(
            id = ns("eap_sidebar"),
            width = 33,
            br(),
            column(12, align = "center",
                   radioGroupButtons(ns("download_format"), "File Format",
                                     choices = c("RTF", "PDF"))),
            column(12, hr()),
            column(12, align = "center", h4("Current selected subject")),
            column(12, align = "center", 
                   downloadButton(ns("download_eap"), "Download")),
            column(12, hr()),
            column(12, align = "center", h4("Mass Download")),
            column(12, pickerInput_vars(ns("eap_download_subj"), "Subjects")),
            column(12, align = "center", 
                   downloadButton(ns("download_eap_all"), "Download")),
            icon = shiny::icon("download")
          ),
          fluidRow(
            column(4, selectizeInput(ns("eap_subj"), "Subject ID", NULL))
          ),
          column(12, align = "center", htmlOutput(ns("subject_eap"))),
          uiOutput(ns("eap_profile_ui")) %>% add_spinner()
        ) %>% {
          htmltools::tagQuery(.)$
            find(paste0("#", ns("eap_sidebar")))$
            removeAttrs("data-original-title")$
            addAttrs(`data-original-title`="Download")$
            allTags()
        }
      )
      
    )
  )
}

# Module server function
mod_main_cart_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues()
    
    # Source files
    source_files <- list.files("./extra", pattern = "^cart_.*\\.R$",
                               full.names = TRUE)
    for (i in seq_along(source_files)) {
      source(source_files[i], local = TRUE)
    }
    
    # Upload files
    observe({
      req(input$file_main)
      rv$df_main <- upload_file(input$file_main)
    })
    observe({
      file <- input$file_dict
      req(file)
      check_file(file, c("xls", "xlsx"), "Please upload an Excel file.")
      rv$df_dict <- readxl::read_excel(file$datapath, sheet = 2)
    })
    observe({
      file <- input$file_oos
      req(file)
      ext <- tools::file_ext(file$datapath)
      check_file(file, c("xls", "xlsx", "csv"), "Please upload an Excel or CSV file.")
      if(ext %in% c("xls", "xlsx")) {
        df <- readxl::read_excel(file$datapath)
      } else if(ext == "csv") {
        df <- readr::read_csv(file$datapath, show_col_types = FALSE)
      }
      rv$df_oos <- df
    })
    
    # Check file is of valid type
    check_file <- function(file, filetypes, message) {
      req(file)
      ext <- tools::file_ext(file$datapath)
      validate(
        need(ext %in% filetypes, message)
      )
    }
    
    # Reset fileInputs
    observeEvent(input$reset_file, {
      rv$df_main <- NULL
      rv$df_dict <- NULL
      rv$df_oos <- NULL
      shinyjs::reset("file_main")
      shinyjs::reset("file_dict")
      shinyjs::reset("file_oos")
    })
    
    # Update variables
    observe({
      req(rv$df_main)
      all_names <- names(rv$df_main)
      # Include labels as subtext
      updatePickerInput(session, "subj", choices = all_names, 
                        choicesOpt = list(subtext = cart_var_labels()),
                        selected = grep_var("^did_", all_names))
      updatePickerInput(session, "inf", choices = all_names,
                        choicesOpt = list(subtext = cart_var_labels()),
                        selected = grep_var("inf_num$", all_names))
      updatePickerInput(session, "br_time", choices = all_names,
                        choicesOpt = list(subtext = cart_var_labels()),
                        selected = grep_var("^intct.*brsp", all_names))
      updatePickerInput(session, "br_val", choices = all_names,
                        choicesOpt = list(subtext = cart_var_labels()),
                        selected = grep_var("^ct_.*bresp_l1", all_names))
    })
    observeEvent(input$file_main, {
      filename <- input$file_main$name
      if(grepl("bb", filename, ignore.case = TRUE)) {
        updateRadioGroupButtons(session , "study", selected = "BB2121")
      } else if(grepl("jcar", filename, ignore.case = TRUE)) {
        updateRadioGroupButtons(session , "study", selected = "JCAR017")
      }
    })
    
    # Output number of subjects
    output$subjects_data <- renderUI({
      req(rv$df_main)
      req(input$subj)
      subjects_HTML(subjects_fn(rv$df_main, input$subj))
    })
    
    # Column names
    observe({
      choices <- names(rv$df_main)
      updatePickerInput(session, "columns", choices = choices, selected = choices,
                        choicesOpt = list(subtext = cart_var_labels()))
    })
    
    # Labels for each variable
    cart_var_labels <- reactive(var_labels(rv$df_main))
    
    # Data tables
    output$table_main <- DT::renderDT({
      req(rv$df_main)
      req(length(input$columns) > 0)
      df <- select(rv$df_main, any_of(input$columns))
      
      # Add labels to column names
      labels <- cart_var_labels()[names(df)]
      combnames <- purrr::map2_chr(names(df), labels, ~paste(.x, .y))
      
      DT::datatable(df,
                    extensions = "ColReorder",
                    colnames = combnames,
                    options = list(pageLength = 10,
                                   colReorder = list(realtime = FALSE)),
                    filter = "top")
    })
    output$table_dict <- DT::renderDT({
      req(rv$df_dict)
      data_table_fn(rv$df_dict)
    })
    output$table_oos <- DT::renderDT({
      req(rv$df_oos)
      data_table_fn(rv$df_oos)
    })
    
    # Main data
    main <- reactive({
      req(input$subj %in% names(rv$df_main))
      # When variable labels are kept, EAP download fails
      process_file(rv$df_main, input$subj, label.rm = TRUE)
    })
    
    # Used in the other CAR-T tabs ---------------------------------------------
    
    # Data Dictionary with NA values replaced
    complete_dict <- reactive(zoo::na.locf(rv$df_dict))
    
    # Validate that required variables exist
    validate_var <- function(vars, data) {
      for(x in vars) {
        validate(
          need(x %in% names(data), 
               paste("The variable", x, "must exist in the uploaded data.")
          )
        )
      }
    }
    
    # Substitute values with labels from data dictionary for the given variables
    sub_datadict_label <- function(vars, data, dict = complete_dict()) {
      for(i in seq_along(vars)) {
        var <- vars[i]
        sub_dict <- dict %>%
          filter(Variable == var) %>%
          select(Value, Label)
        for(i in 1:nrow(sub_dict)) {
          data[[var]][data[[var]] == sub_dict[[i, "Value"]]] <- sub_dict[[i, "Label"]]
        }
      }
      data
    }
    
  })
}
