# Kaplan-Meier (KM) survival plot

# Module UI function
mod_graph_km_ui <- function(id) {
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
      fluidRow(
        column(3, selectInput(ns("subj"), "Subject ID", NULL)),
        column(3, selectInput(ns("study"), "Study ID", NULL)),
        column(3, selectInput(ns("param"), "Parameter", NULL)),
        column(3, selectInput(ns("aval"), "Analysis Value", NULL)),
      ),
      fluidRow(
        column(3, selectInput(ns("censor"), "Censor", NULL))
      )
    ),
    shinydashboardPlus::box(
      width = 12,
      title = "Graph",
      fluidRow(
        column(3, selectInput(ns("param_sel"), "Parameter Select", NULL)),
        column(3, radioGroupButtons(ns("time_break"), "Time Axis Breaks", 
                                    c(3, 6, 12)))
      ),
      plotOutput(ns("plot")) %>% add_spinner()
    )
  )
}

# Module server function
mod_graph_km_server <- function(id) {
  
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
      adsl <- names(rv$df_adsl)
      adtte <- names(rv$df_adtte)
      all <- intersect(adsl, adtte)
      updateSelectInput(session, "subj", choices = all,
                        selected = grep_var("subj", all))
      updateSelectInput(session, "study", choices = all,
                        selected = grep_var("study", all))
      updateSelectInput(session, "param", choices = adtte,
                        selected = grep_var("paramcd", adtte))
      updateSelectInput(session, "aval", choices = adtte,
                        selected = grep_var("aval$", adtte))
      updateSelectInput(session, "censor", choices = adtte,
                        selected = grep_var("cnsr", adtte))
    })
    observe({
      choices <- unique(rv$df_adtte[[input$param]])
      updateSelectInput(session, "param_sel", choices = choices)
    })
    
    # Combined data frame
    df_km <- reactive({
      req(input$param_sel)
      req(all(c(input$subj, input$study) %in% names(rv$df_adsl)))
      req(all(c(input$subj, input$study,
                input$aval, input$censor) %in% names(rv$df_adtte)))
      adsl <- process_file(rv$df_adsl) %>%
        select(input$subj, input$study)
      adtte <- process_file(rv$df_adtte) %>%
        filter(.data[[input$param]] == input$param_sel) %>%
        select(input$subj, input$study, input$aval, input$censor)
      df <- inner_join(adsl, adtte, by = c(input$subj, input$study))
      names(df) <- c("subjid", "StudyID", "aval", "cnsr")
      df
    })
    
    # Survival plot ------------------------------------------------------------
    surv_plot <- reactive({
      req(input$param_sel)
      comb <- df_km()
      # Note: do.call is necessary for survfit to work inside the app
      fit <- do.call(survival::survfit, 
                     list(formula = survival::Surv(aval, 1 - cnsr) ~ StudyID,
                          conf.type = "log-log", data = comb))
      survminer::ggsurvplot(
        fit,
        censor = TRUE,
        break.y.by = 0.1,
        xlab = "Time (Months)",
        ylab = "Survival Probability",
        xlim = c(-1, max(comb$aval)),
        break.time.by = as.numeric(input$time_break),
        surv.median.line = "hv",
        legend = "top",
        legend.title = "",
        risk.table = TRUE,
        risk.table.title = "Number of Subjects at Risk",
        risk.table.y.text = FALSE
      )
    })
    
    # Render plot
    output$plot <- renderPlot(surv_plot())
  })
}
