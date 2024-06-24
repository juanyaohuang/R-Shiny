# Define server logic
server <- function(input, output, session) {
  
  options(
    shiny.maxRequestSize = 15000 * 1024^2, # Maximum file size limit: 15GB
    dplyr.summarise.inform = FALSE,        # Suppress extraneous messages
    DT.options = list(pageLength = 25,
                      searchHighlight = TRUE,
                      scrollX = TRUE)
  )
  
  # Update top padding based on selected tab (zero on home page)
  output$style_tag <- renderUI({
    if(input$tabs == "home") {
      tags$head(tags$style(HTML("
        .content{padding-top:0px}
      ")))
    } else {
      tags$head(tags$style(HTML("
        .content{padding-top:15px}
      ")))
    }
  })
  
  # Modules
  mod_main_lab_server("rwe_lab")
  mod_main_cohort_server("rwe_cohort")
  mod_table_demo_server("rwe_demo")
  mod_main_lot_server("rwe_lot")
  mod_main_ps_server("rwe_ps")
  mod_graph_km_server("rwe_km")
  mod_main_tableval_server("rwe_tv")
  mod_main_cart_server("cart")
  
  mod_main_lab_server("de_vert")
  mod_main_cohort_server("de_horiz", is.gen = TRUE)
  
  mod_graph_dualy_server("de_dualy")
  mod_graph_km_server("de_km")
  
  mod_table_demo_server("de_demo")
  mod_table_aesocpt_server("de_aesocpt")
  mod_table_icf_server("de_icf")
  mod_table_surv_crude_server("t_surv")

  mod_graph_waterfall_server("g_waterfall")
  mod_graph_forest1_server("g_forest1")
  mod_graph_forest2_server("g_forest2")
  mod_graph_tepee_server('g_tepee')
  mod_graph_bar_server('g_bar')
  mod_graph_sankey_server('g_sankey')
  
}
