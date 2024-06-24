library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(purrr)
####
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(DT)
library(ggplot2)
library(ggtext)
library(plotly)
library(lubridate)
library(rlang)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(readxl)
library(writexl)
library(fresh)
library(visR)
library(survival)
library(survminer)
library(tableone)
library(survey)
library(PropCIs)
library(gt)
library(diffdf)
library(reporter)
library(striprtf)
library(zoo)
library(glue)
library(data.table)
library(stringr)
library(sjlabelled)
library(ratesci)
library(colourpicker)
library(plotly)

# Define UI
ui <- shinydashboardPlus::dashboardPage(
  title = "Sherlock",
  shinydashboardPlus::dashboardHeader(
    title = div(img(src = "img/logo.png", height = "30px", 
                    style = "filter:invert(100%)"), "Sherlock"),
    controlbarIcon = NULL
  ),
  shinydashboardPlus::dashboardSidebar(
    minified = FALSE,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = shiny::icon("house")),
      hr_menu(),
      
      ## Menu ------------------------------------------------------------------
      menuItem(
        "RWE Studies",
        icon = shiny::icon("globe"),
        menuSubItem("Lab Assessments", tabName = "rwe_lab", icon = shiny::icon("vial")),
        menuSubItem("Cohort Selection", tabName = "rwe_cohort", icon = shiny::icon("users")),
        menuSubItem("Demographic Table", tabName = "rwe_demo", icon = shiny::icon("table")),
        menuSubItem("Line of Therapy", tabName = "rwe_lot", icon = shiny::icon("chart-gantt")),
        menuSubItem("Propensity Score", tabName = "rwe_ps", icon = shiny::icon("chart-area")),
        menuItem(
          "Graph Builder", 
          icon = shiny::icon("chart-simple"),
          menuSubItem("Survival", tabName = "rwe_km", icon = shiny::icon("chart-line"))
        ),
        menuSubItem("Table Validation", tabName = "rwe_tv", icon = shiny::icon("table"))
      ),
      menuItem("CAR-T Registry", tabName = "cart", icon = shiny::icon("list")),
      menuItem(
        "Exploration & Analytics", 
        icon = shiny::icon("compass"),
        menuItem(
          "Data Exploration",
          icon = shiny::icon("table-list"),
          menuSubItem("BDS Data", tabName = "de_vert", icon = shiny::icon("ruler-vertical")),
          # menuItem("Occurrence Data", tabName = "de_vert_occ", icon = shiny::icon("ruler-vertical")),
          menuSubItem("Subject-Level Data", tabName = "de_horiz", icon = shiny::icon("ruler-horizontal"))
        ),
        menuItem(
          "Table Builder", 
          icon = shiny::icon("table"),
          menuSubItem("Demographic", tabName = "de_demo", icon = NULL),
          menuItem(
            "Safety",
            menuSubItem("AE_SOC_PT", tabName = "de_aesocpt", icon = NULL)
          ),
          menuItem(
            "Efficacy",
            menuSubItem("Summary by Visit", tabName = "de_icf", icon = NULL),
            menuSubItem("Survival", tabName = "t_surv", icon = NULL)
            # menuSubItem("Survival", tabName = "de_surv", icon = NULL),
            # menuSubItem("Response", tabName = "de_resp", icon = NULL)
          )
        ),
        menuItem(
          "Graph Builder", 
          icon = shiny::icon("chart-simple"),
          menuSubItem("Correlation", tabName = "de_dualy", icon = shiny::icon("chart-line")),
          menuSubItem("Survival", tabName = "de_km", icon = shiny::icon("chart-line")),
          menuSubItem("Waterfall", tabName = "g_waterfall", icon = shiny::icon("chart-line")),
          menuSubItem("Forest (1 group)", tabName = "g_forest1", icon = shiny::icon("chart-line")),
          menuSubItem("Forest (2 group)", tabName = "g_forest2", icon = shiny::icon("chart-line")),
          menuSubItem("Tepee Plot", tabName = "g_tepee", icon = shiny::icon("chart-line")),
          menuSubItem("Frequency Bar", tabName = "g_bar", icon = shiny::icon("chart-line")),
          menuSubItem("Sankey Plot", tabName = "g_sankey", icon = shiny::icon("chart-line"))
        )
      ),
      hr_menu(),
      menuItem("Contact Us", tabName = "contact", icon = shiny::icon("address-card"))
    ),
    uiOutput("style_tag")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    fresh::use_theme(mytheme),
    shinyWidgets::useSweetAlert(),
    
    # CSS modifications
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # CSS that does not work in tags$head
    tags$style("
      /* Align tabsetPanel to box edge */
      .tabbable .nav {
        padding-left: 14px;
      }
    "),
    
    ## Tabs --------------------------------------------------------------------
    tabItems(
      
      home_page("home"),
      
      # Modules
      mod_main_lab_ui("rwe_lab"),
      mod_main_cohort_ui("rwe_cohort"),
      mod_table_demo_ui("rwe_demo"),
      mod_main_lot_ui("rwe_lot"),
      mod_main_ps_ui("rwe_ps"),
      mod_graph_km_ui("rwe_km"),
      mod_main_tableval_ui("rwe_tv"),
      mod_main_cart_ui("cart"),
      
      mod_main_lab_ui("de_vert", is.gen = TRUE),
      mod_main_cohort_ui("de_horiz", is.gen = TRUE),
      
      mod_graph_dualy_ui("de_dualy"),
      mod_graph_km_ui("de_km"),
      
      mod_table_demo_ui("de_demo"),
      mod_table_aesocpt_ui("de_aesocpt"),
      mod_table_icf_ui("de_icf"),
      mod_table_surv_crude_ui("t_surv"),
      
      mod_graph_waterfall_ui("g_waterfall"),
      mod_graph_forest1_ui("g_forest1"),
      mod_graph_forest2_ui("g_forest2"),
      mod_graph_tepee_ui('g_tepee'),
      mod_graph_bar_ui('g_bar'),
      mod_graph_sankey_ui('g_sankey'),
      
      contact_page("contact")
    )
  ),
  skin = "blue-light"
)
