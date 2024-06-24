# UI functions

# Create the theme
mytheme <- fresh::create_theme(
  fresh::adminlte_color(light_blue = "#739aa9"),
  fresh::adminlte_global(content_bg = "#dae2e7"),
  fresh::adminlte_sidebar(width = "260px")
)

# Default acceptable file formats
file_accept <- c(".sas7bdat", ".xls", ".xlsx", ".csv", ".rds")

# Display text with superscript "?" link (for bsPopover)
help_link <- function(text, id = NULL) {
  div(text, div(HTML("<sup><a>?</a></sup>"), id = id, style = "float:right"))
}

# Horizontal line with no margins for the sidebar menu
hr_menu <- function(color = "#ccc") {
  hr(style = paste("margin-top:0px; margin-bottom:0px; border-top:1px solid",
                   color, "; width:95%"))
}

# boxDropdown for resetting fileInputs
reset_dropdown <- function(id) {
  shinydashboardPlus::boxDropdown(
    icon = shiny::icon("trash"),
    shinydashboardPlus::boxDropdownItem(actionLink(id, "Clear files"))
  )
}

# pickerInput with preselected options
pickerInput_vars <- function(inputId, label = "Select Variables") {
  pickerInput(inputId, label, choices = NULL, multiple = TRUE,
              options = list(`actions-box` = TRUE,
                             `live-search` = TRUE,
                             `selected-text-format` = "count > 5"))
}


pickerInput_multi <- function(inputId, label = "Select Variables",
                              choices = NULL, selected = NULL) {
  pickerInput(inputId, label, choices = choices, selected = selected,
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE,
                                      selectedTextFormat = "count > 5"))
}


# Loading spinner
add_spinner <- function(ui_element) {
  shinycssloaders::withSpinner(ui_element, type = 8, color = "#739aa9",
                               proxy.height = 200)
}

# Tables with monospace font
DTOutput_div <- function(outputId) {
  div(DT::DTOutput(outputId) %>% add_spinner(), 
      style = "font-family:monospace; font-size:small")
}

# For use in modules -----------------------------------------------------------

# Additional options box for tables
table_option_box <- function(ns, options = NULL) {
  shinydashboardPlus::box(
    width = 12,
    title = "Additional Options",
    collapsible = TRUE,
    collapsed = TRUE,
    fluidRow(
      column(12,
             checkboxGroupInput(ns("table_stats"), 
                                "Numeric statistics",
                                c("n", "Mean (SD)", "Median", 
                                  "Q1, Q3", "Min, Max"),
                                selected = c("n", "Mean (SD)", "Median", 
                                             "Q1, Q3", "Min, Max")),
             hr(style = "border-top:1px solid #777; margin-bottom:10px"),
             options,
             p(strong("Table"), align = "center"),
             textAreaInput(ns("table_title"), "Titles",
                           resize = "vertical"),
             textAreaInput(ns("table_footnote"), "Footnotes",
                           rows = 4,
                           resize = "vertical")
      )
    )
  )
}

# Static UI pages --------------------------------------------------------------

# Organize home page features
feature <- function(left, right) {
  fluidRow(
    column(3, align = "right", p(strong(left, style = "color: #5c8797"))),
    column(9, p(right), style = "border-left:1px solid #eee")
  )
}

# Home page for the application
home_page <- function(id) {
  tabItem(
    id,
    fluidRow(
      column(7,
             offset = 1,
             h1(strong("Sherlock:"), "Real-World Data Visualization and Beyond"),
             hr(style = "border-top:1px solid"),
             tags$ul(
               tags$li(
                 h3("Use R/Shiny to explore data visually and dynamically",
                    "with intuitive graphs and summary results.")
               ),
               tags$li(
                 h3("Intended to faciliate data processing and",
                    "analysis in RWE studies.")
               ),
               tags$li(
                 h3("Contains additional tools for general data exploration",
                    "and analytics.")
               )
             ),
             style = "color:#000;padding-top:50px;padding-bottom:50px"
      ),
      column(3, img(src = "img/logo_text.png", height = "300px"), 
             style = "padding-top:50px;padding-bottom:50px"),
      style = paste0("background-image:url('img/bg.jpg');
                         background-repeat:no-repeat;
                         background-position:center center;
                         -webkit-background-size:cover"),
    ),
    br(),
    tabBox(
      width = 12,
      tabPanel(
        "RWE Features",
        feature("Overview",
                "Display subject level records."),
        feature("Confounder Selection",
                paste("Provide summary of missingness percentage",
                      "by assessment and data source.")),
        feature("Unit Conversion",
                paste("List all the units for each lab assessment,",
                      "and convert multiple units to one unique unit",
                      "for same lab assessment.")),
        feature("Outlier Determination",
                "Provide summary statistics and graphs by assessment and unit."),
        feature("Cohort Selection",
                paste("Explore baseline characteristics and comorbidities by",
                      "applying baseline windows relative to index date.")),
        feature("Demographic Table",
                "Create custom tables with chosen variables and filters."),
        feature("Line of Therapy",
                "Display treatment regimens and corresponding outcomes."),
        feature("Propensity Score",
                "See propensity score distributions by imputation."),
        feature("Survival Plot",
                "View Kaplan-Meier survival plots."),
        feature("Table Validation",
                "Validate RWE tables in batch.")
      ),
      tabPanel(
        "Additional Features",
        feature("Overview",
                "Display subject level records."),
        feature("Summarize",
                "Provide summary statistics and graphs."),
        feature("Table/Graph Builder",
                "Create a variety of tables and graphs with chosen variables and filters."),
        feature("CAR-T Registry",
                paste("Generate profiles and view subject information",
                      "for CAR-T registry studies."))
      )
    )
  )
}

# Organize contact information
contact_info <- function(name, image, email, offset = 0) {
  column(
    4, align = "center", offset = offset,
    div(img(src = paste0("img/users/", image), 
            height = "24px", class = "img-circle"), name),
    p(email, style = "font-size:14px"),
    hr(style = "border-top:1px solid #eee; margin-top:0px; 
       margin-bottom:10px; width:200px")
  )
}

# Contact page for list of contributors and emails
contact_page <- function(id) {
  tabItem(
    id,
    shinydashboardPlus::box(
      width = 12,
      title = "Contact Us",
      p(em("Sherlock"), "is developed and maintained by the Medical Affairs RWE team.", 
        align = "center"),
      p("Any questions? Please direct inquiries to:", em("Diana.Tai@bms.com"), 
        align = "center"),
      hr(style = "margin-top:0px; margin-bottom:10px"),
      p(strong("Collaborators and contributors"), align = "center"),
      contact_info("Diana Tai", "DianaTai.jpg", 
                   "Diana.Tai@bms.com"),
      contact_info("Joy Wang", "JoyWang.jpg", 
                   "Joy.Wang1@bms.com"),
      contact_info("Lixiao Chen", "LixiaoChen.jpg", 
                   "Lixiao.Chen@bms.com"),
      contact_info("Lorraine Fang", "LorraineFang.jpg", 
                   "Lorraine.Fang@bms.com"),
      contact_info('Po-Chun "Kelvin" Lin', "PoChunLin.jpg", 
                   "Po-Chun.Lin@bms.com"),
      contact_info("Shuang Zhang", "ShuangZhang.jpg", 
                   "Shuang.Zhang1@bms.com"),
      contact_info('Yanhui "Kevin" Yang', "YanhuiYang.jpg", 
                   "Yanhui.Yang2@bms.com", offset = 4),
      fluidRow(
        column(12,
               div(img(
                 src = "https://www.bms.com/assets/bms/us/en-us/logos/bms-rebrand-logo.svg", 
                 height = "24px"), 
                 align = "center",
                 style = "margin-bottom:10px")
        )
      )
    )
  )
}
