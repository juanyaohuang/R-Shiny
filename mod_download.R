# Download data as Excel/CSV file, table/graph as PDF/RTF

# Module UI function
mod_download_ui <- function(id, format) {
  ns <- NS(id)
  if(format == "data") {
    shinydashboardPlus::boxDropdown(
      icon = shiny::icon("download"),
      shinydashboardPlus::boxDropdownItem(downloadLink(ns("excel"), "Excel")),
      shinydashboardPlus::dropdownDivider(),
      shinydashboardPlus::boxDropdownItem(downloadLink(ns("csv"), "CSV"))
    )
  } else if(format %in% c("table", "graph")) {
    shinydashboardPlus::boxDropdown(
      icon = shiny::icon("download"),
      shinydashboardPlus::boxDropdownItem(downloadLink(ns("pdf"), "PDF")),
      shinydashboardPlus::dropdownDivider(),
      shinydashboardPlus::boxDropdownItem(downloadLink(ns("rtf"), "RTF"))
    )
  }
}

# Module server function
mod_download_server <- function(id, x, filename, format) {
  
  moduleServer(id, function(input, output, session) {
    
    # Append current date to filename
    filename_time <- paste0(filename, "_", Sys.Date())
    
    if(format == "data") {
      
      output$excel <- downloadHandler(
        filename = function() {
          paste0(filename_time, ".xlsx")
        },
        content = function(file) {
          x() %>%
            writexl::write_xlsx(file)
        }
      )
      
      output$csv <- downloadHandler(
        filename = function() {
          paste0(filename_time, ".csv")
        },
        content = function(file) {
          x() %>%
            write.csv(file, row.names = FALSE)
        }
      )
      
    }
    
    if(format %in% c("table", "graph")) {
      
      output$pdf <- downloadHandler(
        filename = function() {
          paste0(filename_time, ".pdf")
        },
        content = function(file) {
          x() %>%
            reporter::write_report(file, "PDF")
        }
      )
      
      output$rtf <- downloadHandler(
        filename = function() {
          paste0(filename_time, ".rtf")
        },
        content = function(file) {
          x() %>%
            reporter::write_report(file, "RTF")
        }
      )
      
    }
    
  })
}
