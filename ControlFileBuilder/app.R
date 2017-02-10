library(shiny)
ui <- shinyUI(fluidPage(titlePanel("HYSPLIT Control File"), mainPanel(
  
  fluidPage(
  
  textInput("01", label = h5("Dispersion Start (YY MM DD HH)"), value = ""),
  
  textInput("11", label = h5("Pollutant Name (4 Characters Max)"), value = ""),

  textInput("12", label = h5("Average Emission Rate (mass/hr)"), value = ""),
    
  selectInput("02", label = h5("Number of Point Sources"), choices = c("1", "2", "3"),
                      selected = NULL, multiple = FALSE, selectize = FALSE),

  textInput("03", label = h6("Point Source 1 Parameters (lat, lon, height(AGL), exhaust velocity (m/s), area (m^2)"), value = ""),
    
  textInput("04", label = h6("Point Source 2 Parameters (lat, lon, height(AGL), exhaust velocity (m/s), area (m^2)"), value = ""),
  
  textInput("05", label = h6("Point Source 3 Parameters (lat, lon, height(AGL), exhaust velocity (m/s), area (m^2)"), value = ""),
  
  textInput("06", label = h5("Emission Length (hr)"), value = ""),
    
  textInput("08", label = h5("Lower Model Level (m)"), value = ""),
  
  textInput("09", label = h5("Upper Model Level (m)"), value = "")


## Number of MET files go here!

## Path to MET files go here!

))))

server <- function(input, output) {
  
  
  
}

shinyApp(ui = ui, server = server)