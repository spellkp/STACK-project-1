#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# It's Build-A-Bear for Dispersion Modeling
ui <- fluidPage(
   
   # Application title
   titlePanel("HYSPLIT Control Parameters"),
   
   # Input the data!
  column(4, textInput("01", label = h4("Dispersion Start- YY MM DD HH"), value = ""), placeholder = "YY MM DD HH"))

  column(4, selectInput("02", label = h4("Number of Point Sources"), choices = c("1", "2", "3", "4", "5"),
                          selected = NULL, multiple = FALSE, selectize = FALSE))
  

# Run the application 
runApp()

