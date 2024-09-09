#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Based on https://www.scielosp.org/article/ssm/content/raw/?resource_ssm_path=/media/assets/bwho/v87n3/10.pdf

library(shiny)
source("diagnosis.R")
colorize <- function(x, color) {
  sprintf("<h3><span style='color: %s;'>%s</span></h3>", color,
          x)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Primary care skin identification algorithm"),
  "Please check the following boxes:",
  diagnose_UI("itch")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
 diagnose_Server("itch")
}

# Run the application 
shinyApp(ui = ui, server = server)
