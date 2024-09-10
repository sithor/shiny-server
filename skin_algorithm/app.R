#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Based on https://www.scielosp.org/article/ssm/content/raw/?resource_ssm_path=/media/assets/bwho/v87n3/10.pdf

library(shiny)
library(bslib)
source("diagnosis.R")
colorize <- function(x, color) {
  sprintf("<h3><span style='color: %s;'>%s</span></h3>", color,
          x)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(),
  #theme = bslib::bs_theme(bootswatch = "sketchy"),
  # Application title
  titlePanel("Primary care children's skin diagnosis aid"),
  "Please check the following boxes:",
  tags$head(tags$style(
    type="text/css",
    "#image img {max-width: 100%; width: 100%; height: auto}"
  )),
  diagnose_UI("itch")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  bs_themer()
 diagnose_Server("itch")
}

# Run the application 
shinyApp(ui = ui, server = server)
