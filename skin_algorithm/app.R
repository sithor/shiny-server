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
  tags$style(HTML("
    .sidebar-image {
      width: 100%;  /* Set the width to 100% of the container */
      height: auto; /* Maintain the aspect ratio */
      max-width: 400px; /* Set a maximum width (adjust as needed) */
    }
  ")),
  tags$style(HTML("
    #itch-scabies_image_age img, #itch-infected_scabies_image_age img{
      width: 100%;   /* Set the width to 100% of the container */
      height: auto;  /* Maintain aspect ratio */
      max-width: 400px;  /* Set maximum width */
    }
  ")),
  diagnose_UI("itch")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  bs_themer()
 diagnose_Server("itch")
}

# Run the application 
shinyApp(ui = ui, server = server)
