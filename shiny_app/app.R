
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Read model output
data <- read.csv(file.path(datadir, "west_coast_fish_stock_data.csv"), as.is=T)

# Species
spp <- data %>% pull(comm_name) %>% unique() %>% sort()

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))



# User interface
################################################################################

# User interface
ui <- fluidPage(

  # Title
  titlePanel("WC fisheries insurance exploration tool"),

  # Select species
  selectInput(inputId = "species", label = "Select a species:",
              choices = spp,  multiple = F, selected="United States"),
  br(),


  # Plot historical comparison
  plotOutput(outputId = "plot_data", width=850, height=700)


)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot data
  output$plot_data <- renderPlot({
    g <- plot_data(data = data,
                   spp=input$species)
    g
  })

}

shinyApp(ui = ui, server = server)
