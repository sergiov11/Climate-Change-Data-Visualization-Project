library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

source("ui.R")
source("server.R")
library(rsconnect)

# Run the application 
shinyApp(ui = ui, server = server)
