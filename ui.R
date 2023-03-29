library(plotly)
library(bslib)
library(ggplot2)
library(dplyr)

#Data
co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)

country_one <- co2_df %>% filter(year >= "1920" & year <= "2020") %>%  group_by(country, year) %>% summarize(num_co2 = sum(co2_per_capita, na.rm = T))

country_co2 <- country_one[!grepl("(GCP)", country_one$country),]


my_theme <- bs_theme(
  bg = "#E5F6DF",
  fg = "blue",
  primary = "white"
)

my_theme <- bs_theme_update(my_theme, bootswatch = "minty")

intro_page <- tabPanel (
  "Introduction",
  htmlOutput("summary")
  
)

plot_page <- tabPanel(
  "Climate Change Data Visualization",

# Application title
titlePanel("CO2 Emissions Data Around The World Over The Years 1920 - 2020"),

# Sidebar with a slider input for number of bins 
  selectInput(inputId = "user_selection",
              label = "Countries & Others",
              choices = country_co2$country,
              selected = "United States",
              multiple = TRUE),

year_slider_widget <- sliderInput(
  inputId = "year_selection",
  label = "Years",
  min = min(country_co2$year),
  max = max(country_co2$year),
  value = c(1750, 2020),
  sep = ""),
  
  # Show a plot of the generated distribution
mainPanel(
    plotlyOutput(outputId = "co2_plot")
  ),

htmlOutput("conclusion")

)



ui <- navbarPage(
  theme = my_theme,
  "Data Visualization of Climate Change Patterns Across the World Related to CO2",
  intro_page,
  plot_page
)