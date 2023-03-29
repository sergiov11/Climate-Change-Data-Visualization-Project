library(ggplot2)
library(plotly)
library(dplyr)

#Data
co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)

country_one <- co2_df %>% filter(year >= "1920" & year <= "2020") %>%  group_by(country, year) %>% summarize(num_co2 = sum(co2_per_capita, na.rm = T))

country_co2 <- country_one[!grepl("(GCP)", country_one$country),]


# Summary Values
country_max_co2 <- co2_df %>% filter(co2_per_capita == max(co2_per_capita, na.rm = T)) %>% pull(country)

year_most_co2 <- co2_df %>% filter(co2 == max(co2, na.rm = T)) %>% pull(year)

country_min_co2_2021 <- co2_df %>% filter(year == "2020") %>% filter(co2 == min(co2, na.rm = T)) %>% pull(country)

usa_year_highest_mean_co2 <- co2_df %>% filter(country == "United States") %>% group_by(year) %>% summarize(mean_co2 = mean(co2_per_capita, na.rm = T)) %>% filter(mean_co2 == max(mean_co2, na.rm = T)) %>% pull(year)

usa_highest_mean_co2 <- co2_df %>% filter(country == "United States") %>% group_by(year) %>% summarize(mean_co2 = mean(co2_per_capita, na.rm = T)) %>% filter(mean_co2 == max(mean_co2, na.rm = T)) %>% pull(mean_co2)


server <- function(input, output) {
  
  output$summary <- renderUI({
    
    p1 <- ("Climate change is an important topic of discussion in today's world with arguments often being made to try and prevent it but also people denying it as a legitimate concern in the first place. Thankfully, through the use of many resources and with today's technology, we are able to truly analyze the data and create visualizations to help us really understand and communicate with others why we should take affirmative action to try and stop climate change. This is why in this assignment, we will be analyzing data from CO2 and Greenhouse emissions in our world to try and find patterns and analyze how this is affecting our planet")
    
    p2 <- ("")
    
    p3 <- ("The data we are analyzing comes from the website: Our World in Data. A website with a goal as described to 'make data and research on the world's largest problems understandable and accessible'. Having organizations that still have humanitarian goals such as this as their main focus is especially important in today's world since a lot of times, the reason our environment is affected by humans is caused to corporate greed. The data we are going to be analyzing for this assignment was collected by Hannah Ritchie, Max Roser, Edouard Mathieu, Bobbie Macdonald, and Pablo Rosado. Although the data has a lot of useful information, it also contains some limitations that could create some gaps in our understanding of what some of the numbers really represent. For instance, one of them is that the raw data is messy so it creates extra work to clean and understand which in turn can cause unintended miscalculations. Another limitation is that some of the countries in the data don't have actual numbers and instead some of the data is made up of NA values or zeros. Nonetheless, analyzing this data helps to give us valuable insight regarding CO2 and greenhouse emissions in the world and how big of a role some specific countries are playing in this.")
    
    p4 <- ("")
    
    p5 <- ("While analyzing the data, I was able to find a lot of interesting information regarding CO2 emissions by certain countries in specific years. For example, I found that the year 2021 was the year with the highest CO2 emissions and the country that contributed the least to this was Tuvalu. Additionally, I found that the country that has had the highest CO2 emissions Per capita is Sint Maarten (Dutch Part) with a number of around 824.46. When it comes to the United States, the data also provides us with some interesting insight such as how the year with the highest mean number of CO2 emissions was 1973, with a mean of around 23.08. Analyzing our data for information like this is important because it allows us to have a better understanding of it and therefore it makes it easier to communicate with others.")
    
    p6 <- ("")
    
    p7 <- ("In conclusion, analyzing and playing around with this data was not only fun but also a really good learning experience. It taught me a lot more about just how much people from different countries contribute to CO2 emissions and for this reason, it is also information that I want to share with others. Having information like this is important in order for us to truly understand the impact we are having on our planet and what this could mean for not only our future but for the future of generations to come. Because I believe that a big part of making change is informing others about what is wrong in the first place and what we can try and do to fix it together.")
    
    HTML(paste(p1, p2, p3, p4, p5, p6, p7, sep = '<br/>'))
    
  })
  
  output$conclusion <- renderUI({
    
    p1 <- ("")
    
    p2 <- ("The reason I decided to include this chart is because it gives a nice overview of the impact that CO2 emissions per Capita have had over the last 100 years. As you play around with the graph you can find interesting patterns such as how the U.S. had some of its highest CO2 emissions per capita in the early 1970s. I think it would be interesting to do more research and find out the reasoning behind these high numbers.")
    
    HTML(paste(p1, p2, sep = '<br/>'))
    
  })
  
  output$co2_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    
    filtered_df <- country_co2 %>% filter(country %in% input$user_selection) %>% 
      filter(year > input$year_selection[1] & year < input$year_selection[2])
    
    # draw the histogram with the specified number of bins
    co2_plot <- ggplot(data = filtered_df) +
      geom_line(aes(x = year,
                    y = num_co2,
                    color = country)) +
      labs(x = "Year",
           y = "Amount of CO2 Per Capita",
           color = "Countries & Others")
    
    return(ggplotly(co2_plot))
    
  })
}