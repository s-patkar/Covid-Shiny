library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggthemes)
worldometer_raw <- read_csv("worldometer_coronavirus_daily_data.csv")
econ_raw <- read_csv("economic_freedom_index2019_data.csv")

econ_rename <- econ_raw %>% 
  rename("country" = "Country Name",
         "region" = "Region",
         "GDPPC" = "GDP per Capita (PPP)",
         "pop" = "Population (Millions)",
         "rrank" = "Region Rank",
         "prop_right" = "Property Rights",
         "jud_eff" = "Judical Effectiveness",
         "unemp" = "Unemployment (%)",
         "ffree" = "Financial Freedom")


full_raw <- inner_join(x = worldometer_raw, y = econ_rename, by = "country") %>% 
  select("date", "country", "daily_new_cases",
         "cumulative_total_deaths", "cumulative_total_cases",
         "GDPPC", "region", "pop", "rrank", "prop_right",
         "jud_eff", "unemp", "ffree") 



short_raw <- full_raw %>% 
  filter(date == "2021-6-11") %>% 
  filter(cumulative_total_cases > 100) %>% 
  filter(country != "India",  country != "China", country != "Yemen") %>% 
  mutate(death_rate = cumulative_total_deaths/cumulative_total_cases)%>% 
  mutate(infec_rate = cumulative_total_cases/(as.numeric(pop)*1000000))





ui <- navbarPage("Covid TrackR",
                 tabPanel("TrackR"),
                 tabPanel("Socio-economics",
                          fluidPage(                      
                            fluidRow(       
                              column(4,
                                     h3("Variable Selection"),
                                     helpText(h5("Click to explore dropdown menu.")),
                                     selectInput("var",
                                                 label = "Choose an x-variable to display",
                                                 choices = c("Population", "Unemployment", "Property_Rights", "Judical_Efficiency_Score"),
                                                 selected = "Population"),
                                     selectInput("vary",
                                                 label = "Choose an y-variable to display",
                                                 choices = c("Death_Rate", "Infection_rate"),
                                                 selected = "Death_Rate") 
                              ),
                              column(8,
                                     #style = "background-color:#4d3a7d;",
                                     h3("Graph"),
                                     textOutput("selected_var"),
                                     plotOutput("plot1"),
                                     h3("Analysis"),
                                     h4("Population vs. Death Rate"),
                                     h5(""),
                                     h4("Unemployment vs. Death Rate"),
                                     h4("Property Rights Score vs. Death Rate"),
                                     h4("Judical Efficiency Score vs. Death Rate"),
                              )
                            )
                          )
                 ),
                 tabPanel("About"),
                 navbarMenu("Sources",
                            tabPanel("Source A"),
                            tabPanel("Source B"))
)


server <- function(input, output) {
  
  datax <- reactive({
    switch(input$var,
           Population = short_raw$pop,
           Unemployment = short_raw$unemp,
           Property_Rights = short_raw$prop_right, 
           Judical_Efficiency_Score = short_raw$jud_eff
    )
    
  })
  
  datay <- reactive({
    switch(input$vary,
           Death_Rate = short_raw$death_rate,
           Infection_rate = short_raw$infec_rate
    )
  })
  
  
  output$selected_var <- renderText({
    paste("You have selected", input$var, "vs." , input$vary)
  }) 
  
  output$plot1 <- renderPlot({
    plot(datax(), datay(), xlab = input$var, ylab = "Death Rate")
  })
}


shinyApp(ui = ui, server = server)



