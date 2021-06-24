library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggthemes)
worldometer_raw <- read_csv("worldometer_coronavirus_daily_data.csv")
econ_raw <- read_csv("economic_freedom_index2019_data.csv")
lifex_raw <- read_csv("Life Expectancy Data.csv")

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

lifex_raw <- lifex_raw %>% 
  filter(Year == "2015") %>% 
  rename("life_ex" = "Life expectancy",
         "inf_mort" = "infant deaths", 
         "country" = "Country") %>% 
  select("life_ex", "inf_mort", "country")



short_raw <- inner_join(full_raw, lifex_raw, by = "country") %>% 
  filter(date == "2021-6-11") %>% 
  filter(cumulative_total_cases > 100) %>% 
  #filter(country != "India",  country != "China", country != "Yemen") %>% 
  mutate(death_rate = cumulative_total_deaths/cumulative_total_cases)%>% 
  mutate(infec_rate = cumulative_total_cases/(as.numeric(pop)*1000000))





ui <- navbarPage("Covid TrackR",
                 tabPanel("Socio-economics",
                          fluidPage(h2("How do different socio-economic metrics affect
                                       Covid-19 death rates and infection rates around the world?"), align = "center",                     
                            fluidRow(align = "left",       
                              column(2,offset = 2,
                                     h3("Variable Selection"),
                                     helpText(h5("Click to explore dropdown menu.")),
                                     selectInput("var",
                                                 label = "Choose an x-variable to display",
                                                 choices = c("Population", "Unemployment_Rate",
                                                             "Property_Rights_Score", "Judical_Efficiency_Score",
                                                             "Life_Expectancy"),
                                                 selected = "Population"),
                                     selectInput("vary",
                                                 label = "Choose an y-variable to display",
                                                 choices = c("Death_Rate", "Infection_rate"),
                                                 selected = "Death_Rate") 
                              ),
                              column(6,
                                     #style = "background-color:#4d3a7d;",
                                     h3("Graph"),
                                     textOutput("selected_var"),
                                     plotlyOutput("plot2"),
                                     h3("Analysis"),
                                     br(),
                                     h4("Population vs. Death/Infection Rate:"),
                                     h5("There is a slight positive correalation between a country's population
                                        and it's Covid-19 death toll. This may be because countries with very large
                                        populations tend to be clustered in dense cities where transmission is more likely.
                                        This same correlation does not exist with infection rate."),
                                     br(),
                                     h4("Unemployment Rate vs. Death/Infection Rate:"),
                                     h5("There is a strong positive and linear correlation between a country's population
                                        and it's Covid-19 death toll. There is no relation between unemployment and infection rates."),
                                     br(),
                                     h4("Property Rights score vs. Death/Infection Rate:"),
                                     h5("There is a strong positive correlation between a country's property rights score and
                                        infection rate. The opposite is true for death rates."),
                                     br(),
                                     h4("Judical Efficiency Score vs. Death/Infection Rate:"),
                                     h5("There is no correlation"),
                                     br(),
                                     h4("Life Expectancy vs. Death/Infection Rate:"),
                                     h5("There is a slight positive correlation between life expectancy and both death and 
                                        infection rates."),
                                     br(),
                                     br()
                                     
                              )
                            )
                          )
                 ),
                 tabPanel("About",
                          fluidPage(
                                    fluidRow(
                                      column(4, offset = 3,
                                             h3("About Me"),
                                             h5("Hi I'm Shreeram Patkar, a first-year Medical Science student at the
                                                University of Western Ontario with an interest in healthcare, computer
                                                science and robotics."),
                                             br(),
                                             h3("About the Project"),
                                             h4("Covid-19 TrackR and socio-economic causal analysis"),
                                             h5("This project aims to graphically display the progression of Covid-19
                                                over the past year by country and to to analyse the impacts of various
                                                socio-economic metrics and factors on the virus's spread."),
                                             br(),
                                             h3("Goals"),
                                             h5("- To demonstrate correlations while:"),
                                             h5("- Effectively demonstrating data in an easy-to-read format."),
                                             h5("- Retaining data accuracy and credibility.")
                                             ),
                                      column(2, offset =0, 
                                             img(src='image.jpg', height = 200, width = 200))
                                             
                                             
                                    )
                            
                          )),
                 navbarMenu("Sources",
                            tabPanel("Source A", 
                                     fluidPage(
                                       fluidRow(
                                         column(4, offset = 3,
                                                h2("Source A"),
                                                h4("Source A is data about Covid-19 Daily statistics from the worldometer
                                                   organization and is retreived from Kaggle.com"),
                                                br(),
                                                h5("Scraped Kaggle URL:"),a("https://www.kaggle.com/josephassaker/covid19
                                                        -global-dataset?select=worldometer_coronavirus_daily_data.csv"),
                                                h5("Original Source URL:"),a("worldometers.info")
                                         ),
                                         column(2, offset =0, 
                                                img(src='worldometer.jpg', height = 200, width = 200))
                                       ))
                                       
                                     ),
                            tabPanel("Source B", 
                                     fluidPage(
                                       fluidRow(
                                         column(4, offset = 3,
                                                h2("Source B"),
                                                h4("Source B is data about different metrics in the economic freedom index
                                                from the world heritage organization and is retreived from Kaggle.com"),
                                                br(),
                                                h5("Scraped Kaggle URL:"),a("https://www.kaggle.com/lewisduncan93/
                                                                            the-economic-freedom-index"),
                                                h5("Original Source URL:"),a("https://www.heritage.org/index")
                                         ),
                                         column(2, offset =0, 
                                                img(src='heritage.png', height = 200, width = 450))
                                         
                                        ))
                                     
                                     ),
                            tabPanel("Source C", 
                                     fluidPage(
                                       fluidRow(
                                         column(4, offset = 3,
                                                h2("Source C"),
                                                h4("Source C is data about life expectancy and various health metrics 
                                                around the world from the collected by the World Health Organization and
                                                   retreived from Kaggle.com"),
                                                br(),
                                                h5("Scraped Kaggle URL:"),a("https://www.kaggle.com/kumarajarshi/life-expectancy-who"),
                                                h5("Original Source URL:"),a("https://www.who.int/data/gho/data/indicators/
                                                                             indicator-details/GHO/life-expectancy-at-birth-(years)")
                                        ),
                                         column(2, offset =0, 
                                                img(src='who.png', height = 200, width = 200))
                                                
                                         ))
                                     
                                     )
                            )
)


server <- function(input, output) {
  
  datax <- reactive({
    switch(input$var,
           Population = short_raw$pop,
           Unemployment_Rate = short_raw$unemp,
           Property_Rights_Score = short_raw$prop_right, 
           Judical_Efficiency_Score = short_raw$jud_eff,
           Life_Expectancy = short_raw$life_ex
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
  
  
  output$plot2 <- renderPlotly({
    
    econ_plot <- short_raw %>% 
      ggplot(aes(color = country, x= as.numeric(datax()) , y = datay(),
                 text = paste("Country:", country, "\n",
                              paste(input$var,": "), paste(datax(),ifelse(input$var == "Population", "M", ""),
                                                           ifelse(input$var == "Unemployment_Rate", "%", "")), "\n",
                              "Death Rate:", paste(format(round(death_rate*100, 2), nsmall = 2), "%"), "\n",
                              "GDPPC:", GDPPC, "\n",
                              "Total Deaths:", cumulative_total_deaths, "\n",
                              "Total Cases:", cumulative_total_cases, "\n")))+
      geom_point()+
      geom_smooth()+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      theme_clean()+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none")+
      facet_wrap(~region)+
      labs(x = input$var, y = input$vary)
    
    
    econ_plot <- ggplotly(econ_plot, tooltip = "text")
  })
}


shinyApp(ui = ui, server = server)



