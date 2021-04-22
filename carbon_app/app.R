#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("helpers.R")

library(shiny)
library(tidyquant)
library(tidyverse)

djia <- tq_get("DJIA", get = "stock.prices", from = "1990-01-01")
gdp <- tq_get("GDP", get = "economic.data", from = "1990-01-01")
scenarios <- read.csv(file = "data/cd-links-gdp-us.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DJIA Risk Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
                  
        sidebarPanel(p("Placeholder to provide some explanation")
                     #sliderInput("slider_time2",
                     #            label = "Select the aggregation range for volatility of quarterly returns",
                     #            min = 5, max = 300, value = c(5,120),
                     #),
                     
                     #selectInput("select_vola",
                     #            label = "How to calculate the volatility",
                     #            choices = list("rolling", "non-overlapping"),
                     #            selected = "rolling")
                     
        ),
        
        mainPanel(em("please wait for the graphics to load (progress displayed in bottom right corner)"),
                  
                  # Input variables
                  
                  ## GDP and DJIA absolute value
                  h3("Graphs of input variables"),
                  h4("DJIA price and GDP Timeline", em("(quarterly data)")),
                  plotOutput("price_plot"),
                  h4("GDP scenarios", em("(quarterly and five-year data)")),
                  plotOutput("gdp_plot"),
                  
                  ## DJIA returns
                  h4("DJIA Closing Price Returns", em("(daily data)")),
                  selectInput("select_returns",
                              label = "How to calculate the returns",
                              choices = list("arithmetic", "log"),
                              selected = "arithmetic"
                  ),
                  plotOutput("return_plot"),
                  
                  # Market risk
                  h3("Market Risk"),
                  selectInput("select_volatility_aggregation",
                              label = "Select the volatility aggregation level",
                              choices = list("Quarterly volatiltiy" = "volatility_quarterly",
                                             "Daily volatility" = "volatility_daily"),
                              selected = "volatility_quarterly"
                  ),
                  
                  ## Daily analysis
                  conditionalPanel(
                      condition = "input.select_volatility_aggregation == 'volatility_daily'",
                      sliderInput("slider_time_daily",
                                  label = "Select the time horizon",
                                  min = 5, max = 300, value = c(5,50),
                      ),
                      
                      ### plot volatility
                      h4("Market risk (daily)", em("(daily djia closing price volatility)")),
                      plotOutput("vola_plot_daily"),
                      
                      ### message
                      div("Analysis for daily data not done yet.", style = "color:blue"),
                  ),
                  
                  ## Quarterly analysis
                  conditionalPanel(
                      condition = "input.select_volatility_aggregation == 'volatility_quarterly'",
                      
                      ### plot volatility
                      sliderInput("slider_time_quarterly",
                                  label = "Select the time horizon",
                                  min = 5, max = 135, value = c(5,50),
                      ),
                      h4("Market risk", em("(quarterly djia closing price volatility)")),
                      plotOutput("vola_plot_quarterly"),
                      
                      # Including GDP Fprecast in Market Risk
                      h3("Including GDP Forecast in Market risk"),
                      selectInput("select_data_type", 
                                  label = "Select what data to work with",
                                  choices = list("GDP Growth and DJIA Returns" = "gdp_growth",
                                                 "GDP Absolute and DJIA Prices" = "gdp_absolute"),
                                  selected = "gdp_growth"
                      ),
                      
                      ## absolute data
                      conditionalPanel(
                          condition = "input.select_data_type == 'gdp_absolute'",
                          
                          ### regression results
                          h4("Correlation GDP and DJIA Price", em("(quarterly data)")),
                          plotOutput("regression_plot"),
                          verbatimTextOutput("regression_summary"),
                          
                          ### message
                          div("Analysis for absolute GDP and DJIA data not done yet.", style = "color:blue"),
                          
                      ),
                      
                      ## growth data
                      conditionalPanel(
                          condition = "input.select_data_type == 'gdp_growth'",
                          selectInput("select_outlier",
                                      label = "Remove Covid Outlier?",
                                      choices = list("yes" = TRUE, 
                                                     "no" = FALSE),
                                      selected = "yes"),
                          
                          #### regression results
                          h4("Correlation GDP growth and DJIA returns", em("(quarterly data)")),
                          plotOutput("regression_growth_plot"),
                          verbatimTextOutput("regression_growth_summary"),
                          
                          ### message
                          div("Analysis for growth GDP and DJIA data not done yet.", style = "color:blue"),
                          
                      ),
                      
                  ),
                  
                  ## Sources
                  h3("Sources"),
                  uiOutput("tabs")
                  )
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$price_plot <- renderPlot({
        plot_descriptives_quarterly(djia, gdp) 
    })
    
    output$gdp_plot <- renderPlot({
        plot_descriptives_gdp(gdp, scenarios)
    })
    
    output$vola_plot_daily <- renderPlot({
        plot_djia_volatility_daily(djia, c(input$slider_time_daily[1]: input$slider_time_daily[2]))
    })
    
    output$vola_plot_quarterly <- renderPlot({
        plot_djia_volatility_quarterly(djia, c(input$slider_time_quarterly[1]: input$slider_time_quarterly[2]))
    }) 
    
    output$regression_plot <- renderPlot({
        my_regression(djia, gdp)[[1]]
    })
    
    output$regression_summary <- renderPrint({
        my_regression(djia, gdp)[[2]]
    })
    
    output$regression_growth_plot <- renderPlot({
        my_regression_growth(djia, gdp, input$select_outlier)[[1]]
    })
    
    output$regression_growth_summary <- renderPrint({
        my_regression_growth(djia, gdp, input$select_outlier)[[2]]
    })
    
    output$return_plot <- renderPlot({
        plot_descriptives_return(djia, input$select_returns)
    })

    output$selected_variables <- renderText({
        paste("You have selected to calculate the returns as ", input$select_returns,
              ", and to plot volatility in the aggregation range of ", input$slider_time[1],
              " to ", input$slider_time[2], ".", sep = "")
    })
    
    
    url_djia <- a("Yahoo Finance, DJIA", href = "https://finance.yahoo.com/quote/%5EDJI/")
    url_gdp <- a("FRED, GDP", href = "https://fred.stlouisfed.org/series/GDP")
    url_scenarios <- a("IIASA, CD-LINKS Scenario Explorer", href = "https://data.ene.iiasa.ac.at/cd-links/")
    
    output$tabs <- renderUI({
        tagList(url_djia, url_gdp, url_scenarios)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
