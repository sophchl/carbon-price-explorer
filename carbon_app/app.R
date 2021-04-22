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
                  
        sidebarPanel(p("Placeholder to provide some explanation"),
                     em("please wait for the graphics to load (progress displayed in bottom right corner)")
                     #sliderInput("slider_time2",
                     #            label = h4("Select the aggregation range for volatility of quarterly returns"),
                     #            min = 5, max = 300, value = c(5,120),
                     #),
                     
                     #selectInput("select_vola",
                     #            label = h4("How to calculate the volatility"),
                     #            choices = list("rolling", "non-overlapping"),
                     #            selected = "rolling")
                     
        ),
        
        mainPanel(## Input variables: DJIA and GDP
                  h3("Graphs of input variables"),
                  strong("DJIA price and GDP (USA) Timeline"), em("(quarterly data)"),
                  plotOutput("price_plot"),
                  strong("GDP scenarios"), em("(quarterly and five-year data)"),
                  plotOutput("gdp_plot"),
                  
                  ## Market risk
                  h3("Risk"),
                  sliderInput("slider_time_daily",
                              label = h4("Select the aggregation horizon"),
                              min = 5, max = 300, value = c(5,50),
                  ),
                  strong("Market risk (daily)"), em("(daily djia closing price volatility)"),
                  plotOutput("vola_plot_daily"),
                  sliderInput("slider_time_quarterly",
                              label = h4("Select the aggregation horizon"),
                              min = 5, max = 135, value = c(5,50),
                  ),
                  strong("Market risk (quarterly)"), em("(quarterly djia closing price volatility)"),
                  plotOutput("vola_plot_quarterly"),
                  
                  ## Including GDP Scenario in market risk
                  h3("Including GDP Forecast in Market risk"),
                  strong("Correlation GDP and DJIA"), em("quarterly djia closing price and gdp data"),
                  plotOutput("regression_plot"),
                  verbatimTextOutput("regression_summary"),
                  
                  
                  
                  ## DJIA risk and returns
                  h3("Stock Returns"),
                  selectInput("select_returns",
                              label = h4("How to calculate the returns"),
                              choices = list("arithmetic", "log"),
                              selected = "arithmetic"),
                  strong("DJIA returns"), em("(daily data)"),
                  plotOutput("return_plot"),
                  
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
