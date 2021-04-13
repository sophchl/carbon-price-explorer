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
                  
        sidebarPanel(h3("Select the input here please"),
                     
                     selectInput("select_returns",
                                 label = h4("How to calculate the returns"),
                                 choices = list("arithmetic", "log"),
                                 selected = "rolling"),
                     
                     #selectInput("select_vola",
                     #            label = h4("How to calculate the volatility"),
                     #            choices = list("rolling", "non-overlapping"),
                     #            selected = "rolling"),
                     
                     sliderInput("slider_time",
                                 label = h4("Select the aggregation range for volatility of daily returns"),
                                 min = 5, max = 300, value = c(5,200),
                                 )
                     
                     #sliderInput("slider_time2",
                     #            label = h4("Select the aggregation range for volatility of quarterly returns"),
                     #            min = 5, max = 300, value = c(5,120),
                     #)
                     
        ),
        
        mainPanel(em("please wait for the graphics to load (progress displayed in bottom right corner)"),
                  ## Input variables: DJIA and GDP
                  h3("Graphs of input variables"),
                  strong("DJIA price and GDP (USA) Timeline"), em("(quarterly data)"),
                  plotOutput("price_plot"),
                  strong("GDP scenarios"), em("(quarterly and five-year data)"),
                  plotOutput("gdp_plot"),
                  ## DJIA risk and returns
                  h3("Graphs of different risks and returns"),
                  textOutput("selected_variables"),
                  strong("DJIA returns"), em("(daily data)"),
                  plotOutput("return_plot"),
                  strong("Market risk"), em("(daily djia closing price volatility)"),
                  plotOutput("vola_plot"),
                  h3("Sources"),
                  uiOutput("tabs")
                  )
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$price_plot <- renderPlot({
        plot_descriptives_quarterly(djia, gdp) # fix "arithmetic" because returns don't matter in this plot
    })
    
    output$return_plot <- renderPlot({
        plot_descriptives_return(djia, input$select_returns)
    })
    
    output$gdp_plot <- renderPlot({
        plot_descriptives_gdp(gdp, scenarios)
    })

    output$selected_variables <- renderText({
        paste("You have selected to calculate the returns as ", input$select_returns,
              ", and to plot volatility in the aggregation range of ", input$slider_time[1],
              " to ", input$slider_time[2], ".", sep = "")
    })
    
    output$vola_plot <- renderPlot({
        plot_djia_volatility_daily(djia, input$select_returns, c(input$slider_time[1]: input$slider_time[2]))
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
