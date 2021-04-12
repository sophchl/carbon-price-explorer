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
                  h3("Graphs of input variables"),
                  strong("DJIA price and GDP (USA) Timeline"),
                  plotOutput("price_plot"),
                  strong("DJIA returns"),
                  plotOutput("return_plot"),
                  strong("GDP scenarios"),
                  plotOutput("gdp_plot"),
                  h3("Graphs of different risks and returns"),
                  strong("Market risk"),
                  textOutput("selected_variables"),
                  plotOutput("vola_plot"),
                  )
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$price_plot <- renderPlot({
        plot_descriptives_quarterly(djia, "arithmetic", gdp) # fix "arithmetic" because returns don't matter in this plot
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
}

# Run the application 
shinyApp(ui = ui, server = server)
