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
library(DT)

scenarios <- read.csv(file = "data/cd-links-gdp-us.csv")


# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("DJIA Risk Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
                  
        sidebarPanel(p("Placeholder to provide some explanation"),
        ),
        
        mainPanel(# Select data
                  
                  dateRangeInput("slider_data_range",
                                 label = "Select the date range for the analysis",
                                 start = "1930-01-01", end = today(),
                                 format = "yyyy-mm-dd"),
                  em("Note: If a date later than today is chosen the date range will download until today autmatically."),
                  em("Please wait for the graphics to load (progress displayed in bottom right corner)."),
                  
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
                              choices = list("Quarterly returns" = "volatility_quarterly",
                                             "Daily returns" = "volatility_daily"),
                              selected = "volatility_quarterly"
                  ),
                  
                  ## Daily volatility
                  conditionalPanel(
                      condition = "input.select_volatility_aggregation == 'volatility_daily'",
                      sliderInput("slider_time_daily",
                                  label = "Select the time horizon",
                                  min = 5, max = 300, value = c(5,50),
                      ),
                      
                      ### plot volatility
                      h4("Market risk", em("(djia daily return volatility)")),
                      plotOutput("vola_plot_daily"),
                      
                      ### message
                      #h4(">> Analysis for daily data not done yet. <<", style = "color:blue"),
                  ),
                  
                  ## Quarterly volatility
                  conditionalPanel(
                      condition = "input.select_volatility_aggregation == 'volatility_quarterly'",
                      
                      ### plot volatility
                      sliderInput("slider_time_quarterly",
                                  label = "Select the time horizon",
                                  min = 5, max = 125, value = c(5,50),
                      ),
                      h4("Market risk", em("(djia quarterly return volatility)")),
                      plotOutput("vola_plot_quarterly"),
                      
                      
                  ),
                  
                  # Including GDP Forecast in Market Risk
                  h3("Relationship between GDP and DJIA"),
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
                      #h4(">> Analysis for absolute GDP and DJIA data not done yet. <<", style = "color:blue"),
                      
                  ),
                  
                  ## growth data
                  conditionalPanel(
                      condition = "input.select_data_type == 'gdp_growth'",
                      selectInput("select_outlier",
                                  label = "Remove Outlier?",
                                  choices = list("yes" = TRUE, 
                                                 "no" = FALSE),
                                  selected = "yes"),
                      
                      #### regression results
                      h4("Correlation GDP growth and DJIA returns", em("(quarterly data)")),
                      strong("Correlation plot"), 
                      plotOutput("regression_growth_plot"),
                      strong("Regression results"),
                      verbatimTextOutput("regression_growth_summary"),
                      
                      ### message
                      #h4(">> Analysis for growth GDP and DJIA data not done yet. <<", style = "color:blue"),
                      
                  ),
                  
                  ## comparison of vola in periods of calm and distress
                  h4("Compare return volatility in periods of market calm and distress"),
                  strong("Table of GDP loss quantiles"),
                  dataTableOutput("table_gdp_quantiles"),
                  selectInput("select_quantile",
                              label = "Selected Quantile",
                              choices = list(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1),
                              selected = 0.5),
                  strong("Return volatility plot filtered by GDP losses"),
                  conditionalPanel(
                      condition = "input.select_quantile == 0.5",
                      img(src = 'vola_plot.png'),
                      
                  ),
                  conditionalPanel(
                      condition = "input.select_quantile!= 0.5",
                      em("This computation is still very slow"),
                      plotOutput("vola_plot_separate"),
                  ),
                  
                  ## Sources
                  h3("Sources"),
                  uiOutput("tabs")
                  )
        )
    
)

# Define server 
server <- function(input, output) {
    
    djia <- reactive({
        tq_get("DJIA", get = "stock.prices", from = input$slider_data_range[1], to = input$slider_data_range[2])
    })
    
    gdp <- reactive({
        tq_get("GDP", get = "economic.data", from = input$slider_data_range[1], to = input$slider_data_range[2])
    })
    
    output$price_plot <- renderPlot({
        plot_descriptives_quarterly(djia(), gdp()) 
    })
    
    output$gdp_plot <- renderPlot({
        plot_descriptives_gdp(gdp(), scenarios)
    })
    
    output$vola_plot_daily <- renderPlot({
        plot_djia_volatility(djia(), c(input$slider_time_daily[1]: input$slider_time_daily[2]), "daily")
    })
    
    output$vola_plot_quarterly <- renderPlot({
        plot_djia_volatility(djia(), c(input$slider_time_quarterly[1]: input$slider_time_quarterly[2]), "quarterly")
    }) 
    
    output$regression_plot <- renderPlot({
        my_regression(djia(), gdp())[[1]]
    })
    
    output$regression_summary <- renderPrint({
        my_regression(djia(), gdp())[[2]]
    })
    
    output$regression_growth_plot <- renderPlot({
        my_regression_growth(djia(), gdp(), input$select_outlier)[[1]]
    })
    
    output$regression_growth_summary <- renderPrint({
        my_regression_growth(djia(), gdp(), input$select_outlier)[[2]]
    })
    
    output$return_plot <- renderPlot({
        plot_descriptives_return(djia(), input$select_returns)
    })

    output$selected_variables <- renderText({
        paste("You have selected to calculate the returns as ", input$select_returns,
              ", and to plot volatility in the aggregation range of ", input$slider_time[1],
              " to ", input$slider_time[2], ".", sep = "")
    })
    
    output$table_gdp_quantiles <- DT::renderDataTable({
        datatable(quantile_analysis(gdp()), 
                  rownames = FALSE) %>% 
            formatRound(columns = 2, digits = 5) %>% 
            formatPercentage(columns = 1, digits = 0)
    })
    
    output$vola_plot_separate <- renderPlot({
        
        if (req(input$select_quantile) != 0.5){
        plot_djia_volatility_separate(djia(), gdp(), input$select_quantile, c(5:50), "daily")
        } else if (input$select_quantile == 0.5){
            em("The image should be displayed, not computed.")
        }
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
