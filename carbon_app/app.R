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
                  
        sidebarPanel(# Select data
                    dateRangeInput("slider_data_range",
                           label = "Select the date range for the analysis",
                           start = "1930-01-01", end = today(),
                           format = "yyyy-mm-dd"),
                    em("Note: If a date later than today is chosen the date range will download until today autmatically."),
                    em("Please wait for the graphics to load (progress displayed in bottom right corner)."),
        ),
        
        mainPanel(# Input variables
                  
                  ## GDP and DJIA absolute value
                  h3("Graphs of input variables"),
                  p("The following graphs show the data available for analysis"),
                  tags$ul(
                      tags$li(strong("Dow Jones Industrial Average (DJIA)"), ": Daily data of DJIA, returns are calculated both as logarithmic and arithmetic, prices are inflation-adjusted. To compare GDP and DJIA prices, DJIA was aggregated to quarterly data."),
                      tags$li(strong("Gross Domestic Product (GDP)"), ": Quarterly data of GDP in billions of dollars, seasonally adjusted annual rate"),
                      tags$li(strong("Scenario Data"), ": Scenario data analysing the effect of climate change (mitigation) on GDP, using the integrated assessment model", em("Remind 1.7 - MAgPIE 3.0"), tags$a(href = "https://data.ene.iiasa.ac.at/cd-links/#/docs", "(Model documentation)"), "for six different scenarios (Explanation of scenarios at the end)."), 
                  ),
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
                  p("The following graph aggregates djia return volatility (daily or quarterly, depending on the input) and plots the standard deviation as a function of aggregation horizon."),
                  
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
                  h4("Correlation GDP growth and DJIA returns", em("(quarterly data)")),
                  p("The following paragraph explores the relationship between GDP and DJIA. First correlation and regression are displayed for both GDP and DJIA prices and returns for quarterly data."),
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
                      strong("Correlation plot"), 
                      plotOutput("regression_growth_plot"),
                      strong("Regression results"),
                      verbatimTextOutput("regression_growth_summary"),
                      
                      ### message
                      #h4(">> Analysis for growth GDP and DJIA data not done yet. <<", style = "color:blue"),
                      
                  ),
                  
                  ## comparison of vola in periods of calm and distress
                  h4("Compare return volatility in periods of market calm and distress"),
                  p("The following section analyses the relationship between GDP losses and DJIA by plotting selecting the largest GDP losses and plotting DJIA return standard deviation separate for quarters where GDP losses exceed the selected quantile (in loss period) and quarters where GDP losses do not exceed the selected quantile or GDP increased (outside loss period)."),
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
                  
                  # Brief discussion
                  h3("Discussion points"),
                  tags$ul(
                      tags$li("Integration of the scenario data: The scenario data from iiasa is at 5-year aggregation, and the variation in GDP between the scenarios is not very large, which has to be taken into account when integration the scenario data in the standard deviation plot, which is at daily or quarterly level."),
                      tags$li("The correlation/regression results between GDP and DJIA are mostly strong for price data, which probably however mostly catches the positive trend of both variabels."),
                      tags$li("Quarters with high GDP loss have an overall higher DJIA return volatility over all displayed aggregation horizons."),
                  ),
                  
                  # Further notes
                  h3("Further notes"),
                  p("Scenarios (iiasa documentation):"),
                  tags$ul(
                      tags$li("INDCi: Intended Nationally Determined Contributions (INDC) scenario that implements the first round of INDCs until 2030 and extrapolates the implied effort beyond 2030"),
                      tags$li("No Policy: Baseline scenario without any climate policy in place"),
                      tags$li("NPi: National Policies implemented scenario includes currently implemented climate, energy and land policies and extrapolates the implied effort beyond the direction of the policies"),
                      tags$li("NPI2020_1000: NPi scenario until 2020 with a transition to a globally cost-effective implementation of a carbon budget for the period 2011-2100 of 1000 GtCO2 afterwards, corresponding to staying below 2C at >66% through the 21st century"),
                      tags$li("NPI2020_1600: NPi scenario until 2020 with a transition to a globally cost-effective implementation of a carbon budget for the period 2011-2100 of 1000 GtCO2 afterwards, corresponding to staying below 2C at >66% through the 21st century"),
                      tags$li("NPI2020_400: NPi scenario until 2020 with a transition to a globally cost-effective implementation of a carbon budget for the period 2011-2100 of 400 GtCO2 afterwards, corresponding to a chance of >66% for staying below 1.5C in 2100"),
                  ),
                  
                  # Sources
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
