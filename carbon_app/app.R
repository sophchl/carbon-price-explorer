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
library(bslib)
library(tidyquant)
library(tidyverse)
library(DT)

scenarios <- read.csv(file = "data/cd-links-gdp-us.csv")


# Define UI 
ui <- fluidPage(
    
    # set theme
    theme = bs_theme(version = 4, bootswatch = "minty"),
    tags$head(tags$style(".shiny-notification {position: fixed; top: 5% ;left: 80%")),
    
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
            
            h3("Table of Contents"),    
            tags$a(href="#mh0", class = "content-secondary", "Introduction"), br(),
            tags$a(href="#mh1", class = "content-secondary", "Graphs of Input variables"), br(),
            tags$a(href="#mh2", class = "content-secondary", "Market Risk"), br(),
            tags$a(href="#mh3", class = "content-secondary", "Relationship between GDP and DJIA"), br(),
            tags$a(href="#mh4", class = "content-secondary", "Discussion Points"), br(),
            tags$a(href="#mh5", class = "content-secondary", "Further Notes"), br(),
            tags$a(href="#mh6", class = "content-secondary", "Sources"), br(),
            
            ## Introduction
            uiOutput("mh0"),
            p("The aim of this document is to investigate how", strong ("climate change"), "manifests in", strong("financial markets."), "We use the", strong("Dow Jones Industrial Average (DJIA)"), "and measure", strong("market risk"), "as the", strong("DJIA volatility"), 
            "aggregated over different time horizons. To incorporate the effect of climate change we use", strong("scenario analysis data"), "estimating", strong("US GDP"), "along different emission mitigation pathways."),
            
            ## GDP and DJIA absolute value
            uiOutput("mh1"),
            p("The following graphs show the data available for analysis"),
            tags$ul(
                tags$li(strong("Dow Jones Industrial Average (DJIA)"), ": Daily data of DJIA, returns are calculated both as logarithmic and arithmetic, prices are inflation-adjusted. To compare GDP and DJIA prices, DJIA was aggregated to quarterly data."),
                tags$li(strong("Gross Domestic Product (GDP)"), ": Quarterly data of GDP in billions of dollars, seasonally adjusted annual rate"),
                tags$li(strong("Scenario Data"), ": Scenario data analysing the effect of climate change (mitigation) on GDP, using the integrated assessment model", em("Remind 1.7 - MAgPIE 3.0"), tags$a(href = "https://data.ene.iiasa.ac.at/cd-links/#/docs", "(Model documentation)"), "for six different scenarios (Explanation of scenarios at the end)."), 
            ),
            h5("DJIA price and GDP", em("(quarterly data)")),
            plotOutput("price_plot"),
            h5("GDP scenarios", em("(quarterly and five-year data)")),
            plotOutput("gdp_plot"),
            
            ## DJIA returns
            h5("DJIA Closing Price Returns"),
            selectInput("select_return_type",
                        label = "How to calculate the returns",
                        choices = list("arithmetic", "log"),
                        selected = "arithmetic"
            ),
            selectInput("select_return_period",
                        label = "Select the return period",
                        choices = list("daily", "quarterly", "5-year")
            ),
            plotOutput("return_plot"),
            
            # Market risk
            uiOutput("mh2"),
            p("The following graph aggregates djia return volatility (daily or quarterly, depending on the input) and plots the standard deviation as a function of aggregation horizon."),
            
            selectInput("select_volatility_aggregation",
                        label = "Select the volatility aggregation level",
                        choices = list("Daily returns" = "volatility_daily",
                                       "Quarterly returns" = "volatility_quarterly"),
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
                h5("Market risk", em("(djia daily return volatility)")),
                plotOutput("vola_plot_daily"),
                
                ### message
                #h5(">> Analysis for daily data not done yet. <<", style = "color:blue"),
            ),
            
            ## Quarterly volatility
            conditionalPanel(
                condition = "input.select_volatility_aggregation == 'volatility_quarterly'",
                
                ### plot volatility
                sliderInput("slider_time_quarterly",
                            label = "Select the time horizon",
                            min = 5, max = 125, value = c(5,50),
                ),
                h5("Market risk", em("(djia quarterly return volatility)")),
                plotOutput("vola_plot_quarterly"),
                
                
            ),
            
            ## Non-averaged volatility
            selectInput("select_volatility_aggregation2",
                        label = "Select the volatility aggregation level",
                        choices = list("Daily returns" = "daily",
                                       "Quarterly returns" = "quarterly",
                                       "5-year returns" = "5-year"),
                        selected = "volatility_quarterly"
            ),
            h5("Market risk", em("(djia quarterly return volatility without averaging)")),
            plotOutput("vola_plot2"),
            
            # Including GDP Forecast in Market Risk
            uiOutput("mh3"),
            h5("Linear Relationship GDP growth and DJIA returns", em("(quarterly data)")),
            p("You can select between absolute values and returns to see the results of the variables correlation and linear regression."),
            selectInput("select_data_type", 
                        label = "Select what data to work with",
                        choices = list("GDP Growth and DJIA Returns" = "gdp_growth",
                                       "GDP Absolute and DJIA Prices" = "gdp_absolute"),
                        selected = "gdp_growth"
            ),
            selectInput("select_periodicity_regression",
                        label = "Select the Aggregation Period",
                        choices = list("Quarters" = "quarterly",
                                       "5-Years" = "5-year")
            ),
            
            ## absolute data
            conditionalPanel(
                condition = "input.select_data_type == 'gdp_absolute'",
                
                ### regression results
                plotOutput("regression_plot"),
                verbatimTextOutput("regression_summary"),
                
                ### message
                #h5(">> Analysis for absolute GDP and DJIA data not done yet. <<", style = "color:blue"),
                
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
                #h5(">> Analysis for growth GDP and DJIA data not done yet. <<", style = "color:blue"),
                
            ),
            
            ## comparison of vola in periods of calm and distress
            h5("Compare return volatility in periods of market calm and distress"),
            p("The following graph represents a plot of DJIA volatility, clustered in loss and non-loss periods, depending on how much GDP droped in the respective quarter. You can select different quantiles of GDP loss."),
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
            strong("Table of GDP loss quantiles"),
            dataTableOutput("table_gdp_quantiles"),
            
            # Brief discussion
            uiOutput("mh4"),
            tags$ul(
                tags$li("Integration of the scenario data: The scenario data from iiasa is at 5-year aggregation, and the variation in GDP between the scenarios is not very large, which has to be taken into account when integration the scenario data in the standard deviation plot, which is at daily or quarterly level."),
                tags$li("The correlation/regression results between GDP and DJIA are mostly strong for price data, which probably however mostly catches the positive trend of both variabels."),
                tags$li("Quarters with high GDP loss have an overall higher DJIA return volatility over all displayed aggregation horizons."),
            ),
            
            # Further notes
            uiOutput("mh5"),
            p(strong("Scenarios"), "(iiasa documentation):"),
            tags$ul(
                tags$li("INDCi: Intended Nationally Determined Contributions (INDC) scenario that implements the first round of INDCs until 2030 and extrapolates the implied effort beyond 2030"),
                tags$li("No Policy: Baseline scenario without any climate policy in place"),
                tags$li("NPi: National Policies implemented scenario includes currently implemented climate, energy and land policies and extrapolates the implied effort beyond the direction of the policies"),
                tags$li("NPI2020_1000: NPi scenario until 2020 with a transition to a globally cost-effective implementation of a carbon budget for the period 2011-2100 of 1000 GtCO2 afterwards, corresponding to staying below 2C at >66% through the 21st century"),
                tags$li("NPI2020_1600: NPi scenario until 2020 with a transition to a globally cost-effective implementation of a carbon budget for the period 2011-2100 of 1000 GtCO2 afterwards, corresponding to staying below 2C at >66% through the 21st century"),
                tags$li("NPI2020_400: NPi scenario until 2020 with a transition to a globally cost-effective implementation of a carbon budget for the period 2011-2100 of 400 GtCO2 afterwards, corresponding to a chance of >66% for staying below 1.5C in 2100"),
            ),
            
            # Sources
            uiOutput("mh6"),
            uiOutput("tabs"),
            br(), br(), br(),
            )
        )
    
)

# Define server 
server <- function(input, output) {
    
    output$mh0 <- renderUI(h3("Introduction"))
    output$mh1 <- renderUI(h3("Graphs of Input Variables"))
    output$mh2 <- renderUI(h3("Market Risk"))
    output$mh3 <- renderUI(h3("Relationship between GDP and DJIA"))
    output$mh4 <- renderUI(h3("Discussion Points"))
    output$mh5 <- renderUI(h3("Further Notes"))
    output$mh6 <- renderUI(h3("Sources"))
    
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
    
    output$return_plot <- renderPlot({
        plot_descriptives_return(djia(), input$select_return_type, input$select_return_period)
    })
    
    output$vola_plot_daily <- renderPlot({
        plot_djia_volatility(djia(), c(input$slider_time_daily[1]: input$slider_time_daily[2]), "daily")
    })
    
    output$vola_plot_quarterly <- renderPlot({
        plot_djia_volatility(djia(), c(input$slider_time_quarterly[1]: input$slider_time_quarterly[2]), "quarterly")
    }) 
    
    output$vola_plot2 <- renderPlot({
        plot_djia_volatility2(djia(), input$select_volatility_aggregation2)
    })
    
    output$regression_plot <- renderPlot({
        my_regression(djia(), gdp(), input$select_periodicity_regression)[[1]]
    })
    
    output$regression_summary <- renderPrint({
        my_regression(djia(), gdp(), input$select_periodicity_regression)[[2]]
    })
    
    output$regression_growth_plot <- renderPlot({
        my_regression_growth(djia(), gdp(), input$select_periodicity_regression, input$select_outlier)[[1]]
    })
    
    output$regression_growth_summary <- renderPrint({
        my_regression_growth(djia(), gdp(), input$select_periodicity_regression, input$select_outlier)[[2]]
    })
    

    output$selected_variables <- renderText({
        paste("You have selected to calculate the returns as ", input$select_return_type,
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
