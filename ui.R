library(shiny)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(DT)

shinyUI(
    navbarPage("E-sports Top 100 Earnings Table", 
        tabPanel("The Data",
             sidebarPanel(
                sliderInput("year", 
                            "Time:", 
                            min = 2013,
                            max = 2016,
                            value = c(2013, 2016)),
                sliderInput("earning", 
                            "Earning:",
                            min = 0,
                            max = 6500000,
                            value = c(0, 6500000) 
                ),
                actionButton(inputId = "clearAll", 
                             label = "Clear selection", 
                             icon = icon("square-o")),
                actionButton(inputId = "selectAll",
                             label = "Select all",
                             icon = icon("check-square")),
                uiOutput("gamesControl")
             ),# end of sidebar Panel
             
             mainPanel(
                 tabsetPanel(
                   # Data 
                   tabPanel(p(icon("table"), "Dataset"),
                            dataTableOutput(outputId="dTable"),
                            downloadButton('downloadData', 'Download')
                   ), # end of "Dataset" tab panel
                   tabPanel(p(icon("line-chart"), "Visualize the Data"),
                            h4('Player Earning by Year', align = "center"),
                            h5('Hover over to see the details.', 
                               align ="center"),
                            showOutput("earningByYear", "nvd3"),
                            h4('Number of Coountries by Year', align = "center"),
                            h5('Hover over to see the details.', 
                               align ="center"),
                            showOutput("countryByYear", "nvd3"),
                            h4('Average Player Earning by Year', align = "center"),
                            h5('Hover over to see the details.', 
                               align ="center"),
                            showOutput("earningByYearAvg", "nvd3"),
                            h4('Average Earning by game', align = "center"),
                            h5('Hover over to see the details.', 
                               align ="center"),
                            showOutput("earningBygameAvg", "nvd3")
                   ) # end of "Visualize the Data" tab panel

                 )
                   
            )     
        ), # end of "The Data" tab panel
    
    tabPanel(p(icon("search"), "Search"),
             mainPanel(
                 h4("The page popped-up is the e-sports earning database on e-sportsearnings.com."),
                 h4("Step 1. Type the player ID below and press the 'Search' button:"),
                 textInput(inputId="playerID", label = "Input Player ID"),
                 actionButton("goButtonAdd", "Search"),
                 h5('Output Address:'),
                 textOutput("address"),
                 p(""),
                 h4("Step 2. Click the button below and the link to the player's page is being generated."),
                 p(""),
                 actionButton("goButtonDirect", "Link"),
                 p(""),
                 htmlOutput("inc"),
                 p("# Just a test page in local device......Don't expect it will work in shinyapp.io")
             )
        ),# end of "Search" tab panel
        
        tabPanel("About",
                 mainPanel(
                   includeMarkdown("about.md")
                 )
        ) # end of "About" tab panel
    )  
)
