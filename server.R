library(shiny)

# Load data processing file
source("preprocessing.R")
games <- sort(unique(data$game))

# Shiny server
shinyServer(
  function(input, output) {
    output$playerID <- renderText({input$playerID})
    
    output$address <- renderText({
        input$goButtonAdd
        isolate(paste("https://www.esportsearnings.com/search?search=", 
                input$playerID, sep=""))
        
    })
    
    openPage <- function(url) {
        return(tags$a(href=url, "Click", target="_blank"))
    }
    
    output$inc <- renderUI({ 
        input$goButtonDirect
        isolate(openPage(paste("https://www.esportsearnings.com/search?search=", 
                               input$playerID, sep="")))
    })
    
    # Initialize reactive values
    values <- reactiveValues()
    values$games <- games
    
    # Create games type checkbox
    output$gamesControl <- renderUI({
        checkboxGroupInput('games', 'Games:', 
                           games, selected = values$games)
    })
    
    # Add observer on select-all button
    observe({
        if(input$selectAll == 0) return()
        values$games <- games
    })
    
    # Add observer on clear-all button
    observe({
        if(input$clearAll == 0) return()
        values$games <- c() # empty list
    })

    # Prepare dataset
    dataTable <- reactive({
        groupBygame(data, input$year[1], 
                     input$year[2], input$earning[1],
                     input$earning[2], input$games)
    })

    dataTableBycountryByYear <- reactive({
        groupByYearAgg(data, input$year[1], 
                    input$year[2], input$earning[1],
                    input$earning[2], input$Nationality)
    })

    dataTableByearning <- reactive({
        groupByYearAll(data, input$year[1], 
                       input$year[2], input$earning[1],
                       input$earning[2], input$games)
    })

    dataTableByearningAvg <- reactive({
        groupByearningAvg(data, input$year[1], 
                        input$year[2], input$earning[1],
                        input$earning[2], input$games)
    })

    dataTableByearninggameAvg <- reactive({
        groupByearninggameAvg(data, input$year[1], 
                             input$year[2], input$earning[1],
                             input$earning[2], input$games)
    })
    
    # Prepare dataset for downloads
    #DownloaddataTable <- reactive({
    #    dataTable()
    #})
    
    
    # Render data table
    output$dTable <- renderDataTable({
        dataTable()
    } 
    )

    #output$downloadData <- downloadHandler(
    #    filename = 'data.csv',
    #    content = function(file) {
    #        write.csv(dataTable(), file)#, row.names=FALSE)
    #    }
    #)
    
    output$countryByYear <- renderChart({
        plotcountryCountByYear(dataTableBycountryByYear())
    })

    output$earningByYear <- renderChart({
        plotearningByYear(dataTableByearning())
    })

    output$earningByYearAvg <- renderChart({
        plotearningByYearAvg(dataTableByearningAvg())
    })

    output$earningBygameAvg <- renderChart({
        plotearningBygameAvg(dataTableByearninggameAvg())
    })
    
  } # end of function(input, output)
)