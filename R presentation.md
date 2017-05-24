R presentation - Shiny App for E-sports Salary Table
========================================================
author: Yi-Yang Lin
date: 05/24/2017
autosize: true

Introduction
========================================================

Since E-sports market grows much dast than we thought, this app is used to create some graphs to examine these players' salaries. With visualized data, we can make the business strategy more easily.

For more details about the earning:
<https://www.esportsearnings.com/>.

You can also try the App in the following link:
<https://yi-yanglin.shinyapps.io/salaryApp/>.

- Server process
- Code
- Plot

Server process - Average Earning by game for Example
========================================================


```r
# We create a temporary R code script to store all the function we may need in the App.  
# Then we create the appropriate data table, plot function for Average Earning by game,  and then combine them together.

source("preprocessing.R")
library(shiny)

dataTableByearninggameAvg <- reactive({
    groupByearninggameAvg(data, input$year[1], input$year[2], input$earning[1],
                          input$earning[2], input$games)
    })
```

Code - Average Earning by game for Example
========================================================


```r
plotearningBygameAvg <- function(dt, dom = "earningBygameAvg", xAxisLabel = "games", yAxisLabel = "Number of earning") {
    earningBygameAvg <- nPlot(avgearning ~ game,data = dt,type = "multiBarChart",dom = dom, width = 650);
    earningBygameAvg$chart(margin = list(left = 100));
    earningBygameAvg$chart(color = c('green', 'blue', 'red'));
    earningBygameAvg$yAxis(axisLabel = yAxisLabel, width = 80);
    earningBygameAvg$xAxis(axisLabel = xAxisLabel, width = 200,rotateLabels = -20, height = 200);earningBygameAvg
    }
```

Plot
========================================================


```r
earningBygameAvg <- renderChart({
    plotearningBygameAvg(dataTableByearninggameAvg())
    })

earningBygameAvg 
```

```
function () 
{
    rChart_ <- func()
    cht_style <- sprintf("<style>.rChart {width: %spx; height: %spx} </style>", 
        rChart_$params$width, rChart_$params$height)
    HTML(paste(c(cht_style, rChart_$html()), collapse = "\n"))
}
<environment: 0x0000000017ab2d58>
```


