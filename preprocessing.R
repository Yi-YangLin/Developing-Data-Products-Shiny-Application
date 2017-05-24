# Load required libraries
require(data.table)
library(dplyr)
library(DT)
library(rCharts)

# Read data
data <- fread("./data/salary.csv")

head(data)
colnames(data) 
setnames(data, "Player_ID", "playerID")
setnames(data, "Year", "year")
setnames(data, "Game", "game")
setnames(data, "Total_Year", "earning")
sum(is.na(data)) 
years <- sort(unique(data$year))
games <- sort(unique(data$game))


#' Aggregate dataset by year, earning and game
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minearning
#' @param maxearning
#' @param games
#' @return data.table
#'
groupByYearAll <- function(dt, minYear, maxYear, minearning,
                             maxearning, games) {
    result <- dt %>% filter(year >= minYear, year <= maxYear,
                            earning >= minearning, earning <= maxearning,
                            game %in% games) 
    return(result)
}

#' Aggregate dataset only by year
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @return data.table
#'
groupByYear <- function(dt, minYear, maxYear) {
    result <- dt %>% filter(year >= minYear, year <= maxYear) 
    return(result)
}

#' Aggregate dataset by games
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minearning
#' @param maxearning
#' @param games
#' @return result data.table
#' 
groupBygame <- function(dt, minYear, maxYear, 
                         minearning, maxearning, games) {
    # use pipelining
    dt <- groupByYearAll(dt, minYear, maxYear, minearning,
                           maxearning, games) 
    result <- datatable(dt, options = list(iDisplayLength = 50))
    return(result)
}

#' Aggregate dataset by year to get total count of games
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minearning
#' @param maxearning
#' @param games
#' @return data.table 2 columns
#'
groupByYearAgg <- function(dt, minYear, maxYear, minearning,
                           maxearning, Nationality) {
    dt <- groupByYear(dt, minYear, maxYear)
    result <- dt %>% 
            group_by(year)  %>% 
            summarise(count = n_distinct(Nationality)) %>%
            arrange(year)
    return(result)
}

#' Aggregate dataset by year to get total count of average number of earning
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minearning
#' @param maxearning
#' @param games
#' @return data.table 2 columns
#'
groupByearningAvg <- function(dt,  minYear, maxYear, minearning,
                            maxearning, games) {
    dt <- groupByYearAll(dt, minYear, maxYear, minearning,
                           maxearning, games)
    result <- dt %>% 
            group_by(year) %>% 
            summarise(avg = mean(earning)) %>%
            arrange(year)
    return(result)      
}

#' Average earning for each game
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minearning
#' @param maxearning
#' @param games
#' @return data.table 2 columns
#'
groupByearninggameAvg <- function(dt, minYear, maxYear, minearning,
                                 maxearning, games) {
    dt <- groupByYearAll(dt, minYear, maxYear, minearning,
                           maxearning, games)
    result <- dt %>% 
            group_by(game) %>%
            summarise(avgearning = mean(earning)) %>%
            arrange(game)
    return(result)
}

#' Plot number of earning by year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of earning
#' @return plotearningByYear plot
plotearningByYear <- function(dt, dom = "earningByYear", 
                             xAxisLabel = "Year", 
                             yAxisLabel = "Number of earning") {
    earningByYear <- nPlot(
        earning ~ year,
        data = dt,
        type = "scatterChart",
        dom = dom, width = 650
    )
    earningByYear$chart(margin = list(left = 100), 
                       showDistX = TRUE,
                       showDistY = TRUE)
    earningByYear$chart(color = c('red', 'orange', 'blue'))
    earningByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
  return '<h5><b>Country</b>: ' + e.point.Nationality + '<br>'
    + '<b>Player ID</b>: ' + e.point.playerID + '<br>'
    + '<b>Game</b>: ' + e.point.game
    + '</h5>'
} !#")
    earningByYear$yAxis(axisLabel = yAxisLabel, width = 80)
    earningByYear$xAxis(axisLabel = xAxisLabel, width = 70)
    earningByYear
}

#' Plot number of country by year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of games
#' @return countryByYear plot
plotcountryCountByYear <- function(dt, dom = "countryByYear", 
                                   xAxisLabel = "Year",
                                   yAxisLabel = "Number of countries") {
    countryByYear <- nPlot(
        count ~ year,
        data = dt,
        type = "lineWithFocusChart",
        dom = dom, width = 650
    )
    countryByYear$chart(margin = list(left = 100))
    countryByYear$yAxis(axisLabel = yAxisLabel, width = 80)
    countryByYear$xAxis(axisLabel = xAxisLabel, width = 70)
    countryByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
  return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Countries</b>: ' + e.point.count + '<br>'
    + '</h5>'
} !#")
    countryByYear
}

#' Plot number of average earning by year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of earning
#' @return earningByYearAvg plot
plotearningByYearAvg <- function(dt, dom = "earningByYearAvg", 
                             xAxisLabel = "Year",
                             yAxisLabel = "Number of earning") {

    earningByYearAvg <- nPlot(
        avg ~ year,
        data = dt,
        type = "lineChart",
        dom = dom, width = 650
    )
    earningByYearAvg$chart(margin = list(left = 100))
    earningByYearAvg$chart(color = c('orange', 'blue', 'green'))
    earningByYearAvg$yAxis(axisLabel = yAxisLabel, width = 80)
    earningByYearAvg$xAxis(axisLabel = xAxisLabel, width = 70)
    earningByYearAvg
}

#' Plot number of average earning by game
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel game
#' @param yAxisLabel number of earning
#' @return earningBygameAvg plot
plotearningBygameAvg <- function(dt, dom = "earningBygameAvg", 
                                 xAxisLabel = "games", 
                                 yAxisLabel = "Number of earning") {
    earningBygameAvg <- nPlot(
        avgearning ~ game,
        data = dt,
        type = "multiBarChart",
        dom = dom, width = 650
    )
    earningBygameAvg$chart(margin = list(left = 100))
    earningBygameAvg$chart(color = c('green', 'blue', 'red'))
    earningBygameAvg$yAxis(axisLabel = yAxisLabel, width = 80)
    earningBygameAvg$xAxis(axisLabel = xAxisLabel, width = 200,
                           rotateLabels = -20, height = 200)
    earningBygameAvg
}






