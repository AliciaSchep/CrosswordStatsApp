library(shiny)
library(shinydashboard)
library(DT)
library(r2d3)
source("helpers.R")

DATA_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSHYo_DBWW53tMB-eezEaq1jXWy4Sr8QDsOR9ZtGQQrXQhPN6cpgEHbWcDB20D_p6O-HD3Pefscub9L/pub?gid=0&single=true&output=csv"

ui <- dashboardPage(
    dashboardHeader(title = "Crossword Stats"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
            menuItem("Trends", tabName = "Trends", icon = icon("chart-line")),
            menuItem("About", tabName = "About", icon = icon("question"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("Summary",
                    fluidRow(
                        valueBoxOutput("solveNumber", width = 4),
                        valueBoxOutput("currentStreak", width = 4),
                        valueBoxOutput("longestStreak", width = 4)
                    ),
                    fluidRow(
                        box(DTOutput('dailyStats'), width = 12,
                            title = "Summary by Day-of-Week")
                    ),
                    fluidRow(
                        box(d3Output("completionCalendar", height = "150px"), 
                            width = 12,
                            title = "Puzzle Completions in Past Year (Colored by Streak)")
                    )
            ),
            tabItem("Trends",
                   fluidRow(
                       tabBox(
                           tabPanel('All',ggiraphOutput('trendPlot', height = "600px")),
                           tabPanel('Monday',ggiraphOutput('trendPlotMon', height = "600px")),
                           tabPanel('Tuesday',ggiraphOutput('trendPlotTue', height = "600px")),
                           tabPanel('Wednesday',ggiraphOutput('trendPlotWed', height = "600px")),
                           tabPanel('Thursday',ggiraphOutput('trendPlotThu', height = "600px")),
                           tabPanel('Friday',ggiraphOutput('trendPlotFri', height = "600px")),
                           tabPanel('Saturday',ggiraphOutput('trendPlotSat', height = "600px")),
                           tabPanel('Sunday',ggiraphOutput('trendPlotSun', height = "600px")),
                           width = 12
                    ))),
            tabItem("About",
                    fluidRow(
                        box(
                            h3("What"),
                            p("This is a dashboard showing stats for NYT Crossword. Only puzzles solved same-day (valid for streak) are shown. The data was manually \U0001f620 entered into a google sheet that is read in by this dashboard."),
                            h3("Who"),
                            p("Dashboard created by Alicia Schep. Most of these puzzled were solved in collaboration with my partner."),
                            h3("Why"),
                            p("As a professional data person, I am quite frustrated by the lack of detail on the NYT's Crossword statistics page (and down-right outraged by the improper scaling of the barchart). I want to be able to understand whether I am getting faster, and other trends by day of week. This dashboard is meant to showcase some more informative visualizations possible!"),
                            h3("Acknowledgments"),
                            p("This dashboard was made possible by open source projects, including shiny, shinydashboard, ggplot2, ggiraph, and r2d3.  And thanks to the NYT for providing fun & challenging puzzles... although I hope more investment in the Crossword statistics page will be forthcoming \U0001F600"),
                            width = 12
                        )
                    )
                    )
        )
    )
    ,
    skin = "purple"
)




server <- function(input, output) {
    
    c_data <- reactive({
        invalidateLater(1000000)
        readr::read_csv(DATA_URL)
    })
    
    output$solveNumber <- renderValueBox({
        
        valueBox(
            value = nrow(c_data()),
            subtitle = "Puzzles Solved (same day)",
            icon = icon("area-chart"),
            color = "purple"
        )
    })
    
    output$currentStreak <- renderValueBox({
        
        
        current_streak <- tail(get_streaks(c_data()),1)$streak_len
        
        
        valueBox(
            value = current_streak,
            subtitle = "Current Streak Length",
            icon = icon("area-chart"),
            color = "purple"
        )
    })
    
    output$longestStreak <- renderValueBox({
        
        
        max_streak <- max(get_streaks(c_data())$streak_len)
        
        
        valueBox(
            value = max_streak,
            subtitle = "Max Streak Length",
            icon = icon("area-chart"),
            color = "purple"
        )
    })
    
    output$dailyStats <- renderDT({
        df <- get_dow_stats(c_data())
        r <- range(df$Vals)
        
        cd <- list(list(targets = 3, 
                        render = JS("function(data, type, full){ return '<span class=sparkBox>' + data + '</span>' }")), 
                   list(targets = 4,
                        render = JS("function(data, type, full){ return '<span class=sparkLine>' + data + '</span>' }")))
        box_string <- paste0("type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black',",
            "chartRangeMin: ", r[1], ", chartRangeMax: ", r[2])
        line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"

        cb <- JS(paste0("function (oSettings, json) {
          $('.sparkLine:not(:has(canvas))').sparkline('html', { ", 
                        line_string, " });
          $('.sparkBox:not(:has(canvas))').sparkline('html', { ", 
                        box_string, " });
        }"), collapse = "")

        d <- datatable(df[,1:5], rownames = FALSE, 
                        options = list(columnDefs = cd, fnDrawCallback = cb, paging = FALSE, searching = FALSE,
                                       ordering = FALSE),
                       autoHideNavigation = TRUE)

        d$dependencies <- append(d$dependencies, htmlwidgets:::getDependency("sparkline"))
        d
    }
    )
    
    output$trendPlot <- renderggiraph({
        plot_over_time(c_data())
    })
    
    output$trendPlotMon <- renderggiraph({
            plot_over_time(c_data(),"Mon")
        })
    
    output$trendPlotTue <- renderggiraph({
        plot_over_time(c_data(),"Tue")
    })
    
    output$trendPlotWed <- renderggiraph({
        plot_over_time(c_data(),"Wed")
    })
    
    output$trendPlotThu <- renderggiraph({
        plot_over_time(c_data(),"Thu")
    })
    
    output$trendPlotFri <- renderggiraph({
        plot_over_time(c_data(),"Fri")
    })
    
    output$trendPlotSat <- renderggiraph({
        plot_over_time(c_data(),"Sat")
    })
    
    output$trendPlotSun <- renderggiraph({
        plot_over_time(c_data(),"Sun")
    })
    
    
    output$completionCalendar <- renderD3({
        completion_calendar(c_data())
    })
    
}

shinyApp(ui, server)
