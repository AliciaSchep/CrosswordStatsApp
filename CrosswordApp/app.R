library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(sparkline)
library(hms)
library(vlbuildr)
library(vegawidget)
source("helpers.R")

DATA_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSHYo_DBWW53tMB-eezEaq1jXWy4Sr8QDsOR9ZtGQQrXQhPN6cpgEHbWcDB20D_p6O-HD3Pefscub9L/pub?gid=0&single=true&output=csv"

# Hacky approach for detecting click... using instead of an actionButton so that formatting is same as all the menu items
refresh_menu <- menuItem("Refresh", href = "#", icon = icon("refresh"), newtab = FALSE)
refresh_menu$children[[1]]$attribs['onclick'] <- "Shiny.onInputChange('refreshLink','refresh'); return false"

# Script for getting dimensions... hacky way of handling resize for now
resize <-'
var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});'

sidebarWidth <- 250

ui <- dashboardPage(
  dashboardHeader(title = "Crossword Stats"),
  dashboardSidebar(
    tags$head(tags$script(resize)),
    sidebarMenu(
      menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
      menuItem("Trends", tabName = "Trends", icon = icon("chart-line")),
      menuItem("Streaks", tabName = "Streaks", icon = icon("trophy")),
      menuItem("About", tabName = "About", icon = icon("question")),
      menuItem("Source", href = "https://github.com/AliciaSchep/CrosswordStatsApp", icon = icon("code")),
      refresh_menu
    ),
    width = sidebarWidth
  ),
  dashboardBody(
    tabItems(
      tabItem("Summary",
          fluidRow(
            withSpinner(valueBoxOutput("solveNumber", width = 4), proxy.height = "100px"),
            valueBoxOutput("currentStreak", width = 4),
            valueBoxOutput("longestStreak", width = 4)
          ),
          fluidRow(
            box(DTOutput('dailyStats'), width = 12,
              title = "Summary by Day-of-Week")
          ),
          fluidRow(
            box(vegawidgetOutput("completionCalendar"), 
                p("Hover over square to see date and completion time; click to go to puzzle (requires NYT Crosswords subscription)"),
              width = 12,
              title = "Puzzle Completions in Past Year (Colored by Streak)")
          )
      ),
      tabItem("Trends",
           fluidRow(
             box(radioButtons("dayOfWeek", "Day of Week", 
                              choices = c("All", "Mon", "Tue", "Wed",
                                          "Thu", "Fri", "Sat", "Sun"),
                              selected = "All", inline = TRUE), width = 4),
             box(uiOutput("dateSlider"), width = 4),
             box(sliderInput("smoothWindow", "Moving Average Window:",
                             min = 1, max = 25,
                             value = 10, step = 1), width = 4)
           ),
           fluidRow(
            box(vegawidgetOutput("trendPlot"),
                p("On single day plots, hover over point to see date and completion time; click to go to puzzle (requires NYT Crosswords subscription)"),
                width = 12)
          )),
      tabItem("Streaks",
          fluidRow(
            box(vegawidgetOutput('streakPlot'), title = "Streak lengths & duration", width = 12)
          )
      ),
      tabItem("About",
          fluidRow(
            box(
              h3("What"),
              p("This is a dashboard showing my stats for the NYT Crossword. Only puzzles solved same-day (valid for streak) are shown.",
                " The data was manually (\U0001f620) entered into a google sheet that is read in by this dashboard."),
              h3("Who"),
              p("This dashboard was created by Alicia Schep. Most of these puzzled were solved in collaboration with my partner."),
              h3("Why"),
              p("I am quite frustrated by the lack of detail on the NYT's Crossword statistics page ",
                "(and down-right outraged by the improper scaling of the bar chart). I want to be able to understand whether I am getting",
                " faster and see trends by day of week. This dashboard is meant to showcase some more useful visualizations for crossword statistics!"),
              h3("Acknowledgments"),
              p("This dashboard was made possible by open source projects, including shiny, DT, sparkline, shinydashboard, vegawidget, andvlbuildr. ",
                "Thanks also to those who have contributed documentation and blogs on using those tools; this", 
                a("post by Matt Leonawicz",
                  href="https://leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html"),
              "was especially helpful for getting the sparklines into the DT table."),
              p(" And thanks to the NYT & contributors for providing fun & challenging puzzles... ",
              "although I hope more investment in the Crossword statistics page will be forthcoming \U0001F600"),
              width = 12
            )
          )
          )
    )
  )
  ,
  skin = "purple"
)




server <- function(input, output, session) {
  
  c_data <- reactive({
    # Re-read the data either after refresh button pressed or enought time elapsed
    input$refreshLink
    invalidateLater(1000000)
    readr::read_csv(DATA_URL)
  })
  
  output$dateSlider <- renderUI({
    min_date = min(c_data()$Date)
    max_date = max(c_data()$Date)
    sliderInput("dateRange", "Date Range:", min = min_date, max = max_date, value = c(min_date, max_date))
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
    
    cd <- c_data()
    if (max(cd$Date) < lubridate::today() - lubridate::days(2)){
      # Using -2 to give some wiggle room for delayed data entry, timezones, etc.
      current_streak <- 0
    } else{
      current_streak <- tail(get_streaks(c_data()),1)$streak_len
    }
    
    valueBox(
      value = current_streak,
      subtitle = "Current Streak Length",
      icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  output$longestStreak <- renderValueBox({
    max_streak <- max(get_streaks(c_data())$streak_len)
    valueBox(
      value = max_streak,
      subtitle = "Max Streak Length",
      icon = icon("certificate"),
      color = "purple"
    )
  })
  
  output$dailyStats <- renderDT({
    dow_summary_table(c_data())
  })
  
  output$streakPlot <- renderVegawidget(
    quote(
      plot_streak_times(
        c_data(), 
        ifelse(input$sidebarCollapsed, input$dimension[1], input$dimension[1] - sidebarWidth) * 0.75)
      ),
    quote = TRUE
  )
  
  output$trendPlot <- renderVegawidget(
    quote(
      plot_over_time(
        c_data(), 
        ifelse(input$sidebarCollapsed, input$dimension[1], input$dimension[1] - sidebarWidth) * 0.75, 
        input$smoothWindow,
        input$dateRange,
        input$dayOfWeek)),
    quote = TRUE
    )
  
  
  output$completionCalendar <- renderVegawidget(
    quote(completion_calendar(c_data(), as.character(floor(ifelse(input$sidebarCollapsed, input$dimension[1], input$dimension[1] - sidebarWidth) * 0.8)))), 
    quoted = TRUE
  )

}

shinyApp(ui, server)
