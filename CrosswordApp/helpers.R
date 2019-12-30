library(dplyr)
library(ggplot2)
library(ggiraph)
library(DT)

get_streaks <- function(df){
  # Take in dataframe of stats (with columns Date and Duration)
  # return dataframe with streaks -- sindex, tart date for streak, and streak len
  df %>% 
    mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), 
         grp = cumsum(opens)) %>% 
    group_by(grp) %>% 
    summarize(start_date = min(Date), streak_len = n()) 
}

get_dow_stats <- function(df){
  # Take in dataframe of stats (with columns Date and Duration)
  # return a summary table with stats
  # Some of the columns are raw data meant to be used by sparkline
  # as in dow_summary_table function
  df %>% 
    mutate(Day =  lubridate::wday(Date, label = TRUE), 
           o = lubridate::wday(Date, week_start = 1)) %>%
    group_by(Day,o) %>% 
    summarize(Fastest = as.character(hms::hms(as.numeric(min(Duration)))), 
              Median = as.character(hms::hms(as.numeric(median(Duration)))),
              Boxplot = paste(round(as.numeric(Duration)
                                    ),collapse = ","),
              Trend = paste(Date - min(df$Date), round(as.numeric(Duration)#/60
                                                       ),sep = ":", collapse = ","),
              Vals = list(as.numeric(Duration)
                          )) %>% 
    ungroup() %>% 
    arrange(o) %>% 
    select(-o)
}

dow_summary_table <- function(df){
  # Make a DT table with summary stats.
  # calls get_dow_stats for computation of summary stats
  # df input should be crossword data with Date and Duration columns
  df <- get_dow_stats(df)
  r <- range(df$Vals)
  
  cd <- list(list(targets = 3, 
                  render = JS("function(data, type, full){ return '<span class=sparkBox>' + data + '</span>' }")), 
             list(targets = 4,
                  render = JS("function(data, type, full){ return '<span class=sparkLine>' + data + '</span>' }")))
  box_string <- paste0("type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black',",
                       "chartRangeMin: ", r[1], ", chartRangeMax: ", r[2],
                       ", numberFormatter: function(x){ var d = new Date(null); d.setSeconds(x); var ts = d.toISOString().substr(11,8); return ts;}") 
  line_string <- paste0("type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'",
                        ", numberFormatter: function(x){ var d = new Date(null); d.setSeconds(x); var ts = d.toISOString().substr(11,8); return ts;}") 
  
  
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

plot_streak_times <- function(df) {
  streak_data <- df %>% 
    mutate(
      Day = lubridate::wday(Date, label = TRUE),
      opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), grp = cumsum(opens)) %>%  
    group_by(grp) %>% 
    mutate(relative_day = row_number(), 
           total_duration = as.hms(cumsum(as.numeric(Duration)))) %>% 
    ungroup() %>%
    mutate(tooltip =  glue::glue("Date: {Date} ({Day})<br>Solve Time: {Duration}<br>Streak Day: {relative_day}<br>Total Streak Duration: {total_duration}"))
  
  p <- ggplot(streak_data, aes(x = relative_day, y = total_duration, group = grp, color = grp)) + 
    geom_line() + 
    xlab("Days since start of streak") + 
    ylab("Total solving time during streak") + 
    theme_bw(14) + 
    scale_color_gradientn(colors = RColorBrewer::brewer.pal(9,"YlOrRd")[3:9], breaks = function(x) x, labels = c('Long ago', 'Recent')) + 
    geom_point_interactive(aes(tooltip = tooltip)) + 
    theme(legend.position = c(0.8,0.1), legend.direction = "horizontal", legend.title = element_blank())
  
  girafe(ggobj = p)
}

plot_record_over_time <- function(df) {
  records <-  df %>% 
    mutate(Day =  factor(lubridate::wday(Date, label = TRUE),
                        ordered = TRUE,
                        levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
    group_by(Day) %>% 
    mutate(min = as.hms(cummin(as.numeric(Duration)), units = "secs")) 
  
  only_new <- records %>% 
    filter(min != lag(min) | is.na(lag(min))) %>%
    ungroup() %>%
    mutate(tooltip = glue::glue("Date: {Date} ({Day})<br>Record Time: {min}"))
  
  p <- records %>% ungroup() %>%
    ggplot(aes(x = Date, y = min, color = Day)) + 
    geom_step() +
    geom_point_interactive(data = only_new, aes(tooltip = tooltip)) +
    theme_bw(14) + 
    scale_color_brewer(palette = "Dark2", drop = FALSE) + 
    ylab('Current Record Time')
  
  girafe(ggobj = p)
}


plot_over_time <- function(df, day = NULL){
  # Plot duration over time
  # df input should be crossword data with Date and Duration columns
  # If day argument is provided, only plot for single day
  if (!is.null(day)){
    df <- df %>% filter((lubridate::wday(Date, label = TRUE) == day))
  }
  df <- df %>%  
    mutate(Day =  factor(lubridate::wday(Date, label = TRUE),
                         ordered = TRUE,
                         levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
  df_fastest <- df %>% group_by(Day) %>% summarize(fastest_duration = min(Duration))
  df <-  df %>% 
      inner_join(df_fastest, by = "Day") %>% 
      mutate(
           fastest = if_else(Duration == fastest_duration, glue::glue("Fastest {Day} Solve Time!"),""),
           tooltip = glue::glue("Date: {Date} ({Day})<br>Solve Time: {Duration}<br>{fastest}"),
           onclick = glue::glue("window.open(\"https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}\")"),
           Symbol = factor("Single Puzzle", levels = c("Single Puzzle","Trend","Fastest Solve")))
  

  p <- ggplot(df, aes(x = Date, y = Duration, color = Day, shape = Symbol, lty = Symbol)) + 
    geom_point(data = filter(df, Duration == fastest_duration), shape = 1, size = 4) +
    geom_smooth(se = FALSE) + 
    scale_y_time(labels = function(x){strftime(x,'%H:%M')}, limits = c(0,NA)) +
    geom_point_interactive(aes(tooltip = tooltip, onclick = onclick)) + 
    scale_color_brewer(palette = "Dark2", drop = FALSE) +
    theme_bw(14) +
    ylab("Solve Time (Hours:Minutes)") + 
    scale_shape_discrete(drop = FALSE, name = '') +
    scale_linetype_discrete(drop = FALSE, name = '') +
    guides(color = guide_legend(override.aes = list(shape = 15, linetype = 'blank')),
           shape = guide_legend(override.aes = list(shape = c(16, NA, 1), 
                                                    linetype = c('blank','solid','blank'),
                                                    size = c(2,1,4),
                                                    color = 'black')))
  if (!is.null(day)) {
    p <- p + ggtitle(glue::glue("Solve Time Trend ({day})")) 
  } else{
    p <- p + ggtitle("Solve Time Trends by Day of Week")
  }
  girafe(ggobj = p)
}


completion_calendar <- function(df){
  # Make calendar plot showing days of completion, colored by streak
  # df input should be crossword data with Date and Duration columns
  n_col <- 4
  df <- df %>% 
    mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), 
           grp = cumsum(opens),
           col = grp %% n_col + 1,
           Day =  lubridate::wday(Date, label = TRUE),
           tooltip = glue::glue("Date: {Date} ({Day}) <br/> Solve Time: {Duration}<br>"),
           links = glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}")) 
  
  opt_list <- list(
    colors = RColorBrewer::brewer.pal(n_col, "Dark2"),
    min = 1,
    max = n_col,
    endDate = max(c(lubridate::today(),max(df$Date))) + lubridate::days(1)
  )
  
  d3_file <- "calendar_plot.js"
  
  r2d3::r2d3(df, options = opt_list, script = d3_file, d3_version = 4, container = "div")
  
}


