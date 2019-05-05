library(dplyr)
library(ggplot2)
library(ggiraph)

get_streaks <- function(df){
  df %>% 
    mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), 
         grp = cumsum(opens)) %>% 
    group_by(grp) %>% 
    summarize(start_date = min(Date), streak_len = n()) 
}

get_dow_stats <- function(df){
  df %>% 
    mutate(Day =  lubridate::wday(Date, label = TRUE), 
           o = lubridate::wday(Date, week_start = 1)) %>%
    group_by(Day,o) %>% 
    summarize(Fastest = as.character(hms::hms(as.numeric(min(Duration)))), 
              Median = as.character(hms::hms(as.numeric(median(Duration)))),
              Boxplot = paste(round(as.numeric(Duration)/60),collapse = ","),
              Trend = paste(Date - min(df$Date), round(as.numeric(Duration)/60),sep = ":", collapse = ","),
              Vals = list(as.numeric(Duration)/60)) %>% 
    ungroup() %>% 
    arrange(o) %>% 
    select(-o)
}

time_break <- function(x){
  l <- 0
  u <- if (x[2] < lubridate::minutes(10)){
    hms::trunc_hms(x[2],60)
  } else {
    hms::trunc_hms(x[2],60*10)
  }
  c(l,u)
}

plot_over_time <- function(df, day = NULL){
  if (!is.null(day)){
    df <- df %>% filter((lubridate::wday(Date, label = TRUE) == day))
  }
  p <- df %>%  
    mutate(Day =  factor(lubridate::wday(Date, label = TRUE),
                         ordered = TRUE,
                         levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
           tooltip = glue::glue("Date: {Date} ({Day})<br>Solve Time: {Duration}<br>"),
           onclick = glue::glue("window.open(\"https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}\")")) %>%
    ggplot(aes(x = Date, y = Duration, color = Day)) + 
    geom_smooth(se = FALSE) +
    geom_point_interactive(aes(tooltip = tooltip, onclick = onclick)) + 
    scale_color_brewer(palette = "Dark2", drop = FALSE) +
    theme_bw() +
    ylab("Solve Time") 
  if (!is.null(day)) {
    p <- p + ggtitle(glue::glue("Solve Rates Over Time ({day})")) +
      guides(color = NULL)
  } else{
    p <- p + ggtitle("Solve Rates Over Time")
  }
  girafe(ggobj = p)
}


completion_calendar <- function(df){
  n_col <- 4
  df <- df %>% 
    mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), 
           grp = cumsum(opens),
           col = grp %% n_col,
           Day =  lubridate::wday(Date, label = TRUE),
           tooltip = glue::glue("Date: {Date} ({Day}) <br/> Solve Time: {Duration}<br>"),
           links = glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}")) 
  
  opt_list <- list(
    colors = RColorBrewer::brewer.pal(n_col, "Dark2"),
    min = 1,
    max = n_col,
    endDate = lubridate::today()
  )
  
  d3_file <- "calendar_plot.js"
  
  r2d3::r2d3(df, options = opt_list, script = d3_file, d3_version = 4, container = "div")
  
}


dow_summary_table <- function(df){
  df <- get_dow_stats(df)
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
