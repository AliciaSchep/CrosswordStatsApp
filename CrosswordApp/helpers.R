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

# plot_over_time <- function(df){
#   
#   df %>%  
#     mutate(Day =  factor(lubridate::wday(Date, label = TRUE),
#                          ordered = TRUE,
#                          levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
#     ggplot(aes(x = Date, y = Duration, color = Day)) + 
#     geom_smooth() +
#     geom_point() + 
#     facet_grid(rows = vars(Day), scales = "free_y") +
#     theme_bw() +
#     ylab("Solve Time") +
#     scale_color_brewer(palette = "Dark2", guide = FALSE) +
#     scale_y_time(breaks = time_break, 
#                  expand = expand_scale(add = c(0,0), 
#                                        mult = c(0,.15)))
#   
# }

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

# animation_by_dow <- function(df){
#   
#   tmp <- df %>%
#     mutate(day =  factor(lubridate::wday(Date, label = TRUE),
#                          ordered = TRUE,
#                          levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
#            week = paste0(lubridate::year(Date),'-',lubridate::week(Date))) 
#   
#   expanded <- purrr::map_dfr(unique(tmp$week), function(x){
#     tmp %>% filter(week <= x) %>% mutate(week2 = x)
#     })
#   
#   expanded %>% 
#     ggplot(aes(x = day, y = Duration, color = day)) + 
#     geom_boxplot() +
#     #geom_jitter(
#     #  position = position_jitter(width = 0.25, height = 0)) + 
#     scale_color_brewer(palette = "Dark2") +
#     theme_bw() + 
#     transition_states(week2) +
#     ease_aes("cubic-in-out")
# }



calendar_plot <- function(x, dates, fill,
                          tooltip = fill,
                          colors = RColorBrewer::brewer.pal(7, "BuPu"),
                          color_min = NULL,
                          color_max = NULL,
                          endDate = NULL,
                          legend = NULL){
  
  opt_list <- list(
    dates = dates,
    fill = fill,
    colors = colors,
    min = if (is.null(color_min)) min(x[[fill]]) else color_min,
    max = if (is.null(color_max)) max(x[[fill]]) else color_max,
    endDate = if (is.null(endDate)) lubridate::today() else endDate,
    tooltip = tooltip
  )
  
  d3_file <- "calendar_plot.js"
  
  r2d3::r2d3(x, options = opt_list, script = d3_file, d3_version = 4, container = "div")
  
}



completion_calendar <- function(df){
  n_col <- 4
  df <- df %>% 
    mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), 
           grp = cumsum(opens),
           col = grp %% n_col) 
  calendar_plot(df, "Date","col", tooltip = "Duration", colors = RColorBrewer::brewer.pal(n_col, "Dark2"))
}



