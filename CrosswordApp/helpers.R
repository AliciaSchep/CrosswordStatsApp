library(dplyr)
library(DT)
library(glue)

to_time_format <- function(x) {
  as.character(hms::hms(as.numeric(x)))
}

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

plot_streak_times <- function(df, width) {
  streak_grouped <- df %>% 
    mutate(
      Day = lubridate::wday(Date, label = TRUE),
      opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), grp = cumsum(opens)) %>%  
    group_by(grp) %>% 
    mutate(relative_day = row_number(), 
           total_duration = cumsum(as.numeric(Duration))) 
  
  streak_agg <- streak_grouped %>%
    summarise(`Streak Start` = min(Date), len = n()) %>%
    top_n(8, len)
    
  streak_data <- streak_agg %>% 
    inner_join(ungroup(streak_grouped), by = "grp") %>%
    mutate(`Solve Time` = to_time_format(Duration),
           `Total Time` = to_time_format(as_hms(total_duration))) %>%
    select(relative_day, `Streak Start`, Date, total_duration, Duration, `Solve Time`, `Total Time`)
  
  vl_chart(streak_data) %>%
    vl_mark_line(point = TRUE) %>%
    vl_encode_x("relative_day:Q", title = "Days since start of streak") %>%
    vl_encode_y("total_duration:Q", title = "Total solving time during streak (hrs)") %>%
    vl_encode_tooltip(c("Date","Solve Time","Total Time")) %>%
    vl_encode_color("Streak Start:N") %>%
    vl_axis_y(labelExpr = "round(datum.value / 3600)")  %>%
    vl_config_view(width = width * 0.95, height = min(width,350)) %>%
    vl_scale_color(scheme = "Dark2") %>%
    vegawidget()
}

plot_distributions <-  function(df, width, date_range, day_of_week){
  if (is.null(date_range)) return(NULL)
  
  plot_all <- day_of_week == "All"
  # Plot duration over time
  # df input should be crossword data with Date and Duration columns
  df_with_day <- df %>% 
    mutate(Day =  lubridate::wday(Date, label = TRUE),
    duration_minutes = as.numeric(Duration) / 60
  ) 
  
  df_most_recent <- df_with_day %>% 
    group_by(Day) %>% 
    top_n(1, Date) %>% 
    ungroup() %>%
    select(Date) %>%
    mutate(most_recent = TRUE)
  
  if (!plot_all) {
    df_with_day <- df_with_day %>% filter(
      Day == day_of_week
    )
  }
  

  chart_data <- df_with_day %>%
    filter(Date >= lubridate::as_date(date_range[1]),
           Date <=  lubridate::as_date(date_range[2])) %>%
    left_join(df_most_recent, by = "Date") %>%
    mutate(most_recent = coalesce(most_recent, FALSE),
           duration_plus = duration_minutes + runif(length(duration_minutes),0,1/60)) %>%
    group_by(Day) %>%
    mutate(percent_rank = 100 -percent_rank(duration_plus) * 100,
           rank = rank(duration_plus),
           total = n()) %>%#,
           #y_max = max(duration_minutes)) %>%
    ungroup() %>%
    mutate(text = if_else(most_recent,glue("Last Solve:\n{rank} out of {total}\n{to_time_format(Duration)}"),"")) %>% #,
           #x_label = 70) %>%
    select(Day, duration_minutes, most_recent, percent_rank,text) 
  
  bin_size <- chart_data %>% 
    group_by(Day) %>% 
    count() %>%
    ungroup() %>% 
    mutate(binwidth = 100 / (n-1))
  
  recent_df <- chart_data %>% 
    filter(most_recent) %>% 
    inner_join(bin_size, by = "Day") %>% 
    mutate(x = if_else(percent_rank == 0, 0, percent_rank - (binwidth / 2)),
           x2 = if_else(percent_rank == 100, 100, percent_rank + (binwidth / 2)) ,
           y = 0) %>% 
    select(Day, most_recent, x, x2, y)
  
  chart_data <- chart_data %>% 
    left_join(recent_df, by = c("Day","most_recent"))
   
  l1 <- vl_chart() %>% 
    vl_encode_y("duration_minutes:Q", title = "Solve Time (Minutes)") %>%
    vl_encode_x("percent_rank:Q", title = "Percentile") %>%
    vl_mark_area(interpolate = "step") 
  
  l2 <- vl_chart() %>%
    vl_mark_rect(color = 'red') %>% 
    vl_filter("datum.most_recent") %>%
    vl_encode_x("x:Q", title = "Percentile") %>%
    vl_encode_y("y:Q") %>%
    vl_encode_x2("x2") %>%
    vl_encode_y2("duration_minutes")
    
 
  
  if (plot_all) {
    # Use with to get number of columns
    view_width = 150
    view_height = 125
    ncol <- max(1, floor( width / (view_width * 1.1)))
    
    l3 <- vl_chart() %>%
      vl_filter("datum.most_recent") %>%
      vl_encode_x(value = 100) %>%
      vl_encode_y(value = 15) %>% 
      vl_encode_text("text:N") %>%
      vl_mark_text(lineBreak = '\n', color = 'red')
    
    dist_plot <-vl_layer(l1,l2, l3) %>%
      vl_add_data(chart_data) %>% 
      vl_facet_wrap(field = "Day", type = "nominal", columns = ncol, 
                    sort = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), title = NA,
                    header = list(labelFontStyle = "bold", labelFontSize = 16, labelPadding = 2)) %>%
      vl_resolve_axis_y(how = "independent") %>% 
      vl_resolve_scale_y(how = "independent") %>% 
      vl_resolve_axis_x(how = "independent") %>%
      vl_config_view(width = view_width, height = view_height) %>% 
      vl_config_axis(grid = FALSE)
  } else {
    l3 <- vl_chart() %>%
      vl_filter("datum.most_recent") %>%
      vl_encode_x(value = width * 0.8) %>%
      vl_encode_y(value = 25) %>% 
      vl_encode_text("text:N") %>%
      vl_mark_text(lineBreak = '\n', color = 'red',fontSize = 14)
    
    dist_plot <-vl_layer(l1,l2, l3) %>%
      vl_add_data(chart_data) %>%
      vl_config_view(width = width, height = min(width, 350)) %>% 
      vl_config_axis(grid = FALSE)
  }
  vegawidget(dist_plot)
}

plot_over_time <- function(df, width, window_func, window, date_range, day_of_week){
  if (is.null(date_range)) return(NULL)
  
  plot_all <- day_of_week == "All"
  # Plot duration over time
  # df input should be crossword data with Date and Duration columns
  df_with_day <- df %>% 
    mutate(Day =  lubridate::wday(Date, label = TRUE))
  
  if (!plot_all) {
    df_with_day <- df_with_day %>% filter(
      Day == day_of_week
    )
  }
  df_fastest <- df_with_day %>% 
    group_by(Day) %>% 
    summarize(fastest_duration = min(Duration))
  
  start_date <- max(lubridate::as_date(date_range[1]), min(df_with_day$Date))
  end_date <- lubridate::as_date(date_range[2])
  
  chart_data <- df_with_day %>% inner_join(df_fastest, by = "Day") %>% 
    mutate(
      fastest = Duration == fastest_duration,
      `Solve Time` = to_time_format(Duration),
      duration_seconds = as.numeric(Duration),
      links = as.character(glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}")),
      ) %>% 
    select(Date, duration_seconds, fastest, `Solve Time`, Day, links) %>%
    arrange(fastest) %>%
    #mutate(valid = TRUE) %>% 
    group_by(Day) %>%
    mutate(rank = min_rank(Date),
           valid = between(Date,start_date, end_date)) %>%
    ungroup()
  
  #start_date_vl <- glue("datetime({lubridate::year(start_date)}, {lubridate::month(start_date)}, {lubridate::day(start_date)})")
  #end_date_vl <- glue("datetime({lubridate::year(end_date)}, {lubridate::month(end_date)}, {lubridate::day(end_date)})")
  
  max_y <- max(filter(chart_data, Date >= start_date, Date <= end_date)$duration_seconds)
  
  if (plot_all) {
    
    # Problem -- these are not all by day... need to be.
    #min_rank <- min(filter(chart_data, valid)$rank) - window
    #min_rank2 <- max(2, min(filter(chart_data, valid)$rank) - 1)
    
    out <- vl_chart(chart_data) %>% 
      #vl_filter(glue("datum.rank >= {min_rank}")) %>%
      vl_window(frame = list(-(window - 1),0), 
                window = list(vl$Window(field = "duration_seconds", op = window_func, as = "rolling_func")), 
                sort = list(list("field"= "Date", "order"= "ascending")),
                groupby = list("Day")) %>%
      vl_filter("datum.rank > 2") %>%
      #vl_filter(glue("datum.rank >= {min_rank2}")) %>%
      vl_encode_x("Date:T") %>%
      vl_scale_x(domain = list(
        list(year = lubridate::year(start_date), month = lubridate::month(start_date), day = lubridate::day(start_date)),
        list(year = lubridate::year(end_date), month = lubridate::month(end_date), day = lubridate::day(end_date))
      )) %>%
      vl_encode_y("rolling_func:Q") %>%
      vl_scale_y(domain = list(0, max_y * 1.05)) %>%
      vl_axis_x(title = NA) %>%
      vl_axis_y(labelExpr = "round(datum.value / 60)", title = "Solve Time (minutes)") %>%
      vl_encode_color("Day:N") %>%
      vl_scale_color(domain = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), scheme = "Dark2") %>%
      vl_mark_line(interpolate = "monotone", clip = TRUE, tooltip = glue("Moving average (Last {window} points)")) %>%
      vl_config_view(width = width, height = min(width, 350)) %>%
      vegawidget()
  } else {
    l1 <- vl_chart() %>% 
      vl_window(frame = list(-(window - 1),0), 
                window = list(vl$Window(field = "duration_seconds", op = window_func, as = "rolling_func")), 
                sort = list(list("field"= "Date", "order"= "ascending")),
                groupby = list("Day")) %>%
      # Putting this filter after the window makes sure it only affects where plotting is happening not
      # the averaging... this is making sure that line is showing up after a few points
      vl_filter("datum.rank > 2") %>% 
      vl_filter(glue("datum.valid")) %>%
      vl_encode_x("Date:T") %>%
      vl_encode_y("rolling_func:Q") %>%
      vl_mark_line(interpolate = "monotone", tooltip = glue("Moving median (Last {window} points)"), clip = TRUE) 
    
    l2 <- vl_chart() %>% 
      vl_window(frame = list(NA,0), 
                window = list(vl$Window(field = "duration_seconds", op = "min", as = "rolling_min")), 
                sort = list(list("field"= "Date", "order"= "ascending")),
                groupby = list("Day")) %>%
      vl_filter(glue("datum.valid")) %>%
      vl_encode_x("Date:T") %>%
      vl_encode_y("rolling_min:Q") %>%
      vl_mark_area(interpolate = "step-after", color = 'red', opacity = 0.2, tooltip = "Record time", clip = TRUE)
    
    l3 <- vl_chart()  %>%
      vl_filter(glue("datum.valid")) %>%
      vl_encode_x("Date:T") %>%
      vl_encode_y("duration_seconds:Q") %>%
      vl_encode_shape(value = "circle") %>%
      vl_encode_color(value = "grey") %>%
      vl_encode_size(value = 40) %>%
      vl_condition_size(test = "datum.fastest",
                        value = 120) %>%
      vl_condition_color(test = "datum.fastest",
                         value = "red") %>%
      vl_condition_shape(test = "datum.fastest",
                         value = "M0,.5L.6,.8L.5,.1L1,-.3L.3,-.4L0,-1L-.3,-.4L-1,-.3L-.5,.1L-.6,.8L0,.5Z") %>%
      vl_encode_href("links:N") %>%
      vl_mark_point(filled = TRUE) %>%
      vl_axis_x(title = NA) %>%
      vl_axis_y(labelExpr = "round(datum.value / 60)", title = "Solve Time (minutes)") %>%
      vl_encode_tooltip(list(vl$Tooltip(field = "Date", type = "temporal"), vl$Tooltip(field = "Solve Time", type = "nominal")))
    
    layered <- vl_layer(l1, l2, l3) %>%
      vl_add_data(chart_data) %>%
      vl_config_view(width = width, height = min(width, 350))
    out <- vegawidget(layered) 
  }
  
  out
}

get_weeks_ago <- function(x) {
  t1 <- lubridate::today()
  t2 <- t1 + lubridate::days(8 - lubridate::wday(t1, week_start = 1))
  ceiling(lubridate::interval(x,t2) / lubridate::weeks(1)) - 1
}

get_start_of_week_month <- function(x) {
  lubridate::month(x - lubridate::days(lubridate::wday(x, week_start = 1) - 1), label = TRUE)
}

completion_calendar <- function(df, width){
  # Make calendar plot showing days of completion, colored by streak
  # df input should be crossword data with Date and Duration columns
  n_col <- 4
  df <- df %>% 
    mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), 
           grp = cumsum(opens),
           col = grp %% n_col + 1) 
  
  year_dates <- tibble(
    Date = seq(lubridate::today() - lubridate::weeks(52) + lubridate::days(8 - lubridate::wday(lubridate::today(), week_start = 1)), 
               lubridate::today(),
               by = '1 day')
  )
  
  chart_data <- year_dates %>% 
    left_join(df, by = 'Date') %>% 
    mutate(
      links = glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}"),
      Day =  lubridate::wday(Date, label = TRUE),
      week = lubridate::isoweek(Date),
            day_int = lubridate::wday(Date, week_start = 1),
           year = lubridate::year(Date),
           weeks_ago = get_weeks_ago(Date),
           week_index = weeks_ago %% 52,
           week_month = paste0(format(week_index,width = 2),get_start_of_week_month(Date)),
           year_index = weeks_ago %/% 52,
           month = get_start_of_week_month(Date),
           new_month = lag(month) != month,
           `Solve Time` = if_else(is.na(Duration), 'n/a', as.character(hms::hms(as.numeric(Duration)))),
           Date = glue::glue("{Date} ({Day})")) %>%
    filter(year_index == 0) %>%
    select(month, col, Date, `Solve Time`, links, week_index, Day, new_month)
  
  
  p1 <- vl_chart() %>% 
    vl_encode_color("col:N", legend = NA) %>%
    vl_scale_color(scheme = "Dark2") %>%
    vl_mark_rect(stroke = 'black') %>%
    vl_encode_tooltip(list(vl$Tooltip(field = "Date", type = "N"), vl$Tooltip(field = "Solve Time", type = "N"))) %>%
    vl_encode_href("links:N") %>%
    vl_condition_color(test = "datum.col == null", value = "white")
    
  p2 <- vl_chart() %>%
    vl_encode_text("month:N") %>%
    vl_filter("datum.Day == 'Mon' & datum.new_month") %>% 
    vl_mark_text(dy = -20)
    
  p3 <- vl_layer(p1,p2) %>% 
    vl_encode_x("week_index:O", title = NA) %>%
    vl_encode_y("Day:O", title = NA) %>%
    vl_add_data(chart_data) %>%
    vl_scale_y(domain = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) %>%
    vl_scale_x(domain = 51:0) %>%
    vl_axis_x(ticks = FALSE, labels = FALSE, domain = FALSE) %>% 
    vl_axis_y(ticks = FALSE, domain = FALSE) %>%
    vl_config_view(stroke = "transparent") %>% 
    vl_add_properties(height = "140", width = width)

  vegawidget(p3)
}
  
