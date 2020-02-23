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

plot_over_time <- function(df, width, window, date_range, day_of_week){
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
  chart_data <- df_with_day %>% inner_join(df_fastest, by = "Day") %>% 
    mutate(
      fastest = Duration == fastest_duration,
      `Solve Time` = to_time_format(Duration),
      duration_seconds = as.numeric(Duration),
      links = as.character(glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}")),
      ) %>% 
    select(Date, duration_seconds, fastest, `Solve Time`, Day, links) %>%
    arrange(fastest) %>%
    mutate(valid = TRUE) %>% 
    group_by(Day) %>%
    mutate(rank = min_rank(Date)) %>%
    ungroup()
  
  ## a hack to get the area to line up
  min_date <- min(chart_data$Date)
  min_df <- chart_data %>% 
    group_by(Day) %>% 
    top_n(1, desc(Date)) %>% 
    ungroup() %>%
    mutate(Date = min_date,
           valid = FALSE)
  
  max_date <- max(chart_data$Date)
  max_df <- chart_data %>% 
    group_by(Day) %>% 
    top_n(1, Date) %>% 
    ungroup() %>%
    mutate(Date = max_date,
           valid = FALSE)
  
  chart_data <- bind_rows(chart_data, min_df, max_df)
  
  start_date <- lubridate::as_date(date_range[1])
  end_date <- lubridate::as_date(date_range[2])
  
  start_date_vl <- glue("datetime({lubridate::year(start_date)}, {lubridate::month(start_date)}, {lubridate::day(start_date)})")
  end_date_vl <- glue("datetime({lubridate::year(end_date)}, {lubridate::month(end_date)}, {lubridate::day(end_date)})")
  
  
  l1 <- vl_chart() %>% 
    vl_filter("datum.valid") %>%
    vl_window(frame = list(-(window - 1),0), 
              window = list(vl$Window(field = "duration_seconds", op = "mean", as = "rolling_mean")), 
              sort = list(list("field"= "Date", "order"= "ascending")),
              groupby = list("Day")) %>%
    # Putting this filter after the window makes sure it only affects where plotting is happening not
    # the averaging... this is making sure that line is showing up after a few points
    vl_filter("datum.rank > 2") %>% 
    vl_filter(glue("datum.Date >= {start_date_vl}")) %>%
    vl_filter(glue("datum.Date <= {end_date_vl}")) %>%
    vl_encode_x("Date:T") %>%
    vl_encode_y("rolling_mean:Q") %>%
    vl_mark_line(interpolate = "monotone", tooltip = glue("Moving average (Last {window} points)")) 
  
  l2 <- vl_chart() %>% 
    vl_window(frame = list(NA,0), 
              window = list(vl$Window(field = "duration_seconds", op = "min", as = "rolling_min")), 
              sort = list(list("field"= "Date", "order"= "ascending")),
              groupby = list("Day")) %>%
    vl_filter(glue("datum.Date >= {start_date_vl}")) %>%
    vl_filter(glue("datum.Date <= {end_date_vl}")) %>%
    vl_encode_x("Date:T") %>%
    vl_encode_y("rolling_min:Q") %>%
    vl_mark_area(interpolate = "step-after", color = 'red', opacity = 0.2, tooltip = "Record time")
  
  l3 <- vl_chart() %>%
    vl_filter(glue("datum.Date >= {start_date_vl}")) %>%
    vl_filter(glue("datum.Date <= {end_date_vl}")) %>%
    vl_encode_x("Date:T") %>%
    vl_encode_y("duration_seconds:Q") %>%
    vl_encode_shape(value = "circle") %>%
    vl_encode_color(value = "grey") %>%
    vl_encode_size(value = 25) %>%
    vl_condition_size(test = "datum.fastest",
                       value = 100) %>%
    vl_condition_color(test = "datum.fastest",
                       value = "red") %>%
    vl_condition_shape(test = "datum.fastest",
                       value = "M0,.5L.6,.8L.5,.1L1,-.3L.3,-.4L0,-1L-.3,-.4L-1,-.3L-.5,.1L-.6,.8L0,.5Z") %>%
    vl_encode_href("links:N") %>%
    vl_mark_point(filled = TRUE) %>%
    vl_axis_x(title = NA) %>%
    vl_axis_y(labelExpr = "round(datum.value / 60)", title = "Solve Time (minutes)") %>%
    vl_encode_tooltip(list(vl$Tooltip(field = "Date", type = "temporal"), vl$Tooltip(field = "Solve Time", type = "nominal")))  %>%
    vl_filter("datum.valid")
  
  layered <- vl_layer(l1, l2, l3) %>%
    vl_add_data(chart_data) 
  
  if (plot_all) {
    # Use with to get number of columns
    view_width = 250
    ncol <- max(1, floor( width / view_width))
    layered <- layered %>% 
      vl_facet_wrap(field = "Day", type = "nominal", columns = ncol, 
                    sort = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), title = NA,
                    header = list(labelFontStyle = "bold", labelFontSize = 16, labelPadding = 2)) %>%
      vl_resolve_axis_y(how = "independent") %>% 
      vl_resolve_scale_y(how = "independent") %>% 
      vl_resolve_axis_x(how = "independent") %>%
      vl_config_view(width = view_width)
  } else {
    layered <- layered %>% vl_config_view(width = width, height = min(width, 350))
  }
  vegawidget(layered)
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
  
