// !preview r2d3 data = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSHYo_DBWW53tMB-eezEaq1jXWy4Sr8QDsOR9ZtGQQrXQhPN6cpgEHbWcDB20D_p6O-HD3Pefscub9L/pub?gid=0&single=true&output=csv") %>% mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), grp = cumsum(opens),col = grp %% 4,Day =  lubridate::wday(Date, label = TRUE),tooltip = glue::glue("Date: {Date} ({Day}) <br/> Solve Time: {Duration}<br>"),links = glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}")) , d3_version = 4, container = "div", options = list(min = 1, max = 4, colors = c("#1B9E77","#D95F02","#7570B3","#E7298A"), endDate = lubridate::today())

// Based on https://rstudio.github.io/r2d3/articles/gallery/calendar/
// which was based on https://bl.ocks.org/mbostock/4063318

var margin_top = 25;
var cellSize;

if (((height / 8) - (margin_top/7)) > (width / 60)){
  cellSize = width / 60;
  height = cellSize * 8 + margin_top * 7/8;
} else {
  cellSize = (height / 8) - margin_top/7;
  width = cellSize * 60;
}


var endDate = new Date(options.endDate);
var startDate = new Date(options.endDate);
startDate.setFullYear( startDate.getFullYear() - 1 );

var day = startDate.getDay(),
    diff = startDate.getDate() - day - 1; 
startDate.setDate(diff);


var color = d3.scaleQuantize()
    .domain([options.min, options.max])
    .range(options.colors);

var svg = div
  .style("line-height", "0")
  .style("background", "white")
  .append("svg")
  .attr("width", width)
  .attr("height", height)
  .append("g")
    .attr("transform", "translate(" + cellSize * 3.5 + "," + (height - cellSize * 7.5 ) + ")");


var rect = svg.append("g")
    .attr("fill", "none")
    .attr("stroke", "#ccc")
    .attr("stroke-width", "1")
  .selectAll("rect")
  .data(function(d) { return d3.timeDays(startDate, endDate); })
  .enter().append("rect")
    .attr("width", cellSize)
    .attr("height", cellSize)
    .attr("x", function(d) { return d3.timeWeek.count(startDate, d) * cellSize; })
    .attr("y", function(d) { return d.getDay() * cellSize; })
    .datum(d3.timeFormat("%Y-%m-%d"));


var month_name = d3.timeFormat("%b");

svg.selectAll("text.month")
  .data(function(d) { return d3.timeMonths(startDate, endDate); })
  .enter().append("text")
    .attr("class", "month")
    .attr("font-family", "sans-serif")
    .attr("font-size", 2 + 6 * height / 60)
    .style('fill', 'black')
    .attr("y", function(d) {return (-0.5 * cellSize); })
    .attr("x", function(d) {return (d3.timeWeek.count(startDate, d)) * cellSize; })
    .text(month_name);

r2d3.onRender(function(csv, div, width, height, options) {
  var data = d3.nest()
      .key(function(d) { return d['Date']; })
      .rollup(function(d) { return [d[0]['col'], d[0]['tooltip'], d[0]['links']]; })
    .object(csv);
    

  rect.filter(function(d) { return d in data; })
      .attr("fill", function(d) { return color(data[d][0]); })
    .on("click", function(d) {window.open(data[d][2]);})
    .append("title")
      .html(function(d) {return data[d][1]; });

});
