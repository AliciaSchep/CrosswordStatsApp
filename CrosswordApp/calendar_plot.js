// !preview r2d3 data = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSHYo_DBWW53tMB-eezEaq1jXWy4Sr8QDsOR9ZtGQQrXQhPN6cpgEHbWcDB20D_p6O-HD3Pefscub9L/pub?gid=0&single=true&output=csv") %>% mutate(opens = Date - lag(Date,1, default = lubridate::ymd('1000-10-10')) != lubridate::days(1), grp = cumsum(opens),col = grp %% 4 + 1,Day =  lubridate::wday(Date, label = TRUE),tooltip = glue::glue("Date: {Date} ({Day}) <br /> Solve Time: {Duration}<br>"),links = glue::glue("https://www.nytimes.com/crosswords/game/daily/{strftime(Date,format = '%Y/%m/%d')}")) , d3_version = 4, container = "div", options = list(min = 1, max = 4, colors = c("#1B9E77","#D95F02","#7570B3","#E7298A"), endDate = lubridate::today())

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
    diff = startDate.getDate() - day ; 
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

var tooltipDiv = div.append("div")	
    .attr("class", "calendar-tooltip")				
    .style("opacity", 0)
    .style("position", "fixed")
    .style("background", "black")
    .style("color", "white")
    .style("border", "0px")
    .style("pointer-events", "none")
    .style("width","140px")					
    .style("height","28px")					
    .style("padding","2px")				
    .style("font","12px sans-serif");


function get_day(d){
  return (d.getDay() + 6) % 7;
}

var timeWeek = d3.utcMonday;

var rect = svg.append("g")
    .attr("fill", "none")
    .attr("stroke", "#ccc")
    .attr("stroke-width", "1")
  .selectAll("rect")
  .data(function(d) { return d3.timeDays(startDate, endDate); })
  .enter().append("rect")
    .attr("width", cellSize)
    .attr("height", cellSize)
    .attr("x", function(d) { return (timeWeek.count(startDate, d)) * cellSize; })
    //.attr("y", function(d) { return d.getDay() * cellSize; })
    .attr("y", function(d) { return get_day(d) * cellSize; })
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
    .attr("x", function(d) {return ((d3.timeWeek.count(startDate, d) + 1)) * cellSize; })
    .text(month_name);

var week = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat","Sun"];

svg.selectAll("text.day")
  .data(function(d) { return d3.range(7); })
  .enter().append("text")
    .attr("class", "dayofweek")
    .attr("font-family", "sans-serif")
    .attr("font-size", 2 + 6 * height / 60)
    .style('fill', 'black')
    .attr('text-anchor','end')
    .attr("x", function(d) {return (-0.5 * cellSize); })
    .attr("y", function(d) {return (1 + d) * cellSize; })
    .text(function(d) {return week[d];});

r2d3.onRender(function(csv, div, width, height, options) {
  var data = d3.nest()
      .key(function(d) { return d['Date']; })
      .rollup(function(d) { return [d[0]['col'], d[0]['tooltip'], d[0]['links']]; })
    .object(csv);
    

  rect.filter(function(d) { return d in data; })
      .attr("fill", function(d) { return color(data[d][0]); })
    .on("click", function(d) {window.open(data[d][2]);})
    .on("mouseover", function(d) {		
            var curDate = new Date(d);
            tooltipDiv.transition()		
                .duration(200)		
                .style("opacity", 0.9);		
            var el = d3.select(this).node();
            var bb = el.getBoundingClientRect();
            var xp = bb.x;
            var yp = bb.y;
            tooltipDiv.html(data[d][1])
                .style("left", xp - 140 + "px")
                .style("top", yp + cellSize * 0.5 + "px");
                
            })					
        .on("mouseout", function(d) {		
            tooltipDiv.transition()		
                .duration(500)		
                .style("opacity", 0);	
        });
});
