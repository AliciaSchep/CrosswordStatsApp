// !preview r2d3 data = read.csv("https://raw.githubusercontent.com/rstudio/r2d3/master/vignettes/gallery/calendar/dji-latest.csv"), d3_version = 4, container = "div", options = list(end = "2010-9-10", fill = 'Open', min = 5000, max = 15000, colors = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837"), dates = "Date", endDate = '2010-08-31',tooltip = 'Open')

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
      .key(function(d) { return d[options.dates]; })
      .rollup(function(d) { return d[0][options.fill]; })
    .object(csv);
  
  var tool = d3.nest()
      .key(function(d) { return d[options.dates]; })
      .rollup(function(d) { return d[0][options.tooltip]; })
    .object(csv);

  rect.filter(function(d) { return d in data; })
      .attr("fill", function(d) { return color(data[d]); })
    .append("title")
      .text(function(d) { return d + ": " + tool[d]; });
});
