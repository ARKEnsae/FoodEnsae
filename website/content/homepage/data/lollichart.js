
const df = data;


// set the dimensions and margins of the graph
var margin = {top: 10, right: 30, bottom: 60, left: 100},
    width = 460 - margin.left - margin.right,
    height = 300 - margin.top - margin.bottom;

// Margins etc

var svg = div
//var svg = d3.select("#my_dataviz")
  .append("svg")
   .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// Parse the Data
//d3.csv("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum_header.csv", function(data) {

// sort data
//data.sort(function(b, a) {
//  return a.Value - b.Value;
//});

//https://www.d3-graph-gallery.com/graph/basic_datamanipulation.html
const borne = d3.max(df.map(function(d) { return d.borne_max; }));
//const borne = df.map(function(d) { return (d.borne_max)[0]; });

// Add X axis
var x = d3.scaleLinear()
  .domain([0, borne])
  .range([ 0, width]);
svg.append("g")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x))
  .selectAll("text")
    .attr("transform", "translate(-10,0)rotate(-45)")
    .style("text-anchor", "end");

// Y axis
var y = d3.scaleBand()
  .range([ 0, height ])
  .domain(df.map(function(d) { return d.Country; }))
  //.domain(["A","B","C","D","E"])
  .padding(1);


  
  //.attr('strock-width',0)
svg.append("g")
  //.call(d3.axisLeft(y))
  


// X axis title
svg.selectAll("xtitle")
  .data(df)
  .enter()
  .append("text")
    .text(function(d) { return d.titre; })
    .attr("text-anchor", "end")
    .attr("x", width)
    .attr("y", height + margin.top + 45)
    .attr("font-family" , "sans-serif")
    .attr("font-size" , "12px")
    
    
 



    
  

function transition() { 

   var timeLine = svg.selectAll("myline")
   .data(df)
    .enter()
  .append("line")
  .attr("stroke-width", 8)
    .attr("x1", x(0))
    .attr("x2", x(0))
    .attr("y1", function(d) { return y(d.Country); })
    .attr("y2", function(d) { return y(d.Country); })
    .attr("stroke", function(d) { return d.color; })

  var timeCircle = svg.selectAll("mycircle")
    .data(df)
    .enter()
    .append("circle")
      .attr("cx", x(0) )
      .attr("cy", function(d) { return y(d.Country); })
      .attr("r", "15")
      .style("fill", function(d) { return d.color; })
      
    // InnerCircles -> start at X=0
   var timeInnerCircle = svg.selectAll("myinnercircle")
    .data(df)
    .enter()
    .append("circle")
    .attr("cx", x(0) )
    .attr("cy", function(d) { return y(d.Country); })
    .attr("r", "10")
    .style("fill", "white")

      var timeLabel = svg.selectAll("mylabel")
      .data(df)
      .enter()
      .append("text")
      .text(function(d){return d.Country;})
      .attr("x", x(0) )
      .attr("y", function(d) { return y(d.Country)+5; })
    //.attr("font-family" , "Saira")
    .attr("font-size" , "14px")
    .attr("font-weight" , 1000)
    .attr("fill" , function(d){return d.color})
    .attr("text-anchor", "middle")
    
 
  
    repeat();
    
    function repeat() {
      
// Change the X coordinates of line and circle
      timeLine
      .transition()
      .duration(1000)
      .attr("x1", function(d) { return x(d.Value); })
      .transition()
      .duration(5000)
      .transition()
      .duration(0)
      .attr("x1", function(d) { return x(0); })
       .on("end", repeat);  // when the transition finishes start again
      
      
      // Change the X coordinates of line and circle
      timeCircle
      .transition()
      .duration(1000)
      .attr("cx", function(d) { return x(d.Value); })
      .transition()
      .duration(5000)
      .transition()
      .duration(0)
      .attr("cx", function(d) { return x(0); })
      .on("end", repeat);  // when the transition finishes start again

      timeInnerCircle
      .transition()
      .duration(1000)
      .attr("cx", function(d) { return x(d.Value); })
      .transition()
      .duration(5000)
      .transition()
      .duration(0)
      .attr("cx", function(d) { return x(0); })
      .on("end", repeat);  // when the transition finishes start again


      timeLabel
      .transition()
    .duration(1000)
    .attr("x", function(d) { return x(d.Value)-1; })
    .transition()
      .duration(5000)
      .transition()
      .duration(0)
      .attr("x", function(d) { return x(0); })



    };

};

transition();

