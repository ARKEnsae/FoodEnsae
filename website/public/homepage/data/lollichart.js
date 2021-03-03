//var width = d3.select('cont').style('width');

const df = data;


// set the dimensions and margins of the graph
var margin = {top: 10, right: 30, bottom: 100, left: 100},
    width2 = 460 - margin.left - margin.right,
    height2 = 320 - margin.top - margin.bottom;

// Margins etc

//var svg = d3.select("#my_dataviz")
var svg = cont
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height2 + margin.top + margin.bottom)
//  .attr("viewBox", [0, 0, 100,100])
   //.attr( 'preserveAspectRatio',"xMinYMin meet")
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
//const borne = d3.max(df.map(function(d) { return d.borne_max; })); works
//const borne = df.map(function(d) { return (d.borne_max)[0]; }); do not work

const largeur_graphe = d3.min([width-margin.left - margin.right,width2]);

  
// Add X axis //enter().append("cont")
var x = d3.scaleLinear()
  .domain([0, options.borne_max])
 .range([ 0, largeur_graphe]);
 
  
svg.append("g")
  .attr("transform", "translate(0," + height2 + ")")
  .call(d3.axisBottom(x))
  .selectAll("text")
    .attr("transform", "translate(-10,0)rotate(-45)")
    .style("text-anchor", "end");
  

// Y axis
var y = d3.scaleBand()
  .range([ 0, height2 ])
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
    //.text(function(d) { return d.titre; })
    .text(options.titre)
    .attr("text-anchor", "end")
    .attr("x", largeur_graphe)
    .attr("y", height2 + margin.top + 45)
    .attr("font-family" , "sans-serif")
    .attr("font-size" , "12px")
    
// X axis notes
svg.selectAll("xtitle")
  .data(df)
  .enter()
  .append("text")
    .text(options.note1)
    .attr("x", 0)
    .attr("y", height2 + margin.top + 65)
    .attr("font-family" , "sans-serif")
    .attr("font-style" , "italic")
    .attr("font-size" , "11px")
    
svg.selectAll("xtitle")
  .data(df)
  .enter()
  .append("text")
    .text(options.note2)
    .attr("x", 0)
    .attr("y", height2 + margin.top + 80)
    .attr("font-family" , "sans-serif")
    .attr("font-style" , "italic")
    .attr("font-size" , "11px")
 



    
  

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
 //     .on("mouseover", function() { mouseover2(); mouseover(); })
 //    .on("mousemove2", function() { mousemove2(); mousemove(); })
 // .on("mouseleave", function() { mouseleave2(); mouseleave(); })
        .on("mouseover", mouseover)
    .on("mousemove", mousemove)
     .on("mouseleave", mouseleave)
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
       .on("mouseover", mouseover)
    .on("mousemove", mousemove)
     .on("mouseleave", mouseleave)
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

//https://datatricks.co.uk/animated-d3-js-bar-chart-in-r
//https://www.d3-graph-gallery.com/graph/histogram_tooltip.html
// tooltips

//Create a tooltip
var Tooltip = d3.select('#htmlwidget_container')
    .append('div')
    .attr("class", "tooltip")
    .style('position', 'absolute')
    .style('color', 'white')
    .style('background-color', 'black')
    .style('border-radius', '5px')
    .style('padding', '5px')
    .style('opacity', 0)
    //.style("font-family", "Tahoma, Geneva, sans-serif")
    .style("font-size", "10pt");
 
//Mouseover effects for tooltip
var mouseover = function(event,d) {
    Tooltip
        .html('<b>' + d.Value + '</b> produits (<b>' + d.pourc + '</b> %)')
        .style('opacity', 1)
        .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)')
        .style("left", (d3.pointer(event)[0]+120) + "px")
        .style("top", (d3.pointer(event)[1]+0) + "px");

};

// vieille version de d3
//var mousemove = function(d) {
//    Tooltip
//        .html('Hello')
//        .style("left", (d3.mouse(this)[0]+0) + "px")
//        .style("top", (d3.mouse(this)[1]+0) + "px")
//    };
    
var mousemove = function(event,d) {

  
};

var mouseleave = function(d) {
    Tooltip
        .style("opacity", 0);
};

//Mouseover effects for tooltip
var mouseover2 = function(d) {
  
      Tooltip
        .style('opacity', 1)
        .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');

    d3.select(this)
  .style('transition', '0.4s all cubic-bezier(0.5,0.8,0,1.7)')
  .attr("r", function (d) { return 15; });
};

var mousemove2 = function(event) {
  
      Tooltip
        .html('Hello')
        .style("left", (d3.pointer(event)[0]+120) + "px")
        .style("top", (d3.pointer(event)[1]+0) + "px");

    d3.select(this)
  .style('transition', '0.4s all cubic-bezier(0.5,0.8,0,1.7)')
  .attr("r", function (d) { return 18; });
};

var mouseleave2 = function(d) {
  
   Tooltip
        .style("opacity", 0);
        
  d3.select(this)
 .style('transition', '0.4s all cubic-bezier(0.5,0.8,0,1.7)')
 .attr("r", function (d) { return 15; });
};




//svg.selectAll('circle')
//    .on("mouseover", mouseover)
//    .on("mousemove", mousemove)
//    .on("mouseleave", mouseleave);
