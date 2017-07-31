(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
  typeof define === 'function' && define.amd ? define(['exports'], factory) :
  (factory((global.lc = global.lc || {})));
}(this, function (exports) { 'use strict';

  //Basic object that can be chart or layer
  function base() {
  	
    var obj = {};
    obj.propList = [];
  	
    obj.add_property = function( propname, defaultval ) {
  		
      obj.propList.push(propname);
  		var getter = "get_" + propname;
      var overrideList = {};

      obj[ propname ] = function( vf, propname, overrideFunc ) {

        if( vf === undefined )
          return obj[ getter ]();      

        if( vf == "_override_"){
          if(typeof overrideFunc === "function")
            overrideList[propname] = overrideFunc
          else
            overrideList[propname] = function() {return overrideFunc;}
        } else {
          if( typeof(vf) === "function" )
            obj[ getter ] = vf
          else
            obj[ getter ] = function() { return vf };

          for(var i in overrideList)
            obj["get_" + i] = overrideList[i];
        }
        //setter always returns chart, never layer
        if(obj.layers)
          return obj
        else
          if(obj.chart)
            return obj.chart
          else
            return obj;
      }

  		if(typeof defaultval === "function")
  			obj[ getter ] = defaultval
  		else
  			obj[ getter ] = function() { return defaultval };
      return obj;
    }
  	
  	return obj;
  }

  function cache( f ) {
    var the_cache = {}
    return function() {
      if( arguments[0] === "clear" ) {
        the_cache = {}
        return undefined;
      }
      if( !( arguments in Object.keys(the_cache) ) &&
  			!(arguments.length == 0 && Object.keys(the_cache).length != 0))
        the_cache[arguments] = f.apply( undefined, arguments );
      return the_cache[arguments];
    }
  }

  function separateBy(data, properties){
    if(typeof data !== "object")
      throw "Error in function 'separateBy': first argument is not an object";
    
    //check if data is an object or an array
    var type;
    typeof data.length === "undefined" ? type = "obj" : type = "arr";

    if(typeof properties === "number" || typeof properties === "string")
      properties = [properties];
    //turn "properities" into an array and throw an Error if this isn't possible
    if(typeof properties.length === "undefined")
      throw "Error in function 'separateBy': " + properties.toString() +
            " is not a property name"; 
    
    //end of a recursive function. There are no more properties to
    //separate by
    if(properties.length == 0)
      return data;

    var newData = {}, uniqueList = [], keys, value;
    //if data is an array, keys = ["0", "1", "2", ...]
    var keys = Object.keys(data);

    //go through all elements to find all possible values of the selected property
    for(var i = 0; i < keys.length; i++){
      if(typeof data[keys[i]][properties[0]] !== "undefined" &&
        uniqueList.indexOf(data[keys[i]][properties[0]]) == -1
      )
        uniqueList.push(data[keys[i]][properties[0]]);
    }

    //if none of the objects have this property, continue with the next step
    //of the recursion
    if(uniqueList.length == 0){
      properties.shift();
      return separateBy(data, properties)
    }
    //otherwise initialize properties of the new object
    for(var i = 0; i < uniqueList.length; i++)
      type == "obj" ? newData[uniqueList[i]] = {} : newData[uniqueList[i]] = [];

    //go through all the elements again and place them in a suitable category
    for(var i = 0; i < keys.length; i++){
      value = data[keys[i]][properties[0]];
      if(typeof value !== "undefined"){
        delete data[keys[i]][properties[0]];
        if(type == "obj") newData[value][keys[i]] = {};
        type == "obj" ? Object.assign(newData[value][keys[i]], data[keys[i]]) :
                        newData[value].push(data[keys[i]]);
      }
    }
    //if type is array but all values of the property are unique change arrays in objects
    //May be this should be optional
    if(type == "arr"){
      var change = true, i = 0;
      while(change && i < uniqueList.length){
        change = (newData[uniqueList[i]].length == 1);
        i++;
      }
      if(change){
        var a;
        for(var i = 0; i < uniqueList.length; i++){
          a = {};
          Object.assign(a, newData[uniqueList[i]][0]);
          newData[uniqueList[i]] = {};
          Object.assign(newData[uniqueList[i]], a);
        }
      }
    }
    //Now go through all the properties of the new object and call this function
    //recursively
    properties.shift();
    
    for(var i = 0; i < uniqueList.length; i++)
      newData[uniqueList[i]] = separateBy(newData[uniqueList[i]], properties.slice());
    return newData;
  }

  function getEuclideanDistance(a, b) {
  	if(a.length != b.length)
  		throw "Error in getEuclideanDistance: length of the" +
  			"input vectors is not the same";
  	var sum = 0;
  	for(var i = 0; i < a.length; i++)
  		sum += (a[i] - b[i]) * (a[i] - b[i]);
  	
  	return Math.sqrt(sum);
  }

  function add_click_listener(chart){

    var wait_dblClick = null, down, wait_click = null,
      tolerance = 5, click_coord, downThis,
      parcer = call_pacer(100), panStarted = false,
      transitionDuration;
   
    //add a transparent rectangle to detect clicks
    //and make changes to update function
    chart.svg.select(".plotArea").append("rect")
      .attr("class", "clickPanel")
      .attr("fill", "transparent")
      .lower();
    var inherited_updateSize = chart.updateSize;
    
    chart.updateSize = function(){
      inherited_updateSize();
       chart.svg.selectAll(".clickPanel")
        .attr("width", chart.plotWidth())
        .attr("height", chart.plotHeight());
      return chart;
    }

    var on_mousedown = function(){
      //remove all the hints if there are any
      chart.container.selectAll(".hint")
        .remove();

      down = d3.mouse(document.body);
      downThis = d3.mouse(this)
      wait_click = window.setTimeout(function() {wait_click = null;}, 1000);
      var p = d3.mouse(this);  //Mouse position on the heatmap
      if(!chart.pan.mode)
        chart.svg.select(".plotArea").append("rect")
          .attr("class", "selection")
          .attr("x", p[0])
          .attr("y", p[1])
          .attr("width", 1)
          .attr("height", 1);
      if(chart.pan.mode){
        panStarted = true;
        chart.pan.down = downThis;
        transitionDuration = chart.transitionDuration();
        chart.transitionDuration(0)
        chart.defineTransition();
      }
      chart.container.select(".inform")
        .classed("blocked", true);

      document.addEventListener("mouseup", function() {
        chart.container.select(".inform")
          .classed("blocked", false);
        chart.svg.select("rect.selection").remove();
        chart.svg.select(".shadow").remove();
        if(panStarted) {
          panStarted = false;
          chart.transitionDuration(transitionDuration);
          chart.defineTransition();
          chart.pan.down = undefined;
        }

        document.onmouseup = null;
      }, false);
    }
    var wait = false;
    var on_mousemove = function(){
      var s = chart.svg.select(".selection"),
        p = d3.mouse(this);
          
      if(panStarted){
        if(!wait){
          wait = true;
          setTimeout(function() {wait = false}, 100);
          chart.pan.move(p);
        }
        return;
      }

      if(!s.empty()) {
        s.attr("x", d3.min([p[0], downThis[0]]))
          .attr("y", d3.min([downThis[1], p[1]]))
          .attr("width", Math.abs(downThis[0] - p[0]))
          .attr("height", Math.abs(downThis[1] - p[1]));
        
      var shadow = chart.svg.select(".shadow");

      if(shadow.empty() && 
            Math.abs((downThis[0] - p[0]) * (downThis[1] - p[1])) > 10)
        shadow = chart.svg.select(".plotArea").append("path")
          .attr("class", "shadow")
          .attr("fill", "#444")
          .attr("opacity", 0.6);

      shadow
        .attr("d", "M 0 0" + 
                  " h " + chart.plotWidth() + 
                  " v " + chart.plotHeight() + 
                  " h -" + chart.plotWidth() + 
                  " v -" + chart.plotHeight() +
                  " M " + s.attr("x") + " " + s.attr("y") + 
                  " v " + s.attr("height") +
                  " h " + s.attr("width") +
                  " v -" + s.attr("height") +                  
                  " h -" + s.attr("width")) 
       // .lower();
       return;
      }

      if(chart.canvas && chart.canvas.classed("active")){
        parcer.do(function(){
          var point = chart.findPoints(p, p)[0].substr(1).split("_-sep-_");
          chart.container.select(".inform")
            .style("left", (p[0] + 10 + chart.margin().left) + "px")
            .style("top", (p[1] + 10 + chart.margin().top) + "px")
            .select(".value")
              .html(function() {return chart.get_informText(point[0], point[1])});

        })
      }
    }

    var on_mouseup = function(){
      var pos = d3.mouse(this);

      var mark = d3.event.shiftKey || chart.selectMode;
      // remove selection frame
      chart.container.select(".inform")
        .classed("blocked", false);

      if(!chart.svg.select("rect.selection").empty())
        var x = chart.svg.selectAll("rect.selection").attr("x") * 1,
          y = chart.svg.selectAll("rect.selection").attr("y") * 1,
          w = chart.svg.selectAll("rect.selection").attr("width") * 1,
          h = chart.svg.selectAll("rect.selection").attr("height") * 1,
          lu = [x, y], 
          rb = [x + w, y + h];
      
      var points = d3.select(this);
      
      chart.svg.selectAll("rect.selection").remove();
      chart.svg.select(".shadow").remove();

      if(wait_click && getEuclideanDistance(down, d3.mouse(document.body)) < tolerance){
        window.clearTimeout(wait_click);
        wait_click = null;
        if(wait_dblClick && 
          getEuclideanDistance(click_coord, d3.mouse(document.body)) < tolerance
        ){
          //console.log("doubleclick");
          window.clearTimeout(wait_dblClick);
          wait_dblClick = null;
          points.on("dblclick").apply(points, [mark]);
        } else {
          wait_dblClick = window.setTimeout((function(e) {
            return function() {
              points.on("click").call(points, pos, mark);
              wait_dblClick = null;
              if(panStarted) {
                panStarted = false;
                chart.pan.move(pos);
                chart.container.select(".inform").classed("blocked", false);
                chart.transitionDuration(transitionDuration);
                chart.defineTransition();
                chart.pan.down = undefined;
                return;
              }          
            };
          })(d3.event), 300);
        }
        click_coord = d3.mouse(document.body);
        return;
      }

      if(panStarted) {
        panStarted = false;
        chart.pan.move(pos);
        chart.container.select(".inform").classed("blocked", false);
        chart.transitionDuration(transitionDuration);
        chart.defineTransition();
        chart.pan.down = undefined;
        return;
      }

      // remove temporary selection marker class
      if(mark)
        chart.mark(chart.findPoints(lu, rb))
      else 
        chart.zoom(lu, rb);      
    }
    var on_dblclick = function(mark){
      mark ? chart.mark("__clear__") : chart.resetDomain();
    }
    var on_panelClick = function(p, mark){
      if(typeof p === "undefined")
        return;
      var clickedPoints = chart.findPoints(p, p);
      if(!mark){
        var click, clickFun, data;
        for(var i = 0; i < clickedPoints.length; i++) {
          click = chart.svg.select("#" + lc.escapeRegExp(clickedPoints[i]).replace(/ /g, "_"));
          if(!click.empty()) {
            clickFun = click.on("click");
            clickFun.call(click.node(), click.datum());
          } else { //required for canvas and therefore supposed to be used only in case of a heatmap
            data = clickedPoints[i].substr(1).split("_-sep-_");
            chart.get_on_click(data[0], data[1]);
          }
        }
      } else {
        chart.mark(clickedPoints);
      }
    }

    chart.svg.selectAll(".plotArea")
      .on("mousedown", on_mousedown, true)
      .on("mousemove", on_mousemove, true)
      .on("mouseup", on_mouseup, true)
      .on("dblclick", on_dblclick, true)
      .on("click", on_panelClick, true);
    
    return chart;
  }

  function pearsonCorr( v1, v2 ) {
     var sum1 = 0;
     var sum2 = 0;
     for( var i = 0; i < v1.length; i++ ) {
        sum1 += v1[i];
        sum2 += v2[i];
     }
     var mean1 = sum1 / v1.length;
     var mean2 = sum2 / v2.length;
     var cov = 0
     var var1 = 0
     var var2 = 0
     for( var i = 0; i < v1.length; i++ ) {
        cov += ( v1[i] - mean1 ) * ( v2[i] - mean2 );
        var1 += ( v1[i] - mean1 ) * ( v1[i] - mean1 );
        var2 += ( v2[i] - mean2 ) * ( v2[i] - mean2 );
     } 
     return cov / Math.sqrt( var1 * var2 );
  } 

  function wrapText(text, width, height, minSize, maxSize, fontRatio){
    var splitBy = function(text, symbol){
      var spl = text.split(symbol);
      if(spl[spl.length - 1] == "")
        spl.pop();
      if(spl.length == 1) return;
      var mult = 0, bestSep, leftSide = 0,
        rightSide = text.length;
      for(var i = 0; i < spl.length - 1; i++){
        leftSide += (spl[i].length + 1);
        rightSide -= (spl[i].length + 1);
        if(mult < leftSide * rightSide){
          mult = leftSide *  rightSide;
          bestSep = i;
        }
      }
      return [spl.slice(0, bestSep + 1).join(symbol) + symbol, 
              spl.slice(bestSep + 1, spl.length - bestSep).join(symbol)];
    }

    var splitByVowel = function(text){
      var vowelInd = Array.apply(null, Array(text.length)).map(Number.prototype.valueOf,0),
        vowels = ["a", "A", "o", "O", "e", "E", "u", "U", "i", "I"];
      
      for(var i = 0; i < text.length; i++)
        if(vowels.indexOf(text[i]) != -1)
          vowelInd[i] = 1;
      for(var i = 0; i < vowelInd.length - 1; i++)
        vowelInd[i] = (vowelInd[i] - vowelInd[i + 1]) * vowelInd[i];
      vowelInd[vowelInd.length - 1] = 0;
      vowelInd[vowelInd.length - 2] = 0;
      if(vowelInd.indexOf(1) == -1)
        return [text.substring(0, Math.ceil(text.length / 2)) + "-", 
                text.substring(Math.ceil(text.length / 2))];
      var mult = 0, bestSep;
      for(var i = 0; i < text.length; i++)
        if(vowelInd[i] == 1)
          if(mult < (i + 2) * (text.length - i - 1)){
            mult = (i + 2) * (text.length - i - 1);
            bestSep = i;
          }

        return [text.substring(0, bestSep + 1) + "-", 
                text.substring(bestSep + 1)];
    }

    if(typeof minSize === "undefined")
      minSize = 8;
    if(typeof maxSize === "undefined")
      maxSize = 13;
    if(typeof fontRatio === "undefined")
      fontRatio = 0.6;
    var fontSize = d3.min([height, maxSize]),
      spans = [text], maxLength = text.length,
      allowedLength, longestSpan = 0,
      mult, br;

    while(maxLength * fontSize * fontRatio > width && fontSize >= minSize){
      if(maxLength == 1)
        fontSize = width / fontRatio * 0.95
      else {
        if(height / (spans.length + 1) < width / (maxLength * fontRatio) * 0.95)
          fontSize = width / (maxLength * fontRatio) * 0.95
        else {
          var charachters = [" ", ".", ",", "/", "\\", "-", "_", "+", "*", "&", "(", ")", "?", "!"],
            spl, i = 0;
          while(typeof spl === "undefined" && i < charachters.length){
            spl = splitBy(spans[longestSpan], charachters[i]);
            i++;
          }
          if(typeof spl === "undefined")
            spl = splitByVowel(spans[longestSpan]);
          spans.splice(longestSpan, 1, spl[0], spl[1]);

          allowedLength = Math.floor(width / (fontSize * fontRatio));

          for(var i = 0; i < spans.length - 1; i++)
            if(spans[i].length + spans[i + 1].length <= allowedLength &&
                spans[i].length + spans[i + 1].length < maxLength){
              spans.splice(i, 2, spans[i] + spans[i + 1]);
              fontSize = d3.min([height / (spans.length - 1), maxSize]);
              allowedLength = Math.floor(width / (fontSize * fontRatio));
            }

          fontSize = d3.min([height / spans.length, maxSize]);
          maxLength = spans[0].length;
          longestSpan = 0;
          for(var i = 1; i < spans.length; i++)
            if(spans[i].length > maxLength){
              maxLength = spans[i].length;
              longestSpan = i;
            }
        }
      }     
    }

   // fontSize = d3.min([height / spans.length, width / (maxLength * fontRatio)]);

    return {spans: spans, fontSize: fontSize};
  }

  function fillTextBlock(g, width, height, text, minSize, maxSize, fontRatio){
    var fit = wrapText(text, width, height, minSize, maxSize, fontRatio),
      spans = g.selectAll("text").data(d3.range(fit.spans.length));
      spans.exit().remove();
      spans.enter().append("text")
        .merge(spans)
          .attr("class", "plainText")
          .attr("text-anchor", "left")
          .attr("font-size", fit.fontSize)
          .attr("y", function(d) {return (d + 1) * fit.fontSize;})
          .text(function(d) {return fit.spans[d]});
  }

  function get_symbolSize(type, r) {
    var sizeCoef = {
      "Circle": 28.2,
      "Cross": 35,
      "Diamond": 46,
      "Square": 36,
      "Star": 47,
      "Triangle": 44,
      "Wye": 37
    };

    return Math.pow(r * 28.2 / sizeCoef[type], 2) * 3.14;
  }

  function escapeRegExp(str) {
    return str.replace(/[\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
  }

  function call_pacer( interval ) {

     /*
     This "call pacer" serves to avoid jerky appearance when a redraw
     function is triggered to often because events fire to rapidly.
     
     Say, you have a handler, e.g., for a 'drag' event, which is supposed
     to call an 'update' function. Usually you would write
        some_object.on( "drag", update );
     If 'update' is complex, you may want to make sure that it is not called
     more often than, say, once in 30 ms. If the drag event fires more often,
     intermittant calls to 'update' should be skipped. For this, write.
       var pacer = call_pacer( 30 );
       some_object.on( "drag", function() { pacer.do( update ) } )
     Now, pacer.do will call 'update', but only if the last call was more than
     30 ms ago. It also ensures that after a lapse of more than 30 ms without
     event, the last lapsed call is then called, to draw the final state.
     */

     var obj = {
        interval: interval,
        prev_time: -Infinity,
        timer: null }

     obj.do = function() {
        var callback = arguments[0],
          args = [];
        for(var i = 1; i < arguments.length; i++)
          args.push(arguments[i]);
        if( obj.timer )
           obj.timer.stop();
        if( d3.now() - obj.prev_time >= interval ) {         
           callback.call(this, args);
           obj.prev_time = d3.now();
        } else {
           obj.timer = d3.timeout( callback, 1.5 * interval )
        }
     }

     return obj;
  }

  function layerBase(id) {
  	
  	var layer = base()
      .add_property("npoints")
      .add_property("dataIds")
  		.add_property("pointMouseOver", function() {})
  		.add_property("pointMouseOut", function() {})
  		.add_property("on_click", function() {})
  		.add_property("layerDomainX")
  		.add_property("layerDomainY")
  		.add_property("contScaleX", true)
  		.add_property("contScaleY", true)
      .add_property("colour", function(id) {
        return layer.colourScale(layer.get_colourValue(id));
      })
      .add_property("addColourScaleToLegend", true)
      .add_property("palette")
      .add_property("colourRange")
      .add_property("colourValue", undefined)
  		.add_property("dresser", function(){});

  	layer.id = id;

    layer.dataIds( "_override_", "npoints", function(){
      return layer.get_dataIds().length;
    });
    layer.npoints( "_override_", "dataIds", function() {
      return d3.range( layer.get_npoints() );
    });
    layer.colour( "_override_", "addColourScaleToLegend", false );

    layer.colourRange(function() {
      var ids = layer.get_dataIds();
      if(layer.get_colourValue(ids[0]) !== undefined){
        var range = [];
        for(var i = 0 ; i < ids.length; i++)
          //colour range can contain only unique values
          if(range.indexOf(layer.get_colourValue(ids[i])) == -1)
            range.push(layer.get_colourValue(ids[i]));

        return range;
      }
    });

    layer.colourScale = function(){
      return "black";
    }

    layer.resetColourScale = function() {
      var range = layer.get_colourRange();
      if(range === undefined)
        return;
      //first of all, let's check if the colour scale supposed to be
      //categorical or continuous
      var allNum = true;
      for(var i = 0; i < range.length; i++)
        allNum = allNum && typeof range[i] === "number";
      if(allNum)
        range.sort(function(a, b) {return a - b});
      if(allNum){
        //the scale is continuous
        //Now look at the palette
        if(typeof layer.get_palette() == "undefined")
          if(d3.interpolateSpectral)
            layer.palette(d3.interpolateSpectral)
          else
            layer.palette(["red", "yellow", "green", "blue"]);
        //if palette is an array of colors, make a linear colour scale using all
        //values of the palette as intermideate points
        if(layer.get_palette().splice){
          var palette = layer.get_palette();
          if(palette.length != range.length)
            range = [d3.min(range), d3.max(range)];
          if(palette.length == 1)
            palette.push(palette[0]);
          if(palette.length > range.length){
            var newRange = [];
            for(var i = 0; i < palette.length; i++)
              newRange.push(range[0] + i*(range[1] - range[0]) / (palette.length - 1));
            range = newRange; 
          }
          //now palette and range have exactly the same number of elements
          layer.colourValueScale = d3.scaleLinear()
            .domain(range)
            .range(palette);
          layer.colourScale = function(val) {
            return layer.colourValueScale(val);
          }
        } else {
          //palette is a d3.scale or d3.interpolator
          range = [d3.min(range), d3.max(range)];
          //if palette has a domain - use it, otherwise set a domain to
          //[0, 1] (used in d3. interpolators)
          var pDomain = [0, 1];
          if(layer.get_palette().domain)
            pDomain = layer.get_palette().domain();

          layer.colourValueScale = d3.scaleLinear()
            .domain(range)
            .range(pDomain);
          layer.colourScale = function(val) {
            return layer.get_palette(layer.colourValueScale(val));
          }
        }
      } else {
        //the colour scale is categorical
        if(typeof layer.get_palette() === "undefined")
          layer.palette(["#000"].concat(d3.schemeCategory10));
        if(layer.get_palette().length){
          var palette = layer.get_palette();
          //just make sure that palette has enough elements to provide
          //colour to each object type
          var paletteLength = palette.length;
          for(var i = 0; i < range.length - paletteLength; i++)
            palette.push(palette[i % paletteLength]);

          layer.colourValueScale = d3.scaleOrdinal()
            .domain(range)
            .range(palette);      
          layer.colourScale = function(val) {
            return layer.colourValueScale(val);
          }
        } else {
          var pDomain = [0, 1];
          if(layer.get_palette().domain)
            pDomain = layer.get_palette().domain();

          layer.colourValueScale = d3.scalePoint()
            .domain(range)
            .range(pDomain);        
          layer.colourScale = function(val) {
            return layer.get_palette(layer.colourValueScale(val));
          }
        } 
      }

      layer.colourScale.domain = layer.get_colourRange;
      if(layer.chart.showLegend())
        layer.addLegend(layer.colourScale, "colour", layer.id);
    }

    layer.legendBloccks = [];

    layer.addLegend = function(scale, type, id){
      layer.chart.legend.add(scale, type, id, layer);
      layer.legendBloccks.push(id);

      return layer; 
    }

  	layer.update = function() {
      
      layer.updatePoints();
      layer.updatePointStyle();
      layer.updatePointLocation();

      return layer;
    };

  	layer.put_static_content = function() {
      layer.g = layer.chart.svg.select(".plotArea").append("g")
        .attr("class", "chart_g")
        .attr("id", layer.id)
        //.attr("clip-path", "url(#" + layer.chart.svg.select("clipPath").attr("id") + ")");  # <-- TO FIX!
      //layer.chart.svg.select(".clickPanel").raise();
  	};
  	
  	layer.afterUpdate = function(){};
    
    layer.updateSize = function(){
    }
    layer.updatePoints = function() {
    };
    layer.updatePointStyle = function() {
      layer.resetColourScale();
    	layer.get_dresser(layer.g.selectAll(".data_point"));
    	return layer;
    };
    layer.updatePointLocation = function() {};
    layer.findPoints = function() {return [];}; //return empty selection	
  	layer.get_position = function(id) {return undefined;}

  	return layer;
  }

  function legend(chart) {
  	var legend = base()
  		.add_property("width", 200)
  		.add_property("height", function() {return chart.height();})
  		.add_property("sampleHeight", 20)
  		.add_property("ncol", undefined)
  		.add_property("location");

  	legend.blocks = {};
  	legend.chart = chart;

  	legend.add = function(scale, type, id, layer){
  		//scale can be an array or d3 scale. If scale is an array,
  		//we need to turn it into a scale
  		var block = {};
  		if(typeof scale === "function")
  			block.scale = scale
  		else
  			block.scale = function() {return scale;};
  		if(typeof layer !== "undefined")
  			block.layer = layer;
  		if(["colour", "size", "style", "symbol", "dash"].indexOf(type) == -1)
  			throw "Error in 'legend.add': " + type + " is not a suitable type of legend block. " +
  				"Please, use one of these: 'colour', 'size', 'symbol', 'dash'";
  		block.type = type;

  		legend.blocks[id] = block;
  		legend.updateGrid();

  		return legend.chart;
  	}

  	legend.updateScale = function(scale, id){
  		if(typeof legend.blocks[id] === "undefined")
  			throw "Error in 'legend.updateScale': A block with ID " + id +
  				" is not defined";
  		legend.blocks[id].scale = scale;
  		legend.updateBlock(id);

  		return legend.chart;
  	}

  	legend.convertScale = function(id) {
  		var scale, newScale;
  		if(typeof legend.blocks[id].scale === "function")
  			scale = legend.blocks[id].scale();
  		if(typeof scale === "undefined" || 
  				(typeof scale !== "function" && typeof scale.splice === "undefined"))
  			scale = legend.blocks[id].scale;
  		
  		if(typeof scale !== "function"){
  			var scCont = false,
  				rCont = false;
  			if(scale.length == 1)
  				throw "Error in 'legend.add': range of the scale is not defined.";
  			if(scale[0].length == 2 && typeof scale[0][0] === "number" && 
  																typeof scale[0][1] === "number")
  				scCont = true;
  			if(legend.blocks[id].type == "colour" && scale[0].length != scale[1].length)
  				rCont = true;
  			if(scale[1].length == 2 && typeof scale[0][0] === "number" && 
  																typeof scale[0][1] === "number")
  				rCont = true;
  			if(scCont && rCont){
  				newScale = d3.scaleLinear()
  					.domain(scale[0])
  					.range(scale[1]);
  				scale.steps ? newScale.steps = scale.steps : newScale.steps = 9;
  			}
  			if(scCont && !rCont){
  				newScale = d3.scaleQuantize()
  					.domain(scale[0])
  					.range(scale[1]);
  				newScale.steps = scale[1].length;
  			}
  			if(!scCont && rCont){
  				newScale = d3.scalePoint()
  					.domain(scale[0])
  					.range(scale[1]);
  				newScale.steps = scale[0].length;
  			}
  			if(!scCont && !rCont){
  				if(scale[0].length > scale[1].length)
  					scale[0].splice(scale[1].length);
  				if(scale[1].length > scale[0].length)
  					scale[1].splice(scale[0].length);
  				newScale = d3.scaleOrdinal()
  					.domain(scale[0])
  					.range(scale[1]);
  				newScale.steps = scale[0].length;				
  			}
  			legend.blocks[id].domain = scale[0];
  			if(typeof newScale.domain === "undefined")
  				newScale.domain = legend.blocks[id].domain;
  		} else {
  			//scale is a function it is either a d3 scale or it has a domain property
  			if(typeof scale !== "function")
  				throw "Error in 'legend.add': the type of scale argument is not suported. " +
  					"Scale should be an array or a function."
  			var domain;
  			typeof scale.domain === "function" ? domain = scale.domain() : domain = scale.domain;
  			if(typeof domain === "undefined")
  				throw "Error in 'legend.add': the domain of the scale is not defined.";
  			legend.blocks[id].domain = domain;
  			newScale = scale;
  			if(scale.steps)
  				newScale.steps = scale.steps
  			else {
  				domain.length == 2 && typeof domain[0] === "number" ? newScale.steps = 9 : newScale.steps = domain.length;
  			} 
  		}
  		return newScale;
  	}

  	legend.remove = function(id) {
  		if(typeof legend.blocks[id] === "undefined")
  			throw "Error in 'legend.remove': block with ID " + id +
  			" doesn't exist";
  		if(typeof legend.blocks[id].layer !== "undefined")
  			legend.blocks[id].layer.legendBlocks.splice(
  				legend.blocks[id].layer.legendBlocks.indexOf(id), 1
  			);
  		delete legend.blocks[id];
  		legend.g.select("#" + id).remove();
  		legend.updateGrid();

  		return legend.chart;
  	}

  	legend.rename = function(oldId, newId) {
  		legend.blocks[newId] = legendBlocks.blocks[oldId];
  		delete legend.blocks[oldId];
  		if(typeof legend.blocks[newId].layer !== "undefined")
  			legend.blocks[newId].layer.legendBlocks.splice(
  				legend.blocks[newId].layer.legendBlocks.indexOf(oldId), 1, newId
  			);
  		legend.g.select("#" + oldId)
  			.attr("id", newId);
  		legend.update();

  		return legend.chart;
  	}
  	legend.updateGrid = function() {
  		//define optimal layout for all the blocks
  		//and create a table
  		var bestWidth, bestHeight,
  			n = Object.keys(legend.blocks).length;

  		if(typeof legend.ncol() === "undefined"){
  			var minSum = 1 + n, j;
  			bestHeight = 1; 
  			for(var i = 2; i <= Math.ceil(Math.sqrt(n)); i++){
  				j =  Math.ceil(n / i);
  				if(i + j <= minSum){
  					minSum = i + j;
  					bestHeight = i;
  				}
  			}
  			bestWidth = Math.ceil(n / bestHeight);
  		} else {
  			bestWidth = legend.ncol();
  			bestHeight = Math.ceil(n / bestWidth);
  		}
  		legend.location().select(".legendTable").remove();
  		legend.location().append("table")
  			.attr("class", "legendTable")
  			.selectAll("tr").data(d3.range(bestHeight))
  				.enter().append("tr");
  		legend.location().selectAll("tr").selectAll("td")
  			.data(function(d) {
  				return d3.range(bestWidth).map(function(e) {
  					return [d, e];
  				})
  			})	
  			.enter().append("td")
  				.attr("id", function(d) {
  					try{
  						return Object.keys(legend.blocks)[d[0] * bestWidth + d[1]]
  										.replace(/ /g, "_");
  					} catch(exc) {return undefined;}
  				});
  		for(var i in legend.blocks)
  			legend.updateBlock(i);
  	}


  	legend.updateBlock = function(id){
  		if(typeof legend.blocks[id] === "undefined")
  			throw "Error in 'legend.updateBlock': block with ID " + id +
  				" is not defined";

  		var scale = legend.convertScale(id),
  			tableCell = legend.location().select("#" + id.replace(/ /g, "_")),
  			cellWidth = legend.width() / legend.location().select("tr").selectAll("td").size(),
  			steps = scale.steps,
  			cellHeight = legend.sampleHeight() * steps;

  		var blockSvg = tableCell.selectAll("svg");
  		if(blockSvg.empty())
  			blockSvg = tableCell.append("svg");
  		blockSvg.attr("width", cellWidth)
  			.attr("height", cellHeight);
  	
  		var title = blockSvg.select(".title");
  		if(title.empty())
  			title = blockSvg.append("g")
  				.attr("class", "title");
  		var titleWidth = d3.min([20, cellWidth * 0.2]);
  		fillTextBlock(title, cellHeight, titleWidth, id);
  		title.attr("transform", "rotate(-90)translate(-" + cellHeight + ", 0)");

  		var sampleValues;
  		if(legend.blocks[id].domain.length == steps)
  			sampleValues = legend.blocks[id].domain;
  		else
  			sampleValues = d3.range(steps).map(function(e) {
  				return legend.blocks[id].domain[0] + e * 
  								(legend.blocks[id].domain[1] - legend.blocks[id].domain[0]) / 
  								(steps - 1)
  			})
  		var sampleData = [];
  		for(var i = 0; i < sampleValues.length; i++)
  			sampleData.push([sampleValues[i]]);
  		
  		var samples = blockSvg.selectAll(".sample").data(sampleData);
  		samples.exit().remove();
  		samples.enter().append("g")
  			.attr("class", "sample")
  			.merge(samples)
  				.attr("transform", function(d, i) {
  					return "translate(" + (titleWidth + 1) + ", " + 
  									(i * legend.sampleHeight()) + ")";
  				});

  		if(legend.blocks[id].type == "colour"){
  			var rect = blockSvg.selectAll(".sample").selectAll("rect").data(function(d){
  				return d;
  			});
  			rect.enter().append("rect")
  				.merge(rect)
  					.attr("width", titleWidth)
  					.attr("height", legend.sampleHeight())
  					.attr("fill", function(d) {return scale(d)});
  		}
  		if(legend.blocks[id].type == "symbol"){
  			var size = d3.min([legend.sampleHeight() / 2, 
  													titleWidth / 2]);
  			var symbols = blockSvg.selectAll(".sample").selectAll("path").data(function(d){
  				return d;
  			});
  			symbols.enter().append("path")
  				.merge(symbols)
  					.attr("d", function(d) {
  						return d3.symbol()
  							.type(d3["symbol" + scale(d)])
  							.size(get_symbolSize(scale(d), size))();
  					})
  					.attr("transform", "translate(" + size + ", " + size + ")");
  		}
  		if(legend.blocks[id].type == "dash"){
  			var lines = blockSvg.selectAll(".sample").selectAll("line").data(function(d){
  				return d;
  			});
  			lines.enter().append("line")
  				.style("stroke", "black")
  			 	.merge(lines)
  			 		.attr("x1", 0)
  			 		.attr("x2", titleWidth)
  			 		.attr("y1", legend.sampleHeight() / 2)
  			 		.attr("y2", legend.sampleHeight() / 2)
  			 		.attr("stroke-dasharray", function(d) {return scale(d)});
  		}

  		var sampleText = blockSvg.selectAll(".sample").selectAll("g").data(function(d){
  			return (typeof d[0] === "number") ? [d[0].toString()] : d;
  		});
  		sampleText.enter().append("g")
  			.merge(sampleText)
  				.attr("transform", "translate(" + (titleWidth + 5) + ", 0)");
  		blockSvg.selectAll(".sample").selectAll("g").each(function(d) {
  			fillTextBlock(d3.select(this), cellWidth - 2 * titleWidth - 5, 
  											legend.sampleHeight(), d
  										);
  		});		
  	}
  	legend.update = function() {
  		legend.updateGrid();
  	}

  	return legend;
  }

  function panel(chart) {
  	var panel = base()
  		.add_property("x", function(){
  			return chart.width() - panel.buttonSize - 5;
  		})
  		.add_property("y", function(){
  			return chart.margin().top * 0.05;
  		})
  		.add_property("orientation", "horizontal")
  		.add_property("height", function(){
  			if(panel.orientation() == "vertical")
  				return Math.floor(chart.height() - panel.y() / panel.buttonSize) * 
  					panel.buttonSize
  			else
  				return undefined;
  		})
  		.add_property("width", function() {
  			if(panel.orientation() == "horizontal")
  				return Math.floor(chart.width() - panel.x() / panel.buttonSize) * 
  					panel.buttonSize
  			else
  				return undefined;
  		});

  	panel.chart = chart;
  	panel.buttons = [];
  	panel.buttonSize = 30;


  	panel.put_static_content = function(){
  		panel.g = panel.chart.svg.append("g")
  			.attr("class", "panel_g");

  		panel.initDefs();
  		
  		panel.g.append("use")
  			.attr("xlink:href", "#toggleOff")
  			.attr("id", "toggle")
  			.attr("opacity", 0.7)
  			.attr("title", "Click to show instrument panel")
  			.on("mouseover", function() {
  				d3.select(this)
  					.attr("opacity", 1);
  			})
  			.on("mouseout", function() {
  				d3.select(this)
  					.attr("opacity", 0.7);
  			})
  			.on("click", panel.show)
  			.append("title")
  				.text("Click to show instrument panel");

  		panel.g.append("g")
  			.attr("id", "buttonPanel")
  			.attr("class", "hidden");		
  	}
  	
  	panel.updateSize = function() {
  		var layout = panel.placeButtons();
  		if(panel.orientation() == "vertical"){
  			panel.g.attr("transform", "translate(" + panel.x() + 
  																", " + panel.y() + ")");
  			panel.g.select("#toggle")
  				.attr("transform", "translate(0, 0)");
  			panel.g.select("#buttonPanel")
  				.attr("transform", "translate(0, " + panel.buttonSize + ")");
  		} else {
  			panel.g
  				.attr("transform", "translate(" + 
  														(panel.x() - panel.buttonSize * panel.buttons.length) + 
  																", " + panel.y() + ")");
  			panel.g.select("#toggle")
  				.attr("transform", "translate(" + (panel.buttonSize * panel.buttons.length) + ", 0)");
  			panel.g.select("#buttonPanel")
  				.attr("transform", "translate(0, 0)");			
  		}

  	}

  	panel.placeButtons = function() {
  		var rowLength;
  		if(panel.orientation()  == "horizontal"){
  				rowLength = panel.optimizeSize(panel.buttons.length, panel.width(), panel.height());
  				panel.g.selectAll(".button")
  					.attr("transform", function(d, i){
  						return "translate(" + (i % rowLength * panel.buttonSize) + ", " +
  							(Math.floor(i / rowLength) * panel.buttonSize) + ")";
  					})
  		} else {
  				rowLength = panel.optimizeSize(panel.buttons.length, panel.height(), panel.width());
  				panel.g.selectAll(".button")
  					.attr("transform", function(d, i){
  						return "translate(" + (Math.floor(i / rowLength) * panel.buttonSize) + ", " 
  							+ ((i % rowLength + 1) * panel.buttonSize) + ")";
  					})
  		}
  	}
  	panel.optimizeSize = function(n, width, height){
  		var rows, size;
  		if(height){
  			size = d3.min([width, height]);
  			rows = 1;
  			while(Math.floor(width / size) * rows < n){
  				rows++;
  				size = d3.min([height / rows, size]);
  			}
  			panel.buttonSize = size;
  		} else {
  			size = panel.buttonSize;
  			rows = Math.ceil(width / size);
  		}
  		if(panel.orientation() == "horizontal"){
  			panel.width(size * Math.floor(width / size));
  			//panel.height(size * rows)
  		} else {
  			panel.height(size * Math.floor(width / size));
  			//panel.width(size * rows);
  		}

  		return Math.floor(width / size);
  	}


  	panel.add_button = function(name, icon, fun, hint){
  		//if hint is defined, modify the provided f
  		var hintFired = false;
  		var wrapped = function(chart, button){
  			if(!hintFired){
  				panel.showHint(hint); //hints are showed just once if defined
  				hintFired = true;
  			}
  			fun(chart, button);
  		}

  		panel.buttons.push({
  			name: name,
  			icon: icon,
  			fun: wrapped
  		});

  		var buttons = panel.g.select("#buttonPanel")
  			.selectAll(".button").data(panel.buttons, function(d) {return d.name;});
  		buttons.enter().append("use")
  			.attr("opacity", 0.6)
  			.attr("class", "button")
  			.attr("id", function(d) {return "b_" + d.icon.substr(1)})
  			.attr("xlink:href", function(d) {return d.icon})
  			.on("click", function(d) {d.fun(panel.chart, d3.select(this))})
  			.on("mouseover", function() {
  				d3.select(this)
  					.attr("opacity", 1);
  			})
  			.on("mouseout", function() {
  				d3.select(this)
  					.attr("opacity", 0.6);
  			})
  			.append("title")
  				.text(function(d) {return d.name});		
  	}

  	panel.showHint = function(hint) {
  		if(hint){
  			chart.container.append("div")
  				.attr("class", "hint")
  				.style("left", (panel.chart.width() - 105) + "px")
  				.style("top", (panel.y() + panel.g.node().getBBox().height) + "px")
  				.text(hint);
  		}
  	}

  	panel.show = function(){
  		panel.g.select("#toggle")
  			.attr("opacity", 1)
  			.on("click", panel.hide)
  			.on("mouseout", function() {})
  			.select("title")
  				.text("Click to hide instrument panel");
  		if(panel.orientation() == "horizontal")
  			panel.g.select("#toggle")
  				.attr("xlink:href", "#toggleOnHor")
  		else
  			panel.g.select("#toggle")
  				.attr("xlink:href", "#toggleOnVer");

  		panel.g.select("#buttonPanel")
  			.classed("hidden", false);

  	}

  	panel.hide = function(){
  		panel.g.select("#toggle")
  			.attr("xlink:href", "#toggleOff")
  			.attr("opacity", 0.7)
  			.on("click", panel.show)
  			.on("mouseout", function() {
  				d3.select(this)
  					.attr("opacity", 0.7);
  			})
  			.select("title")
  				.text("Click to show instrument panel");
  		panel.g.select("#buttonPanel")
  			.classed("hidden", true);

  	}

  	panel.initDefs = function(){
  		var defs = panel.g.append("def"),
  			bs = panel.buttonSize - 10;
  		
  		var d = defs.append("g")
  			.attr("id", "toggleOff");
  		d.append("rect")
  			.attr("stroke-width", 2)
  			.attr("width", bs)
  			.attr("height", bs)
  			.attr("fill", "#aaa")
  			.attr("stroke", "#444");
  		d.append("path")
  			.attr("d", "M " + bs/2  + " " + Math.floor(bs/3) + 
  									" L " + Math.ceil(bs * 2 / 3) + " " + Math.ceil(bs * 2 / 3) + 
  									" H " + Math.floor(bs/3) + 
  									" L " + bs/2 + " " + Math.floor(bs/3))
  			.attr("fill", "#444")
  			.attr("stroke-width", 0);			

  		d = defs.append("g")
  			.attr("id", "toggleOnHor");
  		d.append("rect")
  			.attr("stroke-width", 2)
  			.attr("width", bs)
  			.attr("height", bs)
  			.attr("fill", "#aaa")
  			.attr("stroke", "#444");
  		d.append("path")
  			.attr("d", "M " + Math.floor(bs/3) + " " + bs/2 + 
  									" L " + Math.ceil(bs * 2 / 3) + " " + Math.floor(bs/3) + 
  									" V " + Math.floor(bs * 2 / 3) + 
  									" L " + Math.floor(bs/3) + " " + bs/2)
  			.attr("fill", "#444")
  			.attr("stroke-width", 0);			

  		d = defs.append("g")
  			.attr("id", "toggleOnVer");
  		d.append("rect")
  			.attr("stroke-width", 2)
  			.attr("width", bs)
  			.attr("height", bs)
  			.attr("fill", "#aaa")
  			.attr("stroke", "#444");
  		d.append("path")
  			.attr("d", "M " + bs/2 + " " + Math.ceil(bs * 2 / 3) + 
  									" L " + Math.floor(bs/3) + " " + Math.floor(bs/3) + 
  									" H " + Math.floor(bs * 2 / 3) + 
  									" L " + bs/2 + " " + Math.ceil(bs * 2 / 3))
  			.attr("fill", "#444")
  			.attr("stroke-width", 0);

  		d = defs.append("g")
  			.attr("id", "save");
  		d.append("rect")
  			.attr("stroke-width", 0)
  			.attr("width", bs)
  			.attr("height", bs)
  			.attr("fill", "#444")
  			.attr("rx", bs/10)
  			.attr("ry", bs/10);
  		d.append("path")
  			.attr("d", "M " + Math.floor(4 * bs / 5) + " 0" + 
  									" H " + bs + 
  									" V " + Math.ceil(bs/5) + 
  									" L " + Math.floor(4 * bs / 5) + " 0")
  			.attr("fill", "#fff")
  			.attr("stroke-width", 0);
  		d.append("rect")
  			.attr("x", Math.floor(bs/3))
  			.attr("height", Math.floor(bs/3))
  			.attr("width", Math.ceil(bs * 2 / 5))
  			.attr("fill", "#fff")
  			.attr("stroke-width", 0);
  		d.append("rect")
  			.attr("x", Math.floor(44 * bs / 75))
  			.attr("width", Math.ceil(2 * bs / 25))
  			.attr("height", Math.ceil(bs/4))
  			.attr("fill", "#444")
  			.attr("stroke-width", 0);
  		d.append("rect")
  			.attr("x", Math.floor(bs/4))
  			.attr("width", Math.ceil(5 * bs / 12))
  			.attr("y", bs/2)
  			.attr("height", bs/2)
  			.attr("rx", bs/10)
  			.attr("ry", bs/10)
  			.attr("fill", "#fff")
  			.attr("stroke-width", 0);

  		d = defs.append("g")
  			.attr("id", "svg");
  		d.append("text")
  			.attr("font-size", bs * 1.5)
  			.attr("textLength", bs)
  			.attr("lengthAdjust", "spacingAndGlyphs")
  			.attr("fill", "#444")
  			.attr("y", bs)
  			.attr("font-weight", 900)
  			.attr("font-family", "Verdana")
  			.text("SVG");
  		
  		d = defs.append("g")
  			.attr("id", "selection");
  		d.append("rect")
  			.attr("fill-opacity", 0)
  			.attr("width", Math.floor(bs * 2 / 3))
  			.attr("height", Math.floor(bs * 2 / 3))
  			.attr("x", Math.ceil(bs/3))
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("stroke-dasharray", 2);
  		d.append("path")
  			.attr("fill", "#444")
  			.attr("stroke-width", 0)
  			.attr("stroke", "#444")
  			.attr("d", "M " + Math.ceil(bs/3) + " " + Math.floor(bs * 2 / 3) + 
  								" L 0 " + Math.ceil(3 * bs / 4) + 
  								" l " + bs/12 + " " + bs/12 + 
  								" L 0 " + (9 * bs / 10) +
  								" L " + bs/10 + " " + bs +
  								" L " + (11 * bs / 60) + " " + (11 * bs / 12) + 
  								" L " + Math.floor(bs/4) + " " + bs + 
  								" L " + Math.ceil(bs/3) + " " + Math.floor(bs * 2 / 3));
  		d.append("circle")
  			.attr("cx", Math.floor(bs/5))
  			.attr("cy", Math.floor(bs/5))
  			.attr("r", 2)
  			.attr("fill", "#444");
  		d.append("circle")
  			.attr("cx", Math.floor(4 * bs / 5))
  			.attr("cy", Math.floor(4 * bs / 5))
  			.attr("r", 2)
  			.attr("fill", "#444");
  		d.append("circle")
  			.attr("cx", Math.floor(2 * bs / 3))
  			.attr("cy", Math.floor(bs / 3))
  			.attr("r", 3)
  			.attr("fill", "#111");

  		d = defs.append("g")
  			.attr("id", "zoomIn");
  		d.append("path")
  			.attr("fill", "#444")
  			.attr("d", "M " + (2 * bs / 5) + " 0" + 
  								" h " + bs/5 + 
  								" v " + (2 * bs / 5) + 
  								" h " + (2 * bs / 5) +
  								" v " + bs/5 +
  								" h -" + (2 * bs / 5) +
  								" v " + (2 * bs / 5) +
  								" h -" + bs/5 + 
  								" v -" + (2 * bs / 5) +
  								" h -" + (2 * bs / 5) +
  								" v -" + bs/5 + 
  								" h " + (2 * bs / 5) +
  								" v -"+ (2 * bs / 5));

  		d = defs.append("g")
  			.attr("id", "zoomOut");
  		d.append("rect")
  			.attr("y", 3 * bs / 8)
  			.attr("height", bs/4)
  			.attr("width", bs)
  			.attr("fill", "#444");

  		d = defs.append("g")
  			.attr("id", "home");
  		d.append("rect")
  			.attr("x", bs/5)
  			.attr("y", 2 * bs / 5)
  			.attr("width", 3 * bs / 5)
  			.attr("height", 3 * bs / 5)
  			.attr("fill", "#444");
  		d.append("rect")
  			.attr("x", bs * 2 / 5)
  			.attr("y", bs * 3 / 5)
  			.attr("width", bs/5)
  			.attr("height", bs/5)
  			.attr("fill", "#fff");
  		d.append("path")
  			.attr("fill", "#444")
  			.attr("d", "M 0 " + (2 * bs / 5) + 
  								" L " + bs/2 + " 0" +
  								" L " + bs + " " + (2 * bs / 5));
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M 0 " + (2 * bs / 5) +
  								" L " + bs + " " + (2 * bs / 5));

  		d = defs.append("g")
  			.attr("id", "pan");
  		d.append("path")
  			.attr("fill", "#444")
  			.attr("d", "M 0 " + bs/2 + 
  								" l " + bs/5 + " -" + bs/5 + 
  								" v " + bs/10 + 
  								" h " + bs/5 + 
  								" v -" + bs/5 +
  								" h -" + bs/10 +
  								" L " + bs/2 + " 0" + 
  								" l " + bs/5 + " " + bs/5 +
  								" h -" + bs/10 +
  								" v " + bs/5 + 
  								" h " + bs/5 +
  								" v -" + bs/10 +
  								" L " + bs + " " + bs/2 +
  								" l -" + bs/5 + " " + bs/5 +
  								" v -" + bs/10 + 
  								" h -" + bs/5 +
  								" v " + bs/5 +
  								" h " + bs/10 +
  								" L " + bs/2 + " " + bs +
  								" l -" + bs/5 + " -" + bs/5 +
  								" h " + bs/10 + 
  								" v -" + bs/5 +
  								" h -" + bs/5 +
  								" v " + bs/10 + 
  								" L 0 " + bs/2);
  		d = defs.append("g")
  			.attr("id", "fitSelected");
  		d.append("rect")
  			.attr("x", bs/5)
  			.attr("y", bs/5)
  			.attr("width", 3 * bs / 5)
  			.attr("height", 3 * bs / 5)
  			.attr("fill", "#fff")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("stroke-dasharray", 2);
  		d.append("circle")
  			.attr("cx", bs/5 - 3)
  			.attr("cy", bs/2)
  			.attr("r", 2)
  			.attr("fill", "#444");
  		d.append("circle")
  			.attr("cx", 3 * bs / 5)
  			.attr("cy", bs/5 -3)
  			.attr("r", 2)
  			.attr("fill", "#444");
  		d.append("circle")
  			.attr("cx", 4 * bs / 5 + 3)
  			.attr("cy", 2 * bs / 5)
  			.attr("r", 2)
  			.attr("fill", "#444");
  		d.append("circle")
  			.attr("cx", 2 * bs / 5)
  			.attr("cy", 4 * bs / 5 + 3)
  			.attr("r", 2)
  			.attr("fill", "#444");
  		d.append("circle")
  			.attr("cx", 4 * bs / 5 - 3)
  			.attr("cy", bs / 5 + 3)
  			.attr("r", 3)
  			.attr("fill", "#111");
  		d.append("circle")
  			.attr("cx", bs / 5 + 3)
  			.attr("cy", 4 * bs / 5 - 3)
  			.attr("r", 3)
  			.attr("fill", "#111");

  		d = defs.append("g")
  			.attr("id", "clusterRows");
  		d.append("rect")
  			.attr("x", bs * 2 / 5)
  			.attr("width", bs * 3 / 5)
  			.attr("height", bs)
  			.attr("fill", "#444");
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("y", bs/5)
  			.attr("height", bs/2)
  			.attr("width", bs * 2 / 5);
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("y", bs/10)
  			.attr("x", bs/5)
  			.attr("height", bs/5)
  			.attr("width", bs/5);
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("x", bs/10)
  			.attr("y", bs/2)
  			.attr("height", bs * 3 / 10)
  			.attr("width", bs * 3 / 10);
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("x", bs/5)
  			.attr("y", bs * 7 / 10)
  			.attr("height", bs/5)
  			.attr("width", bs/5);
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 2 / 5 + " " + bs/5 + 
  								" L " + bs + " " + bs/5);
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 2 / 5 + " " + bs * 2 / 5 + 
  								" L " + bs + " " + bs * 2 / 5);			
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 2 / 5 + " " + bs * 3 / 5 + 
  								" L " + bs + " " + bs * 3 / 5);
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 2 / 5 + " " + bs * 4 / 5 + 
  								" L " + bs + " " + bs * 4 / 5);

  		d = defs.append("g")
  			.attr("id", "clusterCols");
  		d.append("rect")
  			.attr("y", bs * 2 / 5)
  			.attr("height", bs * 3 / 5)
  			.attr("width", bs)
  			.attr("fill", "#444");
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("x", bs/5)
  			.attr("width", bs/2)
  			.attr("height", bs * 2 / 5);
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("x", bs/10)
  			.attr("y", bs/5)
  			.attr("width", bs/5)
  			.attr("height", bs/5);
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("y", bs/10)
  			.attr("x", bs/2)
  			.attr("width", bs * 3 / 10)
  			.attr("height", bs * 3 / 10);
  		d.append("rect")
  			.attr("stroke", "#444")
  			.attr("stroke-width", 1)
  			.attr("fill", "#fff")
  			.attr("y", bs/5)
  			.attr("x", bs * 7 / 10)
  			.attr("width", bs/5)
  			.attr("height", bs/5);
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs/5 + " " + bs * 2 / 5 + 
  								" L " + bs/5 + " " + bs);
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 2 / 5 + " " + bs * 2 / 5 + 
  								" L " + bs * 2 / 5 + " " + bs);			
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 3 / 5 + " " + bs * 2 / 5 + 
  								" L " + bs * 3 / 5 + " " + bs);
  		d.append("path")
  			.attr("stroke", "#fff")
  			.attr("stroke-width", 1)
  			.attr("d", "M " + bs * 4 / 5 + " " + bs * 2 / 5 + 
  								" L " + bs * 4 / 5 + " " + bs);

  		defs.selectAll("rect")
  			.attr("transform", "translate(5, 5)");
  		defs.selectAll("path")
  			.attr("transform", "translate(5, 5)");
  		defs.selectAll("text")
  			.attr("transform", "translate(5, 5)");
  		defs.selectAll("circle")
  			.attr("transform", "translate(5, 5)");
  		defs.selectAll("g").append("rect")
  			.attr("fill", "transparent")
  			.attr("width", panel.buttonSize)
  			.attr("height", panel.buttonSize)
  			.lower();
  	}

  	return panel;
  }

  //basic chart object
  function chartBase() {
  	var chart = base()
  		.add_property("width", 500)
  		.add_property("height", 500)
  		.add_property("plotWidth", 440)
  		.add_property("plotHeight", 435)
  		.add_property("margin", { top: 15, right: 10, bottom: 50, left: 50 })
  		.add_property("title", "")
  		.add_property("titleX", function() {return chart.width() / 2;})
  		.add_property("titleY", function() {return d3.min([17, chart.margin().top * 0.9]);})
  		.add_property("titleSize", function() {return d3.min([15, chart.margin().top * 0.8]);})
  		.add_property("transitionDuration", 1000) //may be set to zero
  		.add_property("markedUpdated", function() {})
  		.add_property("showPanel", true); 
  	
  	chart.selectMode = false;
  	chart.pan = {mode: false, down: undefined};
  	chart.transition = undefined;
    chart.width("_override_", "plotWidth", function(){
    			return chart.get_width() - 
    				(chart.get_margin().right + chart.get_margin().left);
    });
  /*  chart.plotWidth("_override_", "width", function(){
    			return chart.get_plotWidth() +
    				(chart.get_margin().right + chart.get_margin().left);
    }); */
    chart.margin("_override_", "plotWidth", function(){
    			return chart.get_width() - 
    				(chart.get_margin().right + chart.get_margin().left);
    });
    chart.height("_override_", "plotHeight", function(){
    			return chart.get_height() - 
    				(chart.get_margin().top + chart.get_margin().bottom);
    });
   /* chart.plotHeight("_override_", "height", function(){
    			return chart.get_plotHeight() +
    				(chart.get_margin().top + chart.get_margin().bottom);
    }); */
    chart.margin("_override_", "plotHeight", function(){
    			return chart.get_height() - 
    				(chart.get_margin().top + chart.get_margin().bottom);
    });

    chart.set_margin = function(margin){
    	if(typeof margin.top === "undefined")
    		margin.top = chart.margin().top;
    	if(typeof margin.bottom === "undefined")
    		margin.bottom = chart.margin().bottom;
    	if(typeof margin.left === "undefined")
    		margin.left = chart.margin().left;
    	if(typeof margin.right === "undefined")
    		margin.right = chart.margin().right;
    	
    	chart.margin(margin);
    	return chart;
    }

    chart.put_static_content = function( element ) {
  		chart.container = element.append("div")
  			.style("position", "relative");
  		chart.container.node().ondragstart = function() { return false; };
  		chart.svg = chart.container.append("svg");
  		chart.viewBox = chart.svg.append("defs")
  			.append("clipPath")
  				.attr("id", "viewBox" + Math.random().toString(36).substring(2, 6))
  				.append("rect");
  		chart.container.append("div")
  			.attr("class", "inform hidden")
  			.append("p")
  				.attr("class", "value");
  		chart.svg.append("text")
  			.attr("class", "title plainText")
  			.attr("text-anchor", "middle");
  		chart.svg.append("g")
  			.attr("class", "plotArea");
  		if(chart.showPanel()){
  			chart.panel = panel(chart);
  			chart.panel.put_static_content();

  			chart.panel.add_button("Save plot as png", "#save", function(chart){
  				function drawInlineSVG(svgElement, ctx, callback){
    				var svgURL = new XMLSerializer().serializeToString(svgElement);
    				var img  = new Image();
    				img.onload = function(){
      				ctx.drawImage(this, 0,0);
      				callback();
      			}
    			img.src = 'data:image/svg+xml; charset=utf8, '+encodeURIComponent(svgURL);
   				}

   				chart.svg.select(".panel_g")
   					.style("display", "none");

  				var canvas = document.createElement('canvas');
  				canvas.height = chart.svg.attr('height');
  				canvas.width = chart.svg.attr('width');

  				chart.svg.selectAll("text").attr("fill", "black");
  				drawInlineSVG(chart.svg.node(), canvas.getContext("2d"), 
  					function(){
  						var dataURL = canvas.toDataURL('image/png');
  						var data = atob(dataURL.substring('data:image/png;base64,'.length)),
  		        								asArray = new Uint8Array(data.length);

  						for (var i = 0, len = data.length; i < len; ++i)
  		    			asArray[i] = data.charCodeAt(i);

  						var blob = new Blob([asArray.buffer], {type: 'image/png'});
  						saveAs(blob, 'export_' + Date.now() + '.png');
  					});
   				chart.svg.select(".panel_g")
   					.style("display", undefined);
  			});

  			chart.panel.add_button("Save plot as svg", "#svg", function(chart){
   				chart.svg.select(".panel_g")
   					.style("display", "none");

  				var html = chart.svg
      	    .attr("xmlns", "http://www.w3.org/2000/svg")
        	  .node().parentNode.innerHTML;

  		    var blob = new Blob([html], {type: "image/svg+xml"});
  				saveAs(blob, 'export_' + Date.now() + '.svg');

   				chart.svg.select(".panel_g")
   					.style("display", undefined);
  			});

  			chart.panel.add_button("Select elements", "#selection", function(chart, button){
  				if(button.classed("clicked")){
  					button
  						.classed("clicked", false)
  						.attr("opacity", 0.6)
  						.on("mouseout", function() {
  							d3.select(this)
  								.attr("opacity", 0.6);
  						});
  					chart.selectMode = false;

  				} else {
  					button
  						.classed("clicked", true)
  						.attr("opacity", 1)
  						.on("mouseout", function() {});
  					chart.selectMode = true;
  					var panButton = chart.panel.g.select("#b_pan");
  					if(panButton.classed("clicked"))
  						panButton.on("click").call(panButton.node(), panButton.datum());					
  				}
  			}, "You can also select elements by pressing 'Shift'");
  			chart.panel.add_button("Togle pan mode", "#pan", function(chart, button){
  				if(button.classed("clicked")){
  					button
  						.classed("clicked", false)
  						.attr("opacity", 0.6)
  						.on("mouseout", function() {
  							d3.select(this)
  								.attr("opacity", 0.6);
  						})
  					chart.pan.mode = false;
  				} else {
  					button
  						.classed("clicked", true)
  						.attr("opacity", 1)
  						.on("mouseout", function() {});
  					chart.pan.mode = true;
  					var selectButton = chart.panel.g.select("#b_selection");
  					if(selectButton.classed("clicked"))
  						selectButton.on("click").call(selectButton.node(), selectButton.datum());
  				}
  			});
  			chart.panel.add_button("Reset scales", "#home", function(chart){
  				chart.resetDomain();
  			}, "You can also use double click to reset scales");
  			chart.panel.add_button("Fit selected", "#fitSelected", function(chart){
  				var marked = chart.get_marked();
  				if(marked.length == 0)
  					return;
  				var pos = {x: [], y: []};
  				marked.map(function(e) {
  					var pointPos = chart.get_position(e); 
  					pos.x.push(pointPos[0]);
  					pos.y.push(pointPos[1]);
  				});
  				var x_range = d3.extent(pos.x),
  					y_range = d3.extent(pos.y);
  				chart.zoom([x_range[0], y_range[0]], [x_range[1], y_range[1]]);
  			});
  		}

  	}

  	chart.defineTransition = function(){
  		if(chart.transitionDuration() > 0){
  			chart.transition = 
  				d3.transition().duration(chart.transitionDuration());
  			chart.transition
  				.on("end", chart.defineTransition);
  		} else {
  			chart.transition = undefined;
  		}
  	}

  	chart.mark = function(marked) {
  		if(marked == "__clear__"){
  			chart.svg.selectAll(".data_point.marked")
  				.classed("marked", false);
  			chart.svg.selectAll(".data_point")
  				.attr("opacity", 1);
  			chart.markedUpdated();
  			return;
  		}
  		//marked can be either an array of IDs or a selection
  		if(typeof marked.empty === "undefined") {
  			marked = marked.map(function(e) {return lc.escapeRegExp(e).replace(/ /g, "_")});
  			if(marked.length > 0){
  				var marked = chart.svg.selectAll(
  			 		"#" + marked.join(", #"));
  			} else{
  				var marked = chart.svg.select("_____");
  			}
  		}
  		
  		if(chart.svg.selectAll(".data_point.marked").empty())
  			chart.svg.selectAll(".data_point")
  				.attr("opacity", 0.5);
  		marked.classed("switch", true);
  		if(marked.size() < 2)
  			marked.filter(function() {return d3.select(this).classed("marked");})
  				.classed("switch", false)
  				.classed("marked", false)
  				.attr("opacity", 0.5);
  		marked.filter(function() {return d3.select(this).classed("switch");})
  			.classed("marked", true)
  			.classed("switch", false)
  			.attr("opacity", 1);
  		if(chart.svg.selectAll(".data_point.marked").empty())
  			chart.svg.selectAll(".data_point")
  				.attr("opacity", 1);

  		chart.markedUpdated();
  	}

  	chart.get_marked = function(){
  		var points = [];
  		chart.svg.selectAll(".marked").each(function() {
  			points.push(d3.select(this).datum());
  		});
  		return points;
  	}

  	chart.afterUpdate = function(){
  		if(chart.get_transitionDuration() != 0)
  			chart.defineTransition();
  	}

    chart.place = function( element ) {
      if( element === undefined )
        element = "body";
      if( typeof( element ) == "string" ) {
        var node = element;
        element = d3.select( node );
        if( element.size() == 0 )
          throw "Error in function 'place': DOM selection for string '" +
            node + "' did not find a node."
    	}

  		chart.put_static_content( element );
      chart.update();
      chart.afterUpdate();
      return chart;
    }
  	
  	//update parts
  	chart.updateSize = function(){
  		chart.viewBox
  			.attr("x", -5) //Let's leave some margin for a view box so that not to cut
  			.attr("y", -5) //points that are exactly on the edge
  			.attr("width", chart.plotWidth() + 10) 
  			.attr("height", chart.plotHeight() + 10);
  		if(typeof chart.transition !== "undefined"){
  			chart.svg.transition(chart.transition)
  				.attr("width", chart.width())
  				.attr("height", chart.height());
  			chart.svg.select(".title").transition(chart.transition)
  				.attr("font-size", chart.titleSize())
  				.attr("x", chart.titleX())
  				.attr("y", chart.titleY());
  			chart.svg.select(".plotArea").transition(chart.transition)
  				.attr("transform", "translate(" + chart.margin().left + 
  															", " + chart.margin().top + ")");
  		} else {
  			chart.svg
  				.attr("width", chart.width())
  				.attr("height",	chart.height());
  			chart.svg.select(".title")
  				.attr("font-size", chart.titleSize())
  				.attr("x", chart.titleX())
  				.attr("y", chart.titleY());
  			chart.svg.select(".plotArea")
  				.attr("transform", "translate(" + chart.margin().left + 
  															", " + chart.margin().top + ")");
  		}
  		if(chart.showPanel())
  			chart.panel.updateSize();
  		return chart;			
  	}
  	chart.updateTitle = function(){
  		chart.svg.select(".title")
  			.text(chart.title());		
  	}

  	chart.getPoints = function(data){
  		data = data.map(function(e) {return lc.escapeRegExp(e).replace(/ /g, "_")});
  		return chart.svg.selectAll("#p" + data.join(", #p"));
  	}

  	chart.update = function(){
  		chart.updateSize();
  		chart.updateTitle();
  		return chart;
  	}
    return chart;
  }

  function layerChartBase(){
  	var chart = chartBase()
  		.add_property("activeLayer", undefined)
  		.add_property("showLegend", true)
  		.add_property("layerIds", function() {return Object.keys(chart.layers);})
  		.add_property("layerType", function(id) {return chart.get_layer(id).type;});
  	
  	chart.legend = legend(chart);

  	//Basic layer functionality
  	chart.layers = {};
  	var findLayerProperty = function(propname){
  		return function() {
  			if(chart.get_activeLayer()[propname])
  				return chart.get_activeLayer()[propname].apply(chart, arguments)
  			else {
  				for(var i in chart.layers)
  					if(chart.layers[i][propname])
  						return chart.layers[i][propname].apply(chart, arguments);
  				return;
  			}
  		}
  	}
  	chart.syncProperties = function(layer){
  		for(var i = 0; i < layer.propList.length; i++)
  			if(typeof chart[layer.propList[i]] === "undefined")
  				chart[layer.propList[i]] = findLayerProperty(layer.propList[i]);
  	}

  	chart.get_nlayers = function() {
  		return Object.keys(chart.layers).length;
  	}
  	chart.get_layer = function(id) {
  		if(Object.keys(chart.layers).indexOf(id) == -1)
  			throw "Error in 'get_layer': layer with id " + id +
  				" is not defined";

  		return chart.layers[id];
  	}
  	chart.create_layer = function(id) {
  		if(typeof id === "undefined")
  			id = "layer" + chart.get_nlayers();

  		var layer = layerBase(id);
  		layer.chart = chart;
  		chart.layers[id] = layer;
  		chart.activeLayer(chart.get_layer(id));

  		return chart;
  	}
  	chart.add_layer = function(id) {
  		if(typeof id === "undefined")
  			id = "layer" + chart.get_nlayers();

  		var type;
  		try {
  			type = chart.get_layerType(id);
  		} catch (exc) {};
  		if(typeof type === "undefined"){
  			chart.create_layer(id);
  		} else {
  			if(type == "scatter")
  				lc.scatterChart(id, chart);
  			if(type == "xLine")
  				lc.xLine(id, chart);
  			if(type == "yLine")
  				lc.yLine(id, chart);
  			if(type == "paramCurve")
  				lc.parametricCurve(id, chart);
  			if(type == "bar")
  				lc.barChart(id, chart);
  		}
  		return chart;
  	}
  	chart.remove_layer = function(id) {
  		if( Object.keys(chart.layers).indexOf(id) == -1)
  			return -1;
  		//clean the legend
  		for(i in chart.layers[id].legendBlocks)
  			chart.legend.remove(i);
  		try {
  			chart.layers[id].g.remove();
  		} catch(exc) {};
  		delete chart.layers[id];

  		return 0;
  	}
  	chart.select_layers = function(ids) {
  		if(typeof ids === "undefined")
  			ids = chart.layerIds();

  		var layerSelection = {};
  		layerSelection.layers = {};
  		//extract or initialise all the requested layers
  		for(var i = 0; i < ids.length; i++)
  			if(chart.layerIds().indexOf(ids[i]) != -1) {
  				if(typeof chart.layers[ids[i]] === "undefined"){
  					chart.add_layer(ids[i]);
  					chart.get_layer(ids[i]).put_static_content();
  				}
  				layerSelection.layers[ids[i]] = chart.get_layer(ids[i]);
  			} else {
  				ids.splice(i, 1);
  				i--;
  			}
  		if(Object.keys(layerSelection.layers).length == 0){
  			for(i in chart)
  				layerSelection[i] = function() {return layerSelection};
  			return layerSelection;
  		}
  		//construct generalised property functions
  		//note, that only the properties shared between layers
  		//can  be generalized
  		var prop, flag, j;
  		for(var j = 0; j < ids.length; j++)
  			for(var i = 0; i < layerSelection.layers[ids[j]].propList.length; i++){
  				prop = layerSelection.layers[ids[j]].propList[i];
  				if(typeof layerSelection[prop] === "undefined")
  					layerSelection[prop] = (function(prop) {return function(val){
  						var vf;
  						if(typeof val !== "function")
  							vf = function() {return val;}
  						else
  							vf = val;
  						for(var i = 0; i < ids.length; i++)
  							if(typeof layerSelection.layers[ids[i]][prop] !== "undefined")
  								layerSelection.layers[ids[i]][prop]( (function(id) {return function(){ 
  									var args = []
  									for(var j = 0; j < arguments.length; j++)
  										args.push(arguments[j]);
  									args.unshift(id);
  									return vf.apply(undefined, args); 
  								} })(ids[i]));
  						return layerSelection;
  					} })(prop);
  			}
  		if(layerSelection.length == 0)
  			return chart;
  		return layerSelection;
  	}

  	chart.get_marked = function(){
  		var points = [];
  		chart.svg.selectAll(".marked").each(function() {
  			points.push([d3.select(this.parentNode).attr("id"), 
  										d3.select(this).datum()]);
  		});
  		return points;
  	}

  	chart.findPoints = function(lu, rb){
  		var selPoints = [];
  		chart.svg.selectAll(".chart_g").each(function(){
  			selPoints = selPoints.concat(
  				chart.get_layer(d3.select(this).attr("id")).findPoints(lu, rb)
  			);
  		});
  		return selPoints;
  	}

  	chart.get_position = function(id){
  		return chart.get_layer(id[0]).get_position(id[1]);
  	}

  	chart.placeLayer = function(id){
  		chart.get_layer(id).put_static_content();
  		chart.get_layer(id).updateSize();
  		chart.get_layer(id).update();
  	}

  	var inherited_put_static_content = chart.put_static_content;
  	chart.put_static_content = function(element){
  		inherited_put_static_content(element);
  		chart.container
  			.append("table")
  				.append("tr")
  					.append("td").node()
  						.appendChild(chart.svg.node());
  		//chart.svg.remove();
  		chart.svg = chart.container.select("svg");

  		//add a cell for the legend
  		chart.legend.location(chart.container.select("tr")
  													.append("td").attr("id", "legend"));

  		add_click_listener(chart);
  		for(var k in chart.layers)
  			chart.get_layer(k).put_static_content();		
  	}

  	var inherited_update = chart.update;
  	chart.update = function() {
  		var ids = chart.layerIds(), type;
  		for(var i = 0; i < ids.length; i++){
  			if(typeof chart.layers[ids[i]] === "undefined")
  				chart.add_layer(ids[i]);
  //			if(typeof chart.layers[ids[i]].g === "undefined")
  //				chart.placeLayer(ids[i]);
  		}
  		

  		for(var k in chart.layers){
  			if(ids.indexOf(k) == -1)
  				chart.remove_layer(k)
  			else {
  				chart.get_layer(k).updatePoints();
  				chart.get_layer(k).updatePointStyle();
  			}	
  		}
  		
  		inherited_update();
  		if(chart.showLegend() && Object.keys(chart.legend.blocks).length > 0)
  			chart.legend.update();
  		return chart;
  	}

  	var inherited_afterUpdate = chart.afterUpdate;
  	chart.afterUpdate = function(){
  		inherited_afterUpdate();
  		for(var k in chart.layers)
  			chart.get_layer(k).afterUpdate();
  	}

  	var inherited_updateSize = chart.updateSize;
  	chart.updateSize = function(){
  		inherited_updateSize();
  		for(var k in chart.layers)
  			chart.get_layer(k).updateSize();
  	}

  	return chart;
  }

  function axisChart() {
  	
  	var chart = layerChartBase();
  	
  	chart.add_property("singleScaleX", true)
  		.add_property("singleScaleY", true)
  		.add_property("domainX")
  		.add_property("domainY")
  		.add_property("aspectRatio", null)
  		.add_property("labelX")
  		.add_property("labelY")
  		.add_property("ticksX", undefined)
  		.add_property("ticksY", undefined);

  	chart.axes = {};
  	
  	//default getter for domain
  	//tries to make domain fit data from all layers
  	//for axis capital letters a supposed to be used
  	var get_domain = function(axis) {
  		return function() {
  			var domain;
  			//TODO: add possibility of adding several axises
  			//(one for each plot.layer)
  			if(chart["get_singleScale" + axis]()){
  				//if all the layers use continuous scale, make the scale continuous
  				//otherwise make it categorical
  				var contScale = true;
  				for(var k in chart.layers)
  					contScale = contScale && chart.get_layer(k)["get_contScale" + axis]();

  				if(contScale){//if resulting scale is continous, find minimun and maximum values
  					for(var k in chart.layers)
  						//some of the layers may not have domains at all (such as legends)
  						if(typeof chart.get_layer(k)["get_layerDomain" + axis]() !== "undefined")
  							if(typeof domain === "undefined") 
  								domain = chart.get_layer(k)["get_layerDomain" + axis]()
  							else {
  								domain[0] = d3.min([domain[0], chart.get_layer(k)["get_layerDomain" + axis]()[0]]);
  								domain[1] = d3.max([domain[1], chart.get_layer(k)["get_layerDomain" + axis]()[1]]);
  							}
  				} else { //if scale is categorical, find unique values from each layer
  					for(var k in chart.layers)
  						if(typeof chart.get_layer(k)["get_layerDomain" + axis]() !== "undefined")
  							if(typeof domain === "undefined") 
  								domain = chart.get_layer(k)["get_layerDomain" + axis]()
  							else 
  								domain = domain.concat(chart.get_layer(k)["get_layerDomain" + axis]()
  									.filter(function(e){
  										return domain.indexOf(e) < 0;
  									}));
  				}
  			}
  			return domain;
  		}
  	}

  	chart.get_domainX = get_domain("X");
  	chart.get_domainY = get_domain("Y");

  	//redefine setters for axis domains
  	chart.domainX = function(domain){
  		//set default getter
  		if(domain == "reset"){
  			chart.domainX(chart.origDomainX);
  			return chart;
  		}
  		//if user provided function, use this function
  		if(typeof domain === "function")
  			chart.get_domainX = domain;
  		if(domain.splice)
  			chart.get_domainX = function() {
  				return domain;
  			};
  			
  		return chart;
  	}
  	chart.domainY = function(domain){
  		if(domain == "reset"){
  			chart.domainY(chart.origDomainY);
  			return chart;
  		}
  		if(typeof domain === "function")
  			chart.get_domainY = domain;
  		if(domain.splice)
  			chart.get_domainY = function() {
  				return domain;
  			};
  		
  		return chart;
  	}

  	chart.zoom = function(lu, rb){
  		if(lu[0] == rb[0] || lu[1] == rb[1])
  			return;
      if(chart.axes.scale_x.invert)
      	chart.domainX([chart.axes.scale_x.invert(lu[0]), 
        	             chart.axes.scale_x.invert(rb[0])])
      else {
      	var newDomainX = [], domainX = chart.get_domainX(),
      		i = 0;
      	while(chart.axes.scale_x(domainX[i]) <= rb[0]){
  				if(chart.axes.scale_x(domainX[i]) >= lu[0])
  					newDomainX.push(domainX[i]);
  				i++;    	
      	}
      	if(newDomainX.length > 0)
      		chart.domainX(newDomainX);
      }

      if(chart.axes.scale_y.invert)
  	    chart.domainY([chart.axes.scale_y.invert(rb[1]),
                      chart.axes.scale_y.invert(lu[1])]);
      else {
      	var newDomainY = [], domainY = chart.get_domainY(),
      		i = 0;
      	while(chart.axes.scale_y(domainY[i]) <= rb[1]){
  				if(chart.axes.scale_y(domainY[i]) >= lu[1])
  					newDomainY.push(domainY[i]);
  				i++;    	
      	}
      	if(newDomainY.length > 0)
      		chart.domainY(newDomainY);
      }

      chart.updateAxes();
    }
    chart.resetDomain = function(){
      chart.domainX("reset");
      chart.domainY("reset");
      chart.updateAxes();
    }

    var inherited_put_static_content = chart.put_static_content;
    chart.put_static_content = function( element ) {
      inherited_put_static_content( element );
  		
  		var g = chart.svg.append("g")
  			.attr("class", "axes_g");

      chart.axes.x_g = g.append( "g" )
        .attr( "class", "x axis" );
      chart.axes.x_label = chart.axes.x_g.append( "text" )
        .attr( "class", "label" )
        .attr( "y", -6 )
        .style( "text-anchor", "end" );

      chart.axes.y_g = g.append( "g" )
        .attr( "class", "y axis" )
      chart.axes.y_label = chart.axes.y_g.append( "text" )
        .attr( "class", "label" )
        .attr( "transform", "rotate(-90)" )
        .attr( "y", 6 )
        .attr( "dy", ".71em" )
        .style( "text-anchor", "end" );

  		var domainX = chart.get_domainX();
  		if(domainX.length == 2 && typeof domainX[0] === "number")
  			chart.axes.scale_x = d3.scaleLinear()
  				.nice();
  		else{
  			chart.axes.scale_x = d3.scalePoint() 
  				.padding(0.3);	
  		}
  		chart.origDomainX = chart.get_domainX;
  		
  		var domainY = chart.get_domainY();
  		if(domainY.length == 2 && typeof domainY[0] === "number")
  			chart.axes.scale_y = d3.scaleLinear()
  				.nice();
  		else
  			chart.axes.scale_y = d3.scalePoint()
  				.padding(0.3); 

  		chart.origDomainY = chart.get_domainY;	
  		if(chart.showPanel()) {
  			chart.panel.add_button("Zoom in", "#zoomIn", function(chart){
  				var xDomain = chart.axes.scale_x.domain(),
  					yDomain = chart.axes.scale_y.domain();
  				if(xDomain.length == 2 && typeof xDomain[0] == "number" && typeof xDomain[1])
  					chart.domainX([(xDomain[0] * 4 + xDomain[1])/5, 
  												(xDomain[0] + xDomain[1] * 4)/5])
  				else {
  					var removeElements = Math.ceil(xDomain.length * 0.1);
  					xDomain.splice(0, removeElements);
  					xDomain.splice(xDomain.length - removeElements);
  					if(xDomain.length > 0)
  						chart.domainX(xDomain);
  				}
  				if(yDomain.length == 2 && typeof yDomain[0] == "number" && typeof yDomain[1])
  					chart.domainY([(yDomain[0] * 4 + yDomain[1])/5, 
  												(yDomain[0] + yDomain[1] * 4)/5]);
  				else {
  					var removeElements = Math.ceil(yDomain.length * 0.1);
  					yDomain.splice(0, removeElements);
  					yDomain.splice(yDomain.length - removeElements );
  					if(yDomain.length > 0)
  						chart.domainY(yDomain);
  				}
  				chart.updateAxes();

  			}, "Double click to return to original scales");
  			chart.panel.add_button("Zoom out", "#zoomOut", function(chart){
  				var xDomain = chart.axes.scale_x.domain(),
  					yDomain = chart.axes.scale_y.domain();
  				if(xDomain.length == 2 && typeof xDomain[0] == "number" && typeof xDomain[1])
  					chart.domainX([(xDomain[0] * 6 - xDomain[1])/5, 
  												(-xDomain[0] + xDomain[1] * 6)/5])
  				else{
  					var addElements = Math.ceil(xDomain.length * 0.1),
  						origDomainX = chart.origDomainX(),
  						start = origDomainX.indexOf(xDomain[0]),
  						end = origDomainX.indexOf(xDomain[xDomain.length - 1]);
  					for(var i = start - 1; i >= d3.max([0, start - addElements]); i--)
  						xDomain.unshift(origDomainX[i]);
  					for(var i = end + 1; i < d3.min([origDomainX.length, end + addElements + 1]); i++)
  						xDomain.push(origDomainX[i]);
  					chart.domainX(xDomain);
  				}
  				if(yDomain.length == 2 && typeof yDomain[0] == "number" && typeof yDomain[1])
  					chart.domainY([(yDomain[0] * 6 - yDomain[1])/5, 
  												(-yDomain[0] + yDomain[1] * 6)/5])
  				else{
  					var addElements = Math.ceil(yDomain.length * 0.1),
  						origDomainY = chart.origDomainY(),
  						start = origDomainY.indexOf(yDomain[0]),
  						end = origDomainY.indexOf(yDomain[yDomain.length - 1]);
  					for(var i = origDomainY[start - 1]; i >= d3.max([0, start - addElements]); i--)
  						yDomain.unshift(origDomainY[i]);
  					for(var i = end + 1; i < d3.min([origDomainY.length, end + addElements + 1]); i++)
  						yDomain.push(origDomainY[i]);

  					chart.domainY(yDomain);
  				}
  				chart.updateAxes();			
  			}, "Double click to return to original scales");
  		}
    }	
  	
  	var inherited_updateSize = chart.updateSize;
  	chart.updateSize = function() {
  		inherited_updateSize();

  		if(typeof chart.transition !== "undefined"){
  			chart.svg.select(".axes_g").transition(chart.transition)
  				.attr("transform", "translate(" + chart.get_margin().left + 
  								", " + chart.get_margin().top + ")");
  			chart.axes.x_g.transition(chart.transition)
  				.attr( "transform", "translate(0," + chart.get_plotHeight() + ")" );
  			chart.axes.x_label.transition(chart.transition)
  				.attr("x", chart.get_plotWidth());

  		}	else {
  			chart.svg.select(".axes_g")
  				.attr("transform", "translate(" + chart.get_margin().left + 
  								", " + chart.get_margin().top + ")");
  			chart.axes.x_g
  				.attr( "transform", "translate(0," + chart.get_plotHeight() + ")" );
  			chart.axes.x_label
  				.attr("x", chart.get_plotWidth());
  		}
  		chart.axes.scale_x.range([0, chart.get_plotWidth()]);
  		chart.axes.scale_y.range([chart.get_plotHeight(), 0]);

  		chart.updateAxes();

  		return chart;
  	};

  	// This function takes two linear scales, and extends the domain of one of them to get  
  	// the desired x:y aspect ratio 'asp'. 
  	function fix_aspect_ratio( scaleX, scaleY, asp ) { 
  	   var xfactor = ( scaleX.range()[1] - scaleX.range()[0] ) /  
  	      ( scaleX.domain()[1] - scaleX.domain()[0] ) 
  	   var yfactor = ( scaleY.range()[1] - scaleY.range()[0] ) /  
  	      ( scaleY.domain()[1] - scaleY.domain()[0] ) 
  	   var curasp = Math.abs( xfactor / yfactor )  // current aspect ratio 
  	   if( curasp > asp ) {  // x domain has to be expanded 
  	      var cur_dom_length = ( scaleX.domain()[1] - scaleX.domain()[0] ) 
  	      var extension = cur_dom_length * ( curasp/asp - 1 ) / 2 
  	      scaleX.domain( [ scaleX.domain()[0] - extension, scaleX.domain()[1] + extension ] )       
  	   } else { // y domain has to be expanded 
  	      var cur_dom_length = ( scaleY.domain()[1] - scaleY.domain()[0] ) 
  	      var extension = cur_dom_length * ( asp/curasp - 1 ) / 2 
  	      scaleY.domain( [ scaleY.domain()[0] - extension, scaleY.domain()[1] + extension ] )             
  	   } 
  	} 

  	var get_ticks = function(axis){
  		var ticks = {tickValues: null, tickFormat: null},
  			tickArray = chart["ticks" + axis]();
  		
  		if(tickArray){
  			//check if the ticks are set correctly
  			if(typeof tickArray.splice === "undefined")
  				throw "Error in 'get_ticks': new tick values and labels should be passed " +
  							"as an array";
  			if(typeof tickArray[0].splice === "undefined")
  				tickArray = [tickArray];
  			for(var i = 1; i < tickArray.length; i++)
  				if(tickArray[0].length != tickArray[i].length)
  					throw "Error in 'get_ticks': the amount of tick labels must be equal to the " +
  								"amount of tick values";

  			//if only tick values (not tick labels) then return 					
  			ticks.tickValues = tickArray[0];
  			if(tickArray.length == 1)
  				return ticks;

  			//if all the labels sets are identical, leave only one of them
  			var ident = tickArray.length > 2, j = 1, i;
  			while(ident && j < tickArray.length - 1){
  				i = 0;
  				while(ident && i < tickArray[j].length){
  					ident = (tickArray[j][i] == tickArray[j + 1][i]);
  					i++;
  				}
  				j++;
  			}
  			if(ident)
  				tickArray.splice(2);
  			
  			//if we have several label sets, transform the labels into <tspan> blocks
  			var tickLabels = [], value;
  			if(tickArray.length > 2){
  				for(var i = 0; i < tickArray[0].length; i++){
  					value = "";
  					for(var j = 1; j < tickArray.length; j++){
  						//location
  						value += "<tspan x = 0.5 dy = " + 1.1 + "em";
  						//colour if any
  						if(tickArray.colour) 
  							value += " fill = '" + tickArray.colour[j - 1] + "'>"
  						else
  							value += ">";
  						value += tickArray[j][i] + "</tspan>";
  					}
  					tickLabels.push(value);
  				}
  			} else {
  				tickLabels = tickArray[1];
  			}
  			ticks.tickFormat = function(d) {return tickLabels[ticks.tickValues.indexOf(d)];};
  		}
  		
  		return ticks;
  	}

  	chart.pan.move = function(p){
  		var domainX = chart.axes.scale_x.domain(),
  			domainY = chart.axes.scale_y.domain();
  		if(chart.axes.scale_x.invert){
  			var invPx = chart.axes.scale_x.invert(p[0]),
  				moveX = invPx - chart.axes.scale_x.invert(chart.pan.down[0]);
  			chart.pan.down[0] = p[0];
  			chart.domainX([domainX[0] - moveX, domainX[1] - moveX]);
  		} else {
  			var moveX = p[0] - chart.pan.down[0],
  				steps = Math.floor(Math.abs(moveX) / chart.axes.scale_x.step() * 2);
  			if(steps > 0){
  				chart.pan.down[0] += Math.sign(moveX) * steps * chart.axes.scale_x.step() /2;
  				var origDomainX = chart.origDomainX(),
  					start = origDomainX.indexOf(domainX[0]),
  					end = origDomainX.indexOf(domainX[domainX.length - 1]);
  				if(moveX < 0){
  					domainX.splice(0, steps);
  					domainX = domainX.concat(origDomainX.slice(end + 1, d3.min([origDomainX.length, end + steps + 1])));
  				} else {
  					domainX.splice(domainX.length - steps);
  					domainX = origDomainX.slice(d3.max([0, start - steps]), start).concat(domainX);
  				}
  				if(domainX.length > 0) chart.domainX(domainX);
  			}
  		}

  		if(chart.axes.scale_y.invert){
  			var invPy = chart.axes.scale_y.invert(p[1]),
  				moveY = invPy - chart.axes.scale_y.invert(chart.pan.down[1]);
  			chart.pan.down[1] = p[1];
  			chart.domainY([domainY[0] - moveY, domainY[1] - moveY]);
  		} else {
  			var moveY = p[1] - chart.pan.down[1],
  				steps = Math.floor(Math.abs(moveY) / chart.axes.scale_y.step() * 2);
  			if(steps > 0){
  				chart.pan.down[1] += Math.sign(moveY) * steps * chart.axes.scale_y.step() / 2;
  				var origDomainY = chart.origDomainY(),
  					start = origDomainY.indexOf(domainY[0]),
  					end = origDomainY.indexOf(domainY[domainY.length - 1]);
  				if(moveY > 0){
  					domainY.splice(0, steps);
  					domainY = domainY.concat(origDomainY.slice(end + 1, d3.min([origDomainY.length, end + steps + 1])));
  				} else {
  					domainY.splice(domainY.length - steps);
  					domainY = origDomainY.slice(d3.max([0, start - steps]), start).concat(domainY);
  				}
  				if(domainY.length > 0) chart.domainY(domainY);
  			}
  		}

  		chart.updateAxes();
  	}

  	chart.updateAxes = function(){
      chart.axes.x_label
      	.text( chart.get_labelX());
  		chart.axes.y_label
     		.text( chart.get_labelY() );
      chart.axes.scale_x.domain(chart.get_domainX());
  		chart.axes.scale_y.domain(chart.get_domainY());
  		if(chart.aspectRatio())
  			fix_aspect_ratio(chart.axes.scale_x, chart.axes.scale_y, chart.get_aspectRatio());

  		var ticksX = get_ticks("X"),
  			ticksY = get_ticks("Y");

      if(typeof chart.transition !== "undefined") {
  	    d3.axisBottom()
  	      .scale( chart.axes.scale_x )
  	      .tickValues(ticksX.tickValues)
  	      .tickFormat(ticksX.tickFormat)
  	      ( chart.axes.x_g.transition(chart.transition) );

  	    d3.axisLeft()
  	      .scale( chart.axes.scale_y )
  	      .tickValues(ticksY.tickValues)
  	      .tickFormat(ticksY.tickFormat)
  	      ( chart.axes.y_g.transition(chart.transition) );	
      } else {
  	    d3.axisBottom()
  	      .tickValues(ticksX.tickValues)
  	      .tickFormat(ticksX.tickFormat)
  	      .scale( chart.axes.scale_x )
  	      ( chart.axes.x_g );

  	    d3.axisLeft()
  	      .scale( chart.axes.scale_y )
  	      .tickValues(ticksY.tickValues)
  	      .tickFormat(ticksY.tickFormat) 
  	      ( chart.axes.y_g );    	
      }

      var updateX = function() {
      	chart.axes.x_g.selectAll(".tick").selectAll("text")
      		.html(ticksX.tickFormat)
      };
      if(ticksX.tickFormat)
      	if(chart.transition)
      		setTimeout(updateX, chart.transition.duration())
      	else
      		updateX();

      var updateY = function() {
      	chart.axes.y_g.selectAll(".tick").selectAll("text")
      		.html(ticksX.tickFormat)
      };
      if(ticksY.tickFormat)
      	if(chart.transition)
      		setTimeout(updateY, chart.transition.duration())
      	else
      		updateY();

      for(var k in chart.layers)
      	chart.get_layer(k).updatePointLocation();

      return chart;
  	}

  	return chart;
  }

  function scatterChart(id, chart) {

  	if(chart === undefined)
  		chart = axisChart();
  	if(id === undefined)
  		id = "layer" + chart.get_nlayers();

    var layer = chart.create_layer(id).get_layer(id)
  		.add_property("x")
  		.add_property("y")
      .add_property("size", 6)
      .add_property("stroke", function(d) {
        return d3.rgb(layer.get_colour(d)).darker(0.8)
      })
      .add_property("strokeWidth", function(d) {
        return layer.get_size(d) * 0.1;
      })
      .add_property("symbolType", "Circle")
  		.add_property("groupName", function(i){return i;})
      .add_property("informText", function(id){
        return "ID: <b>" + id + "</b>;<br>" + 
              "x = " + layer.get_x(id).toFixed(2) + ";<br>" + 
              "y = " + layer.get_y(id).toFixed(2)
      });
  	chart.syncProperties(layer);

    layer.type = "scatterChart";

    // Set default for numPoints, namely to count the data provided for x
    layer.npoints( function() {
      var val;
      for( var i = 0; i < 10000; i++ ) {
        try {
          // try to get a value
          val = layer.get_x(i)
        } catch( exc ) {
          // if call failed with exception, report the last successful 
          // index, if any, otherwise zero
          return i > 0 ? i-1 : 0;  
        }
        if( val === undefined ) {
          // same again: return last index with defines return, if any,
          // otherwise zero
          return i > 0 ? i-1 : 0;  
        }
      }
      // If we exit the loop, there is either something wrong or there are
      // really many points
      throw "There seem to be very many data points. Please supply a number via 'npoints'."
    });

    //default hovering behaviour
    layer.pointMouseOver(function(d){
      var pos = d3.mouse(chart.container.node());
      //change colour and class
      d3.select(this)
        .attr("fill", function(d) {
          return d3.rgb(layer.get_colour(d)).darker(0.5);
        })
        .classed("hover", true);
      //show label
      layer.chart.container.select(".inform")
          .style("left", (pos[0] + 10) + "px")
          .style("top", (pos[1] + 10) + "px")
          .select(".value")
            .html(layer.get_informText(d));  
      layer.chart.container.select(".inform")
        .classed("hidden", false);
    });
    layer.pointMouseOut(function(d){
      d3.select(this)
        .attr("fill", function(d) {
          return layer.get_colour(d);
        })
        .classed("hover", false);
      layer.chart.container.select(".inform")
        .classed("hidden", true);
    });


    //These functions are used to react on clicks
    layer.findPoints = function(lu, rb){
      return layer.g.selectAll(".data_point")
        .filter(function(d) {
          var loc = [layer.chart.axes.scale_x(layer.get_x(d)), 
                    layer.chart.axes.scale_y(layer.get_y(d))]
          return (loc[0] - layer.get_size(d) <= rb[0]) && 
            (loc[1] - layer.get_size(d) <= rb[1]) && 
            (loc[0] + layer.get_size(d) >= lu[0]) && 
            (loc[1] + layer.get_size(d) >= lu[1]);
        }).nodes().map(function(e) {return e.getAttribute("id")});
    }
    layer.get_position = function(id){
      return [layer.chart.axes.scale_x(layer.get_x(id)), 
              layer.chart.axes.scale_y(layer.get_y(id))];
    } 

  	layer.layerDomainX(function() {
  		if(layer.get_contScaleX()){
        return d3.extent( layer.get_dataIds(), function(k) { return layer.get_x(k) } )
      } else {
        return layer.get_dataIds().map(function(e) { return layer.get_x(e);});
      }
  	});
  	layer.layerDomainY(function() {
      if(layer.get_contScaleY()) {
  		  return d3.extent( layer.get_dataIds(), function(k) { return layer.get_y(k) } )
      } else{
        return layer.get_dataIds().map(function(e) { return layer.get_y(e);});
      }
  	});

    layer.updatePointLocation = function(){
      if(typeof layer.chart.transition !== "undefined"){
        layer.g.selectAll(".data_point").transition(layer.chart.transition)
          .attr("transform", function(d) {
            return "translate(" + layer.chart.axes.scale_x( layer.get_x(d) ) + ", " + 
            layer.chart.axes.scale_y( layer.get_y(d) ) + ")"
          });
      } else {
        layer.g.selectAll(".data_point")
          .attr("transform", function(d) {
            return "translate(" + layer.chart.axes.scale_x( layer.get_x(d) ) + ", " + 
            layer.chart.axes.scale_y( layer.get_y(d) ) + ")"
          });
      }
      var domainX = layer.chart.axes.scale_x.domain(),
        domainY = layer.chart.axes.scale_y.domain();
      var notShown = layer.g.selectAll(".data_point")
        .filter(function(d) {
          return layer.get_x(d) > domainX[1] || layer.get_x(d) < domainX[0] ||
                  layer.get_y(d) > domainY[1] || layer.get_y(d) < domainY[0];
        }).data();
      var outTicks = layer.g.selectAll(".out_tick").data(notShown, function(d) {return d});
      outTicks.exit().remove();
      outTicks.enter()
        .append("rect")
          .attr("class", "out_tick")
          .attr("fill", function(d){return layer.get_colour(d)})
          .merge(outTicks)
            .attr("width", function(d){
              return layer.get_y(d) > domainY[1] || layer.get_y(d) < domainY[0] ? 4 : 12;
            })
            .attr("height", function(d){
              return layer.get_x(d) > domainX[1] || layer.get_x(d) < domainX[0] ? 4 : 12;
            })
            .attr("x", function(d){
              return d3.max([layer.chart.axes.scale_x(domainX[0]), 
                layer.chart.axes.scale_x(d3.min([layer.get_x(d), domainX[1]])) - d3.select(this).attr("width")]);
            })
            .attr("y", function(d){
              return d3.min([layer.chart.axes.scale_y(domainY[0]) - d3.select(this).attr("height"), 
                layer.chart.axes.scale_y(d3.min([layer.get_y(d), domainY[1]]))]);
            })
            .on("mousedown", function(d){
              layer.chart.domainX([d3.min([domainX[0], layer.get_x(d)]), d3.max([domainX[1], layer.get_x(d)])]);
              layer.chart.domainY([d3.min([domainY[0], layer.get_y(d)]), d3.max([domainY[1], layer.get_y(d)])]);
              layer.chart.updateAxes();
            });
      
      return layer;
    }

    layer.updateSelPointStyle = function(id){
      if(typeof id.length === "undefined")
        id = [id];
      if(typeof layer.chart.transition !== "undefined")
        for(var i = 0; i < id.length; i++)
          layer.g.select("#p" + id[i]).transition(chart.layer.transition)
            .attr( "r", function(d) {return layer.get_size(d)})
            .attr( "fill", function(d) { return layer.get_colour(d)})
            .attr( "style", function(d) { return layer.get_style(d)})
      else
        for(var i = 0; i < id.length; i++)
          layer.g.select("#p" + id[i])
            .attr( "r", layer.get_size(id[i]))
            .attr( "fill", layer.get_colour(id[i]))
            .attr( "style", layer.get_style(id[i]));      
      return layer;
    }

    layer.updatePointStyle = function() {
      layer.resetColourScale();
      var ids = layer.get_dataIds();
      var sel = layer.g.selectAll(".data_point");
      if(typeof layer.chart.transition !== "undefined")
        sel = sel.transition(layer.chart.transition);
      sel
        .attr("d", function(d) {
          return d3.symbol()
            .type(d3["symbol" + layer.get_symbolType(d)])
            .size(get_symbolSize(layer.get_symbolType(d), layer.get_size(d)))();
        })
        .attr("fill", function(d) {return layer.get_colour(d)})
        .attr("stroke", function(d) {return layer.get_stroke(d)})
        .attr("stroke-width", function(d) {return layer.get_strokeWidth(d)})
    }

    layer.dresser(function(sel) {
      sel.attr("fill", function(d) {return layer.get_colour(d);})
        .attr("r", function(d) {return layer.get_size(d);});
    });

    layer.updatePoints = function(){
      var sel = layer.g.selectAll( ".data_point" )
        .data( layer.get_dataIds(), function(d) {return d;} );
      sel.exit()
        .remove();  
      sel.enter().append( "path" )
        .attr( "class", "data_point" )
        .merge(sel)
          .attr("id", function(d) {return "p" + (layer.id + "_" + d).replace(/ /g,"_");})
          .on( "click", layer.get_on_click )
          .on( "mouseover", layer.get_pointMouseOver )
          .on( "mouseout", layer.get_pointMouseOut );
    }

    return chart;
  }

  function lineChart(id, chart){
  	
  	if(chart === undefined)
  		chart = axisChart();
  	if(id === undefined)
  		id = "layer" + chart.get_nlayers();
  	
  	var layer = chart.create_layer(id).get_layer(id)
  		.add_property("lineFun")
  		.add_property("lineStepNum", 100)
  		.add_property("lineWidth", 1.5)
  		.add_property("dasharray", undefined);
  	chart.syncProperties(layer);
  	
  	layer.updatePoints = function(){
  		var lines = layer.g.selectAll(".data_point")
  			.data(layer.get_dataIds(), function(d) {return d;});
  		lines.exit()
  			.remove();
  		lines.enter()
  			.append("path")
  				.attr("class", "data_point")
  				.attr("fill", "none")
  			.merge(lines)
  				.attr("id", function(d) {return "p" + (layer.id + "_" + d).replace(/ /g,"_");})
          .on( "click", layer.get_on_click )
          .on( "mouseover", layer.get_pointMouseOver )
          .on( "mouseout", layer.get_pointMouseOut );			
  	};

  	layer.dresser(function(sel){
  		sel.attr("stroke", function(d) {return layer.get_colour(d);})
  			.attr("stroke-width", function(d) {return layer.get_lineWidth(d);})
  			.attr("stroke-dasharray", function(d) {return layer.get_dasharray(d)});
  	});

  	return layer;
  }

  function xLine(id, chart){
  	
  	var layer = lineChart(id, chart);

  	layer.type = "xLine";

  	layer.updatePointLocation = function(){
  		//define the length of each step
  		var lineStep = (layer.chart.axes.scale_x.domain()[1] - 
  										layer.chart.axes.scale_x.domain()[0]) / 
  										layer.get_lineStepNum();
  		var get_data = function(d){
  			var lineData = [];
  			for(var i = layer.chart.axes.scale_x.domain()[0]; 
  					i < layer.chart.axes.scale_x.domain()[1]; i += lineStep)
  			lineData.push({
  				x: i,
  				y: layer.get_lineFun(d, i)
  			});
  							
  			var line = d3.line()
  				.x(function(c) {return layer.chart.axes.scale_x(c.x);})
  				.y(function(c) {return layer.chart.axes.scale_y(c.y);});
  							
  			return line(lineData);
  		};
  		
  		if(typeof layer.chart.transition !== "undefined")
  			layer.g.selectAll(".data_point").transition(layer.chart.transition)
  				.attr("d", get_data)
  		else
  			layer.g.selectAll(".data_point")
  				.attr("d", get_data);			
  	};

  	return layer;
  }

  function yLine(id, chart){
  	
  	var layer = lineChart(id, chart);

  	layer.type = "yLine";

  	layer.updatePointLocation = function(){
  		//define the length of each step
  		var lineStep = (layer.chart.axes.scale_y.domain()[1] - 
  										layer.chart.axes.scale_y.domain()[0]) / 
  										layer.get_lineStepNum();
  		var get_data = function(d){
  			var lineData = [];
  			for(var i = layer.chart.axes.scale_y.domain()[0]; 
  					i < layer.chart.axes.scale_y.domain()[1]; i += lineStep)
  			lineData.push({
  				y: i,
  				x: layer.get_lineFun(d, i)
  			});
  							
  			var line = d3.line()
  				.x(function(c) {return layer.chart.axes.scale_x(c.x);})
  				.y(function(c) {return layer.chart.axes.scale_y(c.y);});
  							
  			return line(lineData);
  		};
  		
  		if(typeof layer.chart.transition !== "undefined")
  			layer.g.selectAll(".data_point").transition(layer.chart.transition)
  				.attr("d", get_data)
  		else
  			layer.g.selectAll(".data_point")
  				.attr("d", get_data);			
  	};

  	return layer;
  }

  function parametricCurve(id, chart){
  	var layer = lineChart(id, chart);

  	layer.type = "paramCurve";

  	layer
  		.add_property("xFunction")
  		.add_property("yFunction")
  		.add_property("paramRange", [0, 1]);
  	layer.chart.syncProperties(layer);

  	var get_data = function(d){
  		var paramRange = layer.paramRange();
  		
  		if(paramRange[1] < paramRange[0])
  			paramRange = [paramRange[1], paramRange[0]];

  		var lineStep = (paramRange[1] - paramRange[0]) / 
  										layer.get_lineStepNum();

  		var lineData = [];
  		for(var t = paramRange[0]; t < paramRange[1]; t += lineStep)
  			lineData.push({
  				x: layer.get_xFunction(d, t),
  				y: layer.get_yFunction(d, t)
  			});
  			
  		return lineData;
  	};	

  	layer.updatePointLocation = function(){
  		
  		var line = d3.line()
  			.x(function(c) {return layer.chart.axes.scale_x(c.x);})
  			.y(function(c) {return layer.chart.axes.scale_y(c.y);});

  		if(typeof layer.chart.transition !== "undefined")
  			layer.g.selectAll(".data_point").transition(layer.chart.transition)
  				.attr("d", function(d) {return line(get_data(d));})
  		else
  			layer.g.selectAll(".data_point")
  				.attr("d", function(d) {return line(get_data(d));});
  	}

  	layer.layerDomainX(function() {
  		var dataIds = layer.dataIds(),
  			domainX = [];
  		for(var i = 0; i < dataIds.length; i++)
  			domainX = domainX.concat(d3.extent(get_data(dataIds[i]).map(function(e) {return e.x})));
  		return d3.extent(domainX);
  	});

  	layer.layerDomainY(function() {
  		var dataIds = layer.dataIds(),
  			domainY = [];
  		for(var i = 0; i < dataIds.length; i++)
  			domainY = domainY.concat(d3.extent(get_data(dataIds[i]).map(function(e) {return e.y})));
  		return d3.extent(domainY);
  	});

  	return layer;
  }

  var intersect = function(ar1, ar2)
  {
  	var ar_int = []
  	var ar = [ar1, ar2]
  	var l = [ar1.length, ar2.length];
  	var ind = l.indexOf(Math.min.apply(null,l))
  	var ind_large = ind == 0 ? 1:0
  	for(var i = 0; i < ar[ind].length; i++)
  	{
  		if(ar[ind_large].indexOf(ar[ind][i]) != -1)
  			ar_int.push(ar[ind][i])
  	}
  	return ar_int
  }

  function Node(id, val)
  {
    this.id = id;
    this.val = val;
    this.left = null;
    this.right = null;
    //this.left_height = null;
    //this.right_height = null;
    this.parent = null;
    this.height = 0;
    this.x = null;
    this.val_inds = [id];

    var traverse = function()
  	{
  		//console.log(node.id);
  		//if(node == null)
  		//	return;
  		if(this.left == null && this.right == null)
  			console.log(this.id, this.height);
  		var children = [this.left, this.right];
  		for(var i = 0; i < children.length; i++	)
  		{
  			if(children[i] != null)
  					traverse(children[i]);
  		}
  		return;
  	};
  }
       
  function dendogram(heatmap)
  {
  	var dendogram = lc.base()
  		.add_property("orientation", "h")
  		.add_property("height", 100)
  		.add_property("width", 300)
  		.add_property("npoints") //nlabels
  		.add_property("dataIds", function(){return undefined}) //labIds
  		.add_property("margin", {left:20, top:20, bottom:20, right:20}) //padding
  		.add_property("distance", function(a, b){
  			return lc.getEuclideanDistance(a, b);			
  		})
  		.add_property("data")
  		.add_property("scales")
  		.add_property("css_class", ['normal', 'selected']);

  	dendogram.npoints("_override_", "dataIds", function()
  	{
  		return d3.range(dendogram.npoints());
  	})
  	dendogram.dataIds("_override_", "npoints", function()
  	{
  		return dendogram.dataIds().length;
  	})

  	dendogram.heatmap = heatmap;
  	dendogram.clusters = undefined;
  		
  	var n_count = 0;		 
  	dendogram.set_x = function(node)
  	{ 
  		if(node.right == null && node.left == null)
  		{
  			node.x = n_count;
  			n_count++;
  			return;
  		}
  		if(node.left.x == null)
  			this.set_x(node.left);
  		if(node.right.x == null)
  			this.set_x(node.right);
  		node.x = (node.right.x + node.left.x)/2 ;
  		return;
  	}
  	dendogram.set_scale = function()
  	{
  		var t = -1;
  		var rev_height = 0;
  		var bucket_final = this.clusters,
  		 	padding = this.margin(),
  		 	width = this.width(),
  		 	height = this.height();
  		//var n_leaves = bucket_final.val_inds.length;
  		var n_leaves = dendogram.dataIds().length;
  		var box_width = (width - padding.right - padding.left)/n_leaves;
  		var xScale = d3.scaleLinear()
  					   .domain([0, n_leaves-1])
  					   .range([padding.left + box_width/2, 
  					   	width - padding.right - box_width/2]);
  		if(dendogram.get_orientation() == 'v')
  		{	rev_height = height; t = 1;}

  		var yScale = d3.scaleLinear()			   
  						.domain([0, bucket_final.height])
  						.range([height+padding.top*t-rev_height,
  						 rev_height-padding.bottom*t]);
  		return [xScale, yScale];
  	}
  	dendogram.draw_dendo = function(node, g, scales)
  	{
  		//var height = svg.style()[0][0].getAttribute("height");
  		if(node.right != null && node.left != null)
  		{
  			g.append("line")
  			.attr("x1", scales[0](node.left.x))
  			.attr("x2", scales[0](node.right.x))
  			.attr("y1", scales[1](node.height))
  			.attr("y2", scales[1](node.height))
  			.attr("stroke-width", 3)
  			.attr("id", node.id)	
  			.attr("orient", "h")
  			.attr("class", "normal");

  			var children = [node.left, node.right];
  			for(var i = 0; i < children.length; i++)
  			{
  				g.append("line")
  				.attr("x1", scales[0](children[i].x))
  				.attr("x2", scales[0](children[i].x))
  				.attr("y1",  scales[1](node.height))
  				.attr("y2", scales[1](children[i].height))
  				.attr("stroke-width", 3)				
  				.attr("id", children[i].id)
  				.attr("orient", "v")
  				.attr("class", "normal");
  			}
  			
  			this.draw_dendo(node.left, g, scales);
  			this.draw_dendo(node.right, g, scales);

  			return;
  		}

  		if(node.right != null || node.left != null){
  			var child = node.right || node.left;
  			g.append("line")
  				.attr("x1", scales[0](node.x))
  				.attr("x2", scales[0](node.x))
  				.attr("y1", scales[1](node.height))
  				.attr("y2", scales[1](child.height))
  				.attr("stroke-width", 3)
  				.attr("id", child.id)
  				.attr("orient", "v")
  				.attr("class", "normal");

  			this.draw_dendo(child, g, scales);
  		}

  		return;
  	}
  	dendogram.find_node = function(node, id)
  	{	
  		if(node != null)
  		{
  			if(node.id == id)
  				return node;
  			else 
  				return dendogram.find_node(node.left,id) || dendogram.find_node(node.right, id);
  		}
  		return null;
  	}
  	dendogram.add_ids = function(node, inds)
  	{
  		inds.push(node.id);
  		if(node.left != null)
  			dendogram.add_ids(node.left, inds);
  		if(node.right != null)
  			dendogram.add_ids(node.right, inds);
  	}

  	dendogram.check_ele = function(ele, ind_arr)
  	{
  		for(var i = 0; i < ind_arr.length; i++)
  		{
  			if(ind_arr[i] == ele)
  				return true;
  		}
  		return false;
  	}

  	dendogram.set_color = function(g, inds, cla, prop)
  	{
  		g.selectAll('line').attr("class", function(d)
  		{
  			return dendogram.check_ele(this.id, inds) ? cla[1]:cla[0];
  		});
  	}

  	//???
  	dendogram.change_prop = function(root_node, id_and_type, g, cla)
  	{
  		//console.log(root_node);
  		var node_req = dendogram.find_node(root_node, id_and_type[0]),
  			inds = [],
  			prop = -1;
  		
  		dendogram.add_ids(node_req, inds);
  		
  		if(id_and_type[1] == 'h')
  			prop = id_and_type[0];



  		if(dendogram.heatmap != undefined)
  		{
  			if(dendogram.orientation() == 'h')
  			{
  				var inds_int = intersect(inds.map(function(e) {return e.toString()}),
  					dendogram.heatmap.colIds())
  				dendogram.heatmap.cluster('Row', inds_int)
  				//TO DO: check if dendogram already exists
  				dendogram.heatmap.drawDendogram("Row");
  			}
  			if(dendogram.orientation() == 'v')
  			{
  				var inds_int = intersect(inds.map(function(e) {return e.toString()}),
  					dendogram.heatmap.rowIds())
  				dendogram.heatmap.cluster('Col', inds_int)
  				dendogram.heatmap.drawDendogram("Col");
  			}
  			dendogram.heatmap.updateLabelPosition();
  		}

  		//dendogram.set_color(g, [], cla, -1);
  		dendogram.set_color(g, inds, cla, prop);		
  	}

  	dendogram.set_click = function(root, g, cla)
  	{
  		g.selectAll('line').on('click', function(d){
  		dendogram.change_prop(root, [this.getAttribute('id'), this.getAttribute('orient')], g, cla);});
  		return dendogram;
  	}

  	dendogram.cluster = function(){
  		var keys = dendogram.dataIds();
  		dendogram.bucket = []
  		//Initialisation
  		for(var i = 0; i < keys.length; i++)
  			dendogram.bucket[i]  = new Node(keys[i], dendogram.get_data(keys[i]));	
  		var bucket_dist = function(el1_inds, el2_inds)
  		{
  			var max_dist = dendogram.get_distance(dendogram.get_data(el1_inds[0]), dendogram.get_data(el2_inds[0]));
  			for(var i = 0; i < el1_inds.length; i++)
  			{
  				for(var j = 0; j < el2_inds.length; j++)	
  					{
  						var dis = dendogram.get_distance(dendogram.get_data(el1_inds[i]), dendogram.get_data(el2_inds[j]));
  						if(dis > max_dist)
  							max_dist = dis;
  					}
  			}
  			return max_dist;
  		}
  		
  		var merge = function()
  		{
  			var cur_count = dendogram.bucket.length;
  			var bucket_copy = JSON.parse(JSON.stringify(dendogram.bucket));
  			while(bucket_copy.length >  1)
  			{
  				var to_clus = [bucket_copy[0], bucket_copy[1]];
  				var min_dis = bucket_dist(bucket_copy[0].val_inds, bucket_copy[1].val_inds);	
  				var to_rem = [0,1];
  				for(var i = 0; i < bucket_copy.length; i++)
  				{
  					for(var j = i+1;  j < bucket_copy.length; j++)
  					{
  						 var dis = bucket_dist(bucket_copy[i].val_inds, bucket_copy[j].val_inds);
  						 if(dis < min_dis)
  						 {
  						 	min_dis = dis;
  						 	to_clus = [bucket_copy[i], bucket_copy[j]];
  						 	to_rem = [i,j];
  						 }
  					}
  				}
  				//console.log(min_dis);
  				var new_node = new Node(to_clus[0].id + to_clus[1].id, null);
  				new_node.left = to_clus[0];
  				new_node.right = to_clus[1];
  				new_node.height = min_dis;
  				new_node.val_inds = new_node.left.val_inds.concat(new_node.right.val_inds);
  				to_rem.sort(function(a,b){return b-a});
  				bucket_copy.splice(to_rem[0],1);
  				bucket_copy.splice(to_rem[1],1);
  				bucket_copy.push(new_node);
  				cur_count += 1;
  				//break;
  			}
  			return bucket_copy[0];
  		}
  		//var dist_mat = dendogram.calc_dist();
  		//console.log(dist_mat[0])
  		dendogram.clusters = merge();

  		if(dendogram.heatmap){

  		}		
  	}

  	dendogram.calc_dist = function(){
  		var keys = dendogram.dataIds();
  		var dist = {};
  		for(var i = 0; i < keys.length; i++)
  		{
  			dist[keys[i]] = {};
  			for(var j = i; j < keys.length; j++)
  			{
  				var d = dendogram.get_distance(dendogram.get_data(keys[i]), dendogram.get_data(keys[j]));
  				dist[i][j] = d;
  				dist[j][i] = d;
  			}
  		}
  		return dist;
  	}

  	dendogram.draw = function()
  	{
  		dendogram.set_x(dendogram.clusters);
  		n_count = 0;
  		if(dendogram.orientation() == "v"){
  			if(dendogram.heatmap){
  				dendogram.width(dendogram.heatmap.height())
  					.height(dendogram.heatmap.margin().left)
  					.margin({
  						left: dendogram.heatmap.margin().top,
  						top: 0,
  						bottom: 0,
  						right: dendogram.heatmap.margin().bottom
  					});
  				dendogram.svg.select(".row")
  					.attr("transform", "translate(" + 
  														(dendogram.heatmap.margin().left + +dendogram.heatmap.plotWidth() + 5) + 
  														", " + dendogram.heatmap.margin().top + ")")
  					.selectAll("text")
  						.style("text-anchor", "start");
  			}
  			else
  				dendogram.svg
  					.attr("width", dendogram.width())
  					.attr("height", dendogram.height());
  			dendogram.g
  				.attr("transform", "rotate(90) translate(0, -" + dendogram.margin().left + ")");
  		} else {
  			if(dendogram.heatmap) {
  				dendogram.width(dendogram.heatmap.width())
  					.height(dendogram.heatmap.margin().top)
  					.margin({
  						top: 0,
  						left: dendogram.heatmap.margin().left,
  						right: dendogram.heatmap.margin().right,
  						bottom: 0
  					});

  				dendogram.svg.select(".col")
  					.attr("transform", "translate(" + 
  														 + dendogram.margin().left + 
  														", " + (dendogram.heatmap.margin().top + +dendogram.heatmap.plotHeight() + 5) + ")")
  					.selectAll("text")
  						.style("text-anchor", "end");
  				}
  			else
  				dendogram.svg
  					.attr("width", dendogram.width())
  					.attr("height", dendogram.height());
  //			dendogram.g
  //				.attr("transform", "translate(" + dendogram.margin().left + 
  //																	", " + dendogram.margin().right + ")");
  		}

  		var newTree = dendogram.trimNodes();
  		if(newTree === undefined)
  			return;
  		dendogram.scales(dendogram.set_scale());
  		dendogram.draw_dendo(newTree, dendogram.g, dendogram.get_scales() )
  		dendogram.set_click(newTree, dendogram.g, dendogram.get_css_class())		
  		return dendogram;
  	}

  	dendogram.trimNodes = function(){
  		var dataIds = dendogram.dataIds(),
  			newTree = {
  				id: dendogram.clusters.id,
  				left: null,
  				right: null,
  				val_inds: dendogram.clusters.val_inds,
  				x: dendogram.clusters.x,
  				val: dendogram.clusters.val,
  				original: dendogram.clusters,
  				height: dendogram.clusters.height,
  				parent: null
  			};
  		
  		var copyId = function(node, id){
  			if(node.height == 0)
  				return;
  			if(node.left && node.left.val_inds.indexOf(id) != -1){
  				copyId(node.left, id);
  				return;
  			}
  			if(node.right && node.right.val_inds.indexOf(id) != -1){
  				copyId(node.right, id);
  				return;
  			}
  			if(node.original.left.val_inds.indexOf(id) != -1){
  				node.left = {
  					id: node.original.left.id,
  					left: null,
  					right: null,
  					val_inds: node.original.left.val_inds,
  					x: node.original.left.x,
  					val: node.original.left.val,
  					original: node.original.left,
  					height: node.original.left.height,
  					parent: node
  				};
  				copyId(node.left, id);
  				return;
  			}
  			node.right = {
  				id: node.original.right.id,
  				left: null,
  				right: null,
  				val_inds: node.original.right.val_inds,
  				x: node.original.right.x,
  				val: node.original.right.val,
  				original: node.original.right,
  				height: node.original.right.height,
  				parent: node				
  			}
  			copyId(node.right, id);
  		}
  		
  		for(var i = 0; i < dataIds.length; i++){
  			if(dendogram.clusters.val_inds.indexOf(dataIds[i]) == -1){
  				if(dendogram.heatmap){
  					dendogram.remove();
  					return undefined;
  				}
  				dendogram.cluster();
  				dendogram.set_x(dendogram.clusters);
  				return dendogram.clusters;
  			}
  			copyId(newTree, dataIds[i]);
  		}
  		var currentPosition = 0;
  		var cutBranches = function(node) {
  			if(node === null) return;
  			cutBranches(node.left);
  			cutBranches(node.right);
  			if(node.height == 0){
  				node.x = currentPosition;
  				currentPosition++;
  				return;
  			}
  			if(node.left && node.right)
  				node.x = (node.left.x + node.right.x)/2;
  			if(node.parent == null)
  				return;
  			if(node.left == null){
  				node.x = node.right.x;
  				//node.parent.left = node.right.left;
  				//node.parent.right = node.right.right;
  				//node.right.parent = node.parent;
  			}
  			if(node.right == null){
  				node.x = node.left.x;
  				//node.parent.left = node.left.left;
  				//node.parent.right = node.left.right;
  				//node.left.parent = node.parent;
  			}
  		}

  		cutBranches(newTree);

  		var findRoot = function(node){
  			if(node.left && node.right)
  				return node;
  			if(node.left)
  				return findRoot(node.left);
  			if(node.right)
  				return findRoot(node.right);
  		}

  		return findRoot(newTree);
  	}
  	
  	dendogram.put_static_content = function(element)
  	{
  		if(dendogram.heatmap){
  			dendogram.g = dendogram.heatmap.svg
  				.append("g")
  					.attr("class", "dendogram");
  			dendogram.svg = heatmap.svg;
  			dendogram.container = heatmap.container;
  		} else {
  			dendogram.container = element.append("div")
  				.styel("position", "relative");
  			dendogram.svg = dendogram.container
  				.append("svg");
  			dendogram.g = dendogram.svg
  				.append("g");
  		}

  		return dendogram;		
  	}

  	dendogram.remove = function(){
  		dendogram.g.remove();
  		var type;
  		if(dendogram.heatmap){
  			var chart = dendogram.heatmap;
  			dendogram.orientation() == "h" ? type = "Col" : type = "Row";
  			chart.showDendogram(type, false);
  			chart["dendogram" + type] = undefined;
  			if(chart.transition){
  				chart.svg.selectAll(".label_panel." + type.toLowerCase()).transition(chart.transition)
  						.attr("transform", "translate(" + chart.margin().left + ", " +
  									chart.margin().top + ")");
  				if(type == "Row")
  					chart.svg.select(".row").selectAll("text").transition(chart.transition)
  						.style("text-anchor", "end")
  				else
  					chart.svg.select(".col").selectAll("text").transition(chart.transition)
  						.style("text-anchor", "start");
  			}
  			else{ 			
  				chart.svg.selectAll(".label_panel." + type.toLowerCase())
  						.attr("transform", "translate(" + chart.margin().left + ", " +
  									chart.margin().top + ")");
  				if(type == "Row")
  					chart.svg.select(".row").selectAll("text")
  						.style("text-anchor", "end");
  				else	
  					chart.svg.select(".col").selectAll("text")
  						.style("text-anchor", "start");
  			}
  		}
  	}	
  	
    dendogram.place = function( element ) {

      if( element === undefined )
        element = "body";
      if( typeof( element ) == "string" ) {
        var node = element;
        element = d3.select( node );
        if( element.size() == 0 )
          throw "Error in function 'place': DOM selection for string '" +
            node + "' did not find a node."
    	}

  		dendogram.put_static_content( element );
  		//dendogram.cluster();
  		//dendogram.draw();
      return dendogram;
    }

  	return dendogram;
  }

  function heatmapChart(id, chart){

  	var chart = chartBase()
  		.add_property("nrows")
  		.add_property("ncols");
  	
  	chart.add_property("colLabels", function(i) {return i;})
  		.add_property("rowLabels", function(i) {return i;})
  		.add_property("colIds", function() {return undefined})
  		.add_property("rowIds", function() {return undefined})
  		.add_property("dispColIds", function() {return chart.colIds();})
  		.add_property("dispRowIds", function() {return chart.rowIds();})
  		.add_property("heatmapRow", function(rowId) {return chart.dispRowIds().indexOf(rowId);})
  		.add_property("heatmapCol", function(colId) {return chart.dispColIds().indexOf(colId);})
  		.add_property("showDendogramRow", true)
  		.add_property("showDendogramCol", true)
  		.add_property("value")
  		.add_property("mode", "default")
  		.add_property("colour", function(val) {return chart.colourScale(val);})
  		.add_property("palette", d3.interpolateOrRd) //? Do we need it? Really
  		.add_property("colourRange", function() {return chart.dataRange()})
  		.add_property("clusterRowMetric", getEuclideanDistance)
  		.add_property("clusterColMetric", getEuclideanDistance)
  		.add_property("on_click", function() {})
  		.add_property("rowTitle", "")
  		.add_property("showValue", false)
  		.add_property("colTitle", "")
  		.add_property("showLegend", true)
  		.add_property("informText", function(rowId, colId) {
  			return "Row: <b>" + rowId + "</b>;<br>" + 
  						"Col: <b>" + colId + "</b>;<br>" + 
  						"value = " + chart.get_value(rowId, colId).toFixed(2)
  			});

  	chart.margin({top: 100, left: 100, right: 10, bottom: 40});

  	chart.ncols("_override_", "colIds", function(){
  		return d3.range(chart.get_ncols()).map(function(e) {return e.toString()});
  	});
  	chart.nrows("_override_", "rowIds", function(){
  		return d3.range(chart.get_nrows()).map(function(e) {return e.toString()});
  	});
  	chart.rowIds("_override_", "nrows", function(){
  		return chart.get_rowIds().length;
  	});
  	chart.colIds("_override_", "ncols", function(){
  		return chart.get_colIds().length;
  	});
  	chart.rowIds("_override_", "dispRowIds", function(){
  		return chart.rowIds();
  	})
  	chart.colIds("_override_", "dispColIds", function(){
  		return chart.colIds();
  	})

  	chart.axes = {};
  	chart.marked = [];

  	var inherited_put_static_content = chart.put_static_content;
  	chart.put_static_content = function(element){
  		inherited_put_static_content(element);
  		add_click_listener(chart);
  		//create main parts of the heatmap
  		chart.svg.append("g")
  			.attr("class", "row label_panel");
  		chart.svg.append("g")
  			.attr("class", "col label_panel");
  		chart.canvas = chart.container.append("canvas")
  			.style("position", "absolute")
  			.style("z-index", -5);		
  		chart.g = chart.svg.select(".plotArea").append("g")
  			.attr("class", "chart_g")
  			.attr("clip-path", "url(#" + chart.svg.select("clipPath").attr("id") + ")");
  		chart.text = chart.g.append("g")
  			.attr("class", "text_g");
  		chart.axes.x_label = chart.svg.append("text")
  			.attr("class", "axisLabel")
  			.attr("text-anchor", "end");
  		chart.axes.y_label = chart.svg.append("text")
  			.attr("class", "axisLabel")
  			.attr("text-anchor", "end")
  			.attr("transform", "rotate(-90)");
  		chart.svg.append("g")
  			.attr("class", "legend_panel");

  		(get_mode() == "svg") ? chart.g.classed("active", true) : 
  														chart.canvas.classed("active", true);

  		chart.svg.select(".clickPanel")
  			.on("mouseover", function() {
  				chart.container.select(".inform").classed("hidden", false);
  			})
  			.on("mouseout", function() {
  				chart.container.select(".inform").classed("hidden", true);
  			});

  		(function() {
  			var show = {Row: chart.showDendogramRow(), Col: chart.showDendogramCol()};
  			chart.showDendogram = function(type, sh){
  				if(sh === undefined)
  					return show[type];
  				show[type] = sh && chart["showDendogram" + type]();		
  			}
  		})();

  		if(chart.showPanel()){
  			chart.panel.add_button("Zoom in", "#zoomIn", function(chart){
  				var removeRows = -Math.ceil(chart.dispRowIds().length * 0.1),
  					removeCols = -Math.ceil(chart.dispColIds().length * 0.1);
  				chart.dispRowIds(chart.addLines(removeRows, "top"));
  				chart.dispRowIds(chart.addLines(removeRows, "bottom"));
  				chart.dispColIds(chart.addLines(removeCols, "left"));
  				chart.dispColIds(chart.addLines(removeCols, "right"));

  				chart.updateStarted = true;
  				chart.updateLabels();
  				chart.updateLabelPosition();
  				chart.updateStarted = false;				
  			}, "Double click to return to original scales");
  			chart.panel.add_button("Zoom out", "#zoomOut", function(chart){
  				var addRows = Math.ceil(chart.dispRowIds().length * 0.1),
  					addCols = Math.ceil(chart.dispColIds().length * 0.1);
  				
  				chart.dispRowIds(chart.addLines(addRows, "top"));
  				chart.dispRowIds(chart.addLines(addRows, "bottom"));
  				chart.dispColIds(chart.addLines(addCols, "left"));
  				chart.dispColIds(chart.addLines(addCols, "right"));

  				chart.updateStarted = true;
  				chart.updateLabels();
  				chart.updateLabelPosition();
  				chart.updateCellColour();
  				chart.updateLabelText();
  				chart.updateStarted = false;
  			}, "Double click to return to original scales");
  			chart.panel.add_button("Cluster rows", "#clusterRows", function(chart){
  				chart.cluster("Row");
  				chart.showDendogram("Row", true);
  				chart.updateLabelPosition();
  			});
  			chart.panel.add_button("Cluster columns", "#clusterCols", function(chart){
  				chart.cluster("Col");
  				chart.showDendogram("Col", true);
  				chart.updateLabelPosition();
  			});			
  		}
  	}

  	var get_mode = function() {
  		if(chart.mode() == "default")
  			return chart.dispColIds().length * chart.dispRowIds().length > 2500 ? "canvas" : "svg";
  		return chart.mode();
  	}

  	chart.findPoints = function(lu, rb){
  		var selectedIds = [];
  		if(get_mode() == "svg") {
  			var selectedPoints = chart.g.selectAll(".data_point")
  				.filter(function() {
  					var loc = [this.x.baseVal.value, this.y.baseVal.value];
  					return (loc[0] <= rb[0]) && (loc[1] <= rb[1]) && 
  						(loc[0] + chart.cellSize.width >= lu[0]) && 
  						(loc[1] + chart.cellSize.height >= lu[1]);
  				});
  			selectedIds = selectedPoints.data().map(function(e){
  				return "p" + e[0] + "_-sep-_" + e[1];
  			});
  		} else {
  			var selCols = chart.svg.select(".col").selectAll(".label")
  				.filter(function() {
  					var loc = this.y.baseVal[0].value;
  					return (loc >= lu[0] && loc <= rb[0] + chart.cellSize.width)
  				}).data(),
  				selRows = chart.svg.select(".row").selectAll(".label")
  				.filter(function() {
  					var loc = this.y.baseVal[0].value;
  					return (loc >= lu[1] && loc <= rb[1] + chart.cellSize.height)
  				}).data();
  			for(var i = 0; i < selRows.length; i++)
  				for(var j = 0; j < selCols.length; j++)
  					selectedIds.push("p" + selRows[i] + "_-sep-_" + selCols[j]);
  		}

  		return selectedIds;
  	}
  	chart.get_position = function(id){
  		return [chart.axes.scale_x(chart.get_heatmapCol(id[1])) + chart.cellSize.width/2,
  						chart.axes.scale_y(chart.get_heatmapRow(id[0])) + chart.cellSize.height/2]
  	}	
  	//returns maximum and minimum values of the data
  	chart.dataRange = function(){
  		var i = 0, range, newRange,
  			rowIds = chart.get_rowIds(),
  			colIds = chart.get_colIds();
  		do{
  			newRange = d3.extent(colIds, 
  				function(col) {return chart.get_value(rowIds[i], col);});
  			if(typeof range === "undefined")
  				range = newRange;
  			if(newRange[0] < range[0])
  				range[0] = newRange[0];
  			if(newRange[1] > range[1])
  				range[1] = newRange[1];
  			i++;
  		}while (i < chart.get_nrows())
  			
  		return range;
  	}

  	//set default hovering behaviour
  	chart.labelMouseOver = function() {
  		d3.select(this).classed("hover", true);
  	};
  	chart.labelMouseOut = function() {
  		d3.select(this).classed("hover", false);
  	};

  	chart.reorder = function(type, f){
  		if(f == "flip"){
  			chart["get_heatmap" + type]("__flip__");
  			chart.updateLabelPosition();
  			return chart;
  		}
  		f.domain = chart["disp"+ type + "Ids"]().slice();
  		var orderedIds = chart["get_heatmap" + type]("__order__");
  		if(orderedIds == -1)
  			orderedIds = chart[type.toLowerCase() + "Ids"]().slice();

  		var savedOrder = orderedIds.slice();
  		var newF = function(a, b){
  			if(f.domain.indexOf(a) != -1 && f.domain.indexOf(b) != -1)
  				return f(a, b);
  			if(savedOrder.indexOf(a) != -1 && savedOrder.indexOf(b) != -1)
  				return savedOrder.indexOf(a) - savedOrder.indexOf(b);
  			return chart[type.toLowerCase() + "Ids"]().indexOf(a) -
  							chart[type.toLowerCase() + "Ids"]().indexOf(b);
  		}

  		var dispIds = chart["disp" + type + "Ids"]().slice().sort(f);
  		orderedIds.sort(newF);
  		var mult = 1;

  		chart["heatmap" + type](function(id){
  			if(id == "__flip__"){
  				mult *= -1;
  				//dispIds.reverse();
  				orderedIds.reverse();
  				return;
  			}
  			if(id == "__order__")
  				return orderedIds;
  			if(id == "__sort__"){
  				dispIds = chart["disp" + type + "Ids"]().sort(function(a, b){return mult * newF(a, b);});
  				return;
  			}

  			return dispIds.indexOf(id);
  		});

  		if(chart.svg){
  			chart.svg.select(".col").selectAll(".label")
  				.classed("selected", false)
  				.classed("sorted", false);		
  				//chart.updateLabelPosition();
  		}
  		return chart;
  	}

  	chart.addLines = function(k, side){
  		var orderedIds, dispIds;
  		if(side == "top" || side == "bottom"){
  			orderedIds = chart.get_heatmapRow("__order__");
  			if(orderedIds == -1)
  				orderedIds = chart.rowIds();
  			dispIds = chart.dispRowIds();
  		}
  		if(side == "left" || side == "right"){
  			orderedIds = chart.get_heatmapCol("__order__");
  			if(orderedIds == -1)
  				orderedIds = chart.colIds();
  			dispIds = chart.dispColIds();
  		}
  		if(k == 0) return dispIds;
  		if(k < 0){
  			k = -k;
  			var pos, ind;
  			if(side == "top" || side == "left"){
  				pos = 0;
  				while(k > 0 && dispIds.length > 1){
  					ind = dispIds.indexOf(orderedIds[pos]);
  					if(ind != -1){
  						k--;
  						dispIds.splice(ind, 1);
  					}
  					pos++;
  				}
  				return dispIds;
  			}
  			else{
  				pos = orderedIds.length - 1;
  				while(k > 0 && dispIds.length > 1){
  					ind = dispIds.indexOf(orderedIds[pos]);
  					if(ind != -1){
  						k--;
  						dispIds.splice(ind, 1);
  					}
  					pos--;
  				}
  				return dispIds;
  			}
  		}

  		var border;
  		if(side == "top" || side == "left"){
  			border = orderedIds.length
  			for(var i = 0; i < dispIds.length; i++)
  				if(border > orderedIds.indexOf(dispIds[i]))
  					border = orderedIds.indexOf(dispIds[i]);
  			for(var i = border - 1; i >= d3.max([0, border - k]); i--)
  				dispIds.unshift(orderedIds[i]);
  		} else { 
  			border = -1;
  			for(var i = 0; i < dispIds.length; i++)
  				if(border < orderedIds.indexOf(dispIds[i]))
  					border = orderedIds.indexOf(dispIds[i]);
  				for(var i = border + 1; i < d3.min([orderedIds.length, border + k + 1]); i++)
  					dispIds.push(orderedIds[i]);
  		}
  		return dispIds;
  	}
  	
  	var inherited_updateSize = chart.updateSize;
  	chart.updateSize = function(){
  		inherited_updateSize();
  		if(typeof chart.transition !== "undefined"){
  			if(!chart.showDendogram("Row"))
  				chart.svg.selectAll(".label_panel.row").transition(chart.transition)
  					.attr("transform", "translate(" + chart.margin().left + ", " +
  						chart.margin().top + ")");
  			if(!chart.showDendogram("Col"))
  				chart.svg.selectAll(".label_panel.col").transition(chart.transition)
  					.attr("transform", "translate(" + chart.margin().left + ", " +
  						chart.margin().top + ")");

  			chart.svg.select(".legend_panel").transition(chart.transition)
  				.attr("transform", "translate(0, " + 
  					(chart.margin().top + chart.plotHeight()) + ")");
  			chart.axes.x_label.transition(chart.transition)
  				.attr("font-size", d3.min([chart.margin().bottom - 2, 15]))
  				.attr("x", chart.plotWidth() + chart.margin().left)
  				.attr("y", chart.height());
  			chart.axes.y_label.transition(chart.transition)
  				.attr("font-size", d3.min([chart.margin().right - 2, 15]))
  				.attr("x", - chart.margin().top)
  				.attr("y", chart.width());
  		} else {
  			if(!chart.showDendogram("Row"))
  				chart.svg.selectAll(".label_panel.row")
  					.attr("transform", "translate(" + chart.margin().left + ", " +
  						chart.margin().top + ")");
  			if(!chart.showDendogram("Col"))
  				chart.svg.selectAll(".label_panel.col")
  					.attr("transform", "translate(" + chart.margin().left + ", " +
  						chart.margin().top + ")");

  			chart.svg.select(".legend_panel")
  				.attr("transform", "translate(0, " + 
  					(chart.get_margin().top + chart.get_plotHeight()) + ")");
  			chart.axes.x_label
  				.attr("font-size", d3.min([chart.get_margin().bottom - 2, 15]))
  				.attr("x", chart.get_plotWidth() + chart.get_margin().left)
  				.attr("y", chart.get_height());
  			chart.axes.y_label
  				.attr("font-size", d3.min([chart.get_margin().right - 2, 15]))
  				.attr("x", - chart.get_margin().top)
  				.attr("y", chart.get_width());
  		}
  		chart.canvas
  			.style("left", chart.margin().left + "px")
  			.style("top", chart.margin().top + "px")
  			.attr("width", chart.plotWidth())
  			.attr("height", chart.plotHeight());		

  		chart.updateLegendSize();
  		chart.updateLabelPosition();
  		return chart;
  	}

  	chart.updateLabelPosition = function(){
  		var ncols = chart.dispColIds().length,
  			nrows = chart.dispRowIds().length;
  		chart.get_heatmapRow("__sort__");
  		chart.get_heatmapCol("__sort__");
  		//calculate cell size
  		chart.cellSize = {
  			width: chart.plotWidth() / ncols,
  			height: chart.plotHeight() / nrows
  		}
  		//create scales
  		chart.axes.scale_x = d3.scaleLinear()
  			.domain( [0, ncols - 1] )
  			.range( [0, chart.plotWidth() - chart.cellSize.width] );
  		chart.axes.scale_y = d3.scaleLinear()
  			.domain( [0, nrows - 1] )
  			.range( [0, chart.plotHeight() - chart.cellSize.height] );

  		if(typeof chart.transition !== "undefined"){
  			chart.svg.select(".col").selectAll(".label").transition(chart.transition)
  				.attr("font-size", d3.min([chart.cellSize.width, 12]))
  				.attr("y", function(d) {return chart.axes.scale_x(chart.get_heatmapCol(d) + 1);});
  			chart.svg.select(".row").selectAll(".label").transition(chart.transition)
  				.attr("font-size", d3.min([chart.cellSize.height, 12]))
  				.attr("y", function(d) {return chart.axes.scale_y(chart.get_heatmapRow(d) + 1);});
  		
  		} else {
  			chart.svg.select(".col").selectAll(".label")
  				.attr("font-size", d3.min([chart.cellSize.width, 12]))
  				.attr("y", function(d) {return chart.axes.scale_x(chart.get_heatmapCol(d) + 1);});
  			chart.svg.select(".row").selectAll(".label")
  				.attr("font-size", d3.min([chart.cellSize.height, 12]))
  				.attr("y", function(d) {return chart.axes.scale_y(chart.get_heatmapRow(d) + 1);});
  		}
  		chart.updateCellPosition();
  		
  		if(chart.showDendogram("Col"))
  			chart.drawDendogram("Col");
  		if(chart.showDendogram("Row"))
  			chart.drawDendogram("Row");
  		
  		return chart;
  	}

  	chart.updateLabels = function(){
  		//add column labels
  		var colLabels = chart.svg.select(".col").selectAll(".label")
  				.data(chart.get_dispColIds(), function(d) {return d;});
  		colLabels.exit()
  			.remove();
  		//add row labels
  		var rowLabels = chart.svg.select(".row").selectAll(".label")
  				.data(chart.get_dispRowIds(), function(d) {return d;});
  		rowLabels.exit()
  			.remove();
  		colLabels.enter()
  			.append("text")
  				.attr("class", "label")
  				.attr("transform", "rotate(-90)")
  				.style("text-anchor", "start")
  				.attr("dx", 2)
  				.merge(colLabels)
  					.attr("id", function(d) {return d.toString().replace(/ /g,"_")})
  					.on("mouseover", chart.labelMouseOver)
  					.on("mouseout", chart.labelMouseOut)
  					.on("click", chart.labelClick);
  		rowLabels.enter()
  			.append("text")
  				.attr("class", "label")
  				.style("text-anchor", "end")
  				.attr("dx", -2)
  				.merge(rowLabels)
  					.attr("id", function(d) {return d.toString().replace(/ /g,"_")})
  					.on("mouseover", chart.labelMouseOver)
  					.on("mouseout", chart.labelMouseOut)
  					.on("click", chart.labelClick);

  		chart.updateCells();
  		return chart;
  	}

  	chart.updateLabelText = function(){
  		if(typeof chart.transition !== "undefined"){
  			chart.svg.select(".col").selectAll(".label").transition(chart.transition)
  				.text(function(d) {return chart.get_colLabels(d);});
  			chart.svg.select(".row").selectAll(".label").transition(chart.transition)
  				.text(function(d) {return chart.get_rowLabels(d)});		
  		} else {
  			chart.svg.select(".col").selectAll(".label")
  				.text(function(d) {return chart.get_colLabels(d);});
  			chart.svg.select(".row").selectAll(".label")
  				.text(function(d) {return chart.get_rowLabels(d)});
  		}
  		return chart;		
  	}

  	chart.zoom = function(lu, rb){
  		var selectedCells = chart.findPoints(lu, rb);
  		if(selectedCells.length < 2)
  			return;
  		var rowIdsAll = [], colIdsAll = [];
  		selectedCells.map(function(e){
  			rowIdsAll.push(e.substr(1).split("_-sep-_")[0]);
  			colIdsAll.push(e.substr(1).split("_-sep-_")[1]);
  		});
  		var rowIds = [], colIds = [];

  		for(var i = 0; i < rowIdsAll.length; i++)
  			if(rowIds.indexOf(rowIdsAll[i]) == -1)
  				rowIds.push(rowIdsAll[i]);
  		for(var i = 0; i < colIdsAll.length; i++)
  			if(colIds.indexOf(colIdsAll[i]) == -1)
  				colIds.push(colIdsAll[i]);
  		if(rowIds.length > 0 )
  		chart.dispRowIds(rowIds);
  		chart.dispColIds(colIds);
  		//chart.clusterRowIds(rowIds)
  		//chart.clusterColIds(colIds)
  		chart.updateLabels();
  		chart.updateLabelPosition();
  		//chart.cluster('Row')
  		//	 .cluster('Col');
  		//if(chart.dendogramRow) chart.drawDendogram("Row");
  		//if(chart.dendogramCol) chart.drawDendogram("Col");
  		return chart;
  	}

  	chart.resetDomain = function(){
  		chart.dispColIds(chart.get_colIds());
  		chart.dispRowIds(chart.get_rowIds());
  		chart.updateStarted = true;
  		chart.updateLabels()
  			.updateLabelPosition()
  			.updateCellColour()
  			.updateLabelText();
  		chart.updateStarted = false;
  		//chart.clusterColIds(chart.get_colIds());
  		//chart.clusterRowIds(chart.get_rowIds());
  		//chart.cluster('Row');
  		//chart.cluster('Col');
  		//if(chart.dendogramRow) chart.drawDendogram("Row");
  		//if(chart.dendogramCol) chart.drawDendogram("Col");
  		return chart;
  	}

  	chart.resetColourScale = function(){
  	//create colorScale
  		var range = chart.get_colourRange();
  		chart.colourScale = d3.scaleSequential(chart.get_palette).domain(range);
  		if(chart.get_showLegend())
  			chart.updateLegend();		
  	}	

  	//some default onmouseover and onmouseout behaviour for cells and labels
  	//may be later moved out of the main library
  	chart.pointMouseOver = function(d) {
  		var pos = d3.mouse(chart.container.node());
  		//change colour and class
  		d3.select(this)
  			.attr("fill", function(d) {
  				return d3.rgb(chart.get_colour(chart.get_value(d[0], d[1]))).darker(0.5);
  			})
  			.classed("hover", true);		
  		//find column and row labels
  		chart.svg.select(".col").selectAll(".label")
  			.filter(function(dl) {return dl == d[1];})
  				.classed("hover", true);
  		chart.svg.select(".row").selectAll(".label")
  			.filter(function(dl) {return dl == d[0];})
  				.classed("hover", true);
  		//show label
  		if(chart.get_showValue()){
  			chart.g.selectAll(".tval").filter(function(fd){
  				return fd[0] == d[0] && fd[1] == d[1];
  			})
  			.classed("hidden", false);
  		} else {
  		chart.container.select(".inform")
  			.style("left", (pos[0] + 10) + "px")
  			.style("top", (pos[1] + 10) + "px")
  			.select(".value")
  				.html(function() {return chart.get_informText(d[0], d[1])});  
  		chart.container.select(".inform")
  			.classed("hidden", false);
  		}
  	};
  	chart.pointMouseOut = function(d) {
  		//change colour and class
  		d3.select(this)
  			.attr("fill", function(d) {
  				return chart.get_colour(chart.get_value(d[0], d[1]));
  			})
  			.classed("hover", false);
  		//deselect row and column labels
  		chart.svg.selectAll(".label")
  			.classed("hover", false);
  		if(chart.get_showValue()){
  			chart.g.selectAll(".tval").classed("hidden", true);
  		} else {
  			chart.container.select(".inform")
  				.classed("hidden", true);
  		}
  	};
  	
  	//set default clicking behaviour for labels (ordering)
  	chart.labelClick = function(d){
  		//check whether row or col label has been clicked
  		var type;
  		d3.select(this.parentNode).classed("row") ? type = "row" : type = "col";
  		//if this label is already selected, flip the heatmap
  		if(d3.select(this).classed("sorted")){
  			type == "col" ? chart.reorder("Row", "flip") : chart.reorder("Col", "flip");
  		} else {
  			//select new label and chage ordering
  			if(type == "col"){
  				chart.reorder("Row", function(a, b){
  					return chart.get_value(b, d) - chart.get_value(a, d);
  				});
  				if(chart.dendogramRow)
  					chart.dendogramRow.remove();
  			} else {
  				chart.reorder("Col", function(a, b){
  					return chart.get_value(d, b) - chart.get_value(d, a);
  				});
  				if(chart.dendogramCol)
  					chart.dendogramCol.remove();
  			}
  			chart.updateLabelPosition();
  		}
  		d3.select(this).classed("sorted", true);
  		chart.svg.selectAll(".sorted").classed("selected", true);
  	};
  	
  	chart.updateCellColour = function() {
  		if(!chart.checkMode())
  			return chart;

  		if(get_mode() == "svg") {
  			if(typeof chart.transition !== "undefined")
  				chart.g.selectAll(".data_point").transition(chart.transition)
  					.attr("fill", function(d) {
  						return chart.get_colour(chart.get_value(d[0], d[1]));
  				})
  			else
  				chart.g.selectAll(".data_point")
  					.attr("fill", function(d) {
  						return chart.get_colour(chart.get_value(d[0], d[1]));
  				});
  			chart.svg.selectAll(".sorted")
  				.classed("selected", false)
  				.classed("sorted", false);

  			if(chart.get_showValue())
  				chart.updateTextValues();
  		} else {
  			if(!chart.updateStarted)
  				chart.updateCanvas();
  		}
  		
  		return chart;
  	}

  	chart.updateCells = function(){
  		if(!chart.checkMode())
  			return chart;

  		var markedCells = chart.get_marked().length;

  		if(get_mode() == "svg") {
  			//add rows
  			var rows = chart.g.selectAll(".data_row")
  				.data(chart.get_dispRowIds(), function(d) {return d;});
  			rows.exit()
  				.remove();
  			rows.enter()
  				.append("g")
  					.attr("class", "data_row");

  			//add cells	
  			var cells = chart.g.selectAll(".data_row").selectAll(".data_point")
  				.data(function(d) {
  					return chart.get_dispColIds().map(function(e){
  						return [d, e];
  					})
  				}, function(d) {return d;});
  			cells.exit()
  				.remove();
  			cells.enter()
  				.append("rect")
  					.attr("class", "data_point")
  					.attr("opacity", 0.5)
  					.merge(cells)
  						.attr("id", function(d) {return "p" + (d[0] + "_-sep-_" + d[1]).replace(/ /g,"_")})
  						.attr("rowId", function(d) {return d[0];})
  						.attr("colId", function(d) {return d[1];})
  						.on("mouseover", chart.pointMouseOver)
  						.on("mouseout", chart.pointMouseOut)
  						.on("click", function(d) {
  							chart.get_on_click.apply(this, [d[0], d[1]]);
  						});
  			if(chart.get_showValue())
  				chart.updateTexts();
  		} else {
  			var dispRowIds = chart.dispRowIds(),
  				dispColIds = chart.dispColIds(),
  				i = 0;

  			while(i < chart.marked.length)
  				if(dispRowIds.indexOf(chart.marked[i][0]) == -1 || dispColIds.indexOf(chart.marked[i][1]) == -1)
  					chart.marked.splice(i, 1);
  				else
  					i++;

  			if(!chart.updateStarted)
  				chart.updateCanvas();
  		}

  		var newMarked = chart.get_marked().length;

  		if(markedCells > newMarked)
  			chart.markedUpdated();
  		if(newMarked == 0)
  			chart.g.selectAll(".data_point")
  				.attr("opacity", 1);

  		
  		return chart;
  	}

  	chart.updateCellPosition = function(){
  		if(!chart.checkMode())
  			return chart;

  		if(get_mode() == "svg"){
  			if(typeof chart.transition !== "undefined")
  				chart.g.selectAll(".data_point").transition(chart.transition)
  					.attr("x", function(d){
  						return chart.axes.scale_x(chart.get_heatmapCol(d[1]));
  					})
  					.attr("width", chart.cellSize.width)
  					.attr("height", chart.cellSize.height)								
  					.attr("y", function(d) {
  						return chart.axes.scale_y(chart.get_heatmapRow(d[0]))
  					})
  			else
  				chart.g.selectAll(".data_point")
  					.attr("x", function(d){
  						return chart.axes.scale_x(chart.get_heatmapCol(d[1]));
  					})
  					.attr("width", chart.cellSize.width)
  					.attr("height", chart.cellSize.height)								
  					.attr("y", function(d) {
  						return chart.axes.scale_y(chart.get_heatmapRow(d[0]))
  					});
  			if(chart.get_showValue())
  				chart.updateTextPosition();
  		} else {
  			chart.updateCanvas();
  		}

  		return chart;
  	}

  	chart.cluster = function(type, features){
  		console.log(type);
  		if(type != "Row" && type != "Col")
  			throw "Error in 'cluster': type " + type + " cannot be recognised. " +
  					"Please, use either 'Row' or 'Col'";
  		
  		if(chart["dendogram" + type] === undefined){
  			chart["dendogram" + type] = dendogram(chart);
  			type == "Row" ? chart["dendogram" + type].orientation("v") : chart["dendogram" + type].orientation("h"); 
  		};

  		chart["dendogram" + type]
  			.dataIds(function() {
  				return chart["disp" + type + "Ids"]();
  			});
  		if(features === undefined)
  			type == "Row" ? features = chart.dispColIds() :
  											features = chart.dispRowIds();
  		//console.log(features);

  		if(type == "Row")
  			chart["dendogram" + type]
  				.data(function(id) {
  					return features.map(function(e) {return chart.get_value(id, e)});
  				})
  		else
  			chart["dendogram" + type]
  				.data(function(id) {
  					return features.map(function(e) {return chart.get_value(e, id)});
  				});

  		chart["dendogram" + type]
  			.distance(function(a, b){
  				return chart["get_cluster" + type + "Metric"](a, b);
  			})
  			.cluster();

  		var newOrder = chart["dendogram" + type].clusters.val_inds.map(function(e) {return e.toString()});

  		chart.reorder(type, function(a, b){
  			return newOrder.indexOf(a) - newOrder.indexOf(b);
  		});

  		//if(chart.g)
  		//	chart.updateLabelPosition();	

  		return chart;
  	}

  	chart.drawDendogram = function(type){
  		//if rows (or columns) are not clustered and 
  		//thus no dendogram is defined, do nothing
  		if(chart["dendogram" + type] === undefined)
  			return chart;
  		if(chart["dendogram" + type].g === undefined)
  			chart["dendogram" + type].place()
  		else
  			chart["dendogram" + type].g.selectAll("line")
  				.remove();
  			chart["dendogram" + type].draw();
  		return chart;
  	}

  	chart.updateTexts = function(){
  		//add rows
  		var rows = chart.g.selectAll(".text_row")
  			.data(chart.get_dispRowIds(), function(d) {return d;});
  		rows.exit()
  			.remove();
  		rows.enter()
  			.append("g")
  				.attr("class", "text_row");

  		//add text	
  		var text = chart.g.selectAll(".text_row").selectAll(".tval")
  			.data(function(d) {
  				return chart.get_dispColIds().map(function(e){
  					return [d, e];
  				})
  			}, function(d) {return d;});
  		text.exit()
  			.remove();
  		text.enter()
  			.append("text")
  				.attr("class", "tval hidden");
  		return chart;		
  	}
  	chart.updateTextPosition = function(){
  		if(typeof chart.transition !== "undefined")
  			chart.g.selectAll(".tval").transition(chart.transition)
  				.attr("x", function(d){
  					return chart.axes.scale_x(chart.get_heatmapCol(d[1]));
  				})
  				.attr("font-size", chart.cellSize.height * 0.5)								
  				.attr("y", function(d) {
  					return chart.axes.scale_y(chart.get_heatmapRow(d[0]) ) + chart.cellSize.height * 0.75
  				})
  		else
  			chart.g.selectAll(".tval")
  				.attr("x", function(d){
  					return chart.axes.scale_x(chart.get_heatmapCol(d[1]));
  				})
  				.attr("font-size", chart.cellSize.height * 0.5)								
  				.attr("y", function(d) {
  					return chart.axes.scale_y(chart.get_heatmapRow(d[0])) + chart.cellSize.height * 0.75;
  				})
  		return chart;
  	}
  	chart.updateTextValues = function(){
  		if(typeof chart.transition !== "undefined")
  			chart.g.selectAll(".tval").transition(chart.transition)
  				.text(function(d) {
  					return chart.get_value(d[0], d[1]).toFixed(1);
  			})
  		else
  			chart.g.selectAll(".tval")
  				.text(function(d) {
  					return chart.get_value(d[0], d[1]).toFixed(1);
  			});
  		return chart;
  	}

  	chart.updateLegendSize = function(){
  		//calculate the size of element of legend
  		var height = d3.min([chart.get_margin().bottom * 0.5, 20]),
  			width = d3.min([chart.get_width()/23, 30]),
  			fontSize = d3.min([chart.get_margin().bottom * 0.3, width / 2, 15]),
  			blocks = chart.svg.select(".legend_panel").selectAll(".legend_block")
  			.attr("transform", function(d) {
  				return "translate(" + (d + 1) * width + ", 0)";
  			});
  		blocks.selectAll("text")
  			.attr("font-size", fontSize)
  			.attr("dy", chart.get_margin().bottom * 0.4)
  			.attr("dx", width);
  		blocks.selectAll("rect")
  			.attr("height", height)
  			.attr("width", width)
  			.attr("y", chart.get_margin().bottom * 0.5);
  	}

  	chart.updateLegend = function(){
  		var range = chart.get_colourRange(),
  			step = (range[1] - range[0]) / 20,
  			blocks = chart.svg.select(".legend_panel")
  			.selectAll(".legend_block").data(d3.range(21))
  				.enter().append("g")
  					.attr("class", "legend_block");
  		blocks.append("text")
  			.attr("text-anchor", "end");
  		blocks.append("rect");
  		chart.svg.select(".legend_panel")
  			.selectAll(".legend_block").selectAll("text")
  				.text(function(d) {
  					if(d % 2 == 0)
  						return (range[0] + step * d).toFixed(2)
  					else
  						return "";
  				});
  		chart.svg.select(".legend_panel")
  			.selectAll(".legend_block").selectAll("rect")
  				.attr("fill", function(d) {return chart.colourScale(range[0] + step * d)});
  	}

  	chart.checkMode = function(){
  		if((get_mode() == "svg") && (chart.canvas.classed("active"))) {
  			chart.canvas.classed("active", false);
  			chart.g.classed("active", true);
  			chart.canvas.node().getContext("2d")
  				.clearRect(0, 0, chart.plotWidth(), chart.plotHeight());

  			if(chart.updateStarted)
  				return true;
  			else{			
  				chart.updateStarted = true;
  				chart.updateLabels()
  					.updateLabelText()
  					.updateCellColour();
  				chart.updateStarted = false;
  				chart.mark(chart.marked.map(function(e) {return "p" + e.join("_-sep-_")}));
  				chart.marked = [];
  				return false;
  			}
  		}
  		if((get_mode() == "canvas") && chart.g.classed("active")){
  			chart.canvas.classed("active", true);
  			chart.marked = chart.g.selectAll(".marked").data();
  			chart.g.classed("active", false);
  			while (chart.g.node().firstChild) 
      		chart.g.node().removeChild(chart.g.node().firstChild);
  		}
  		return true;
  	}

  	chart.updateCanvas = function(){
  		console.log("Canvas");
  		var ctx = chart.canvas.node().getContext("2d");
  		ctx.clearRect(0, 0, chart.plotWidth(), chart.plotHeight());
  		var rowIds = chart.dispRowIds(),
  			colIds = chart.dispColIds(),
  			ncols = colIds.length, nrows = rowIds.length;
  		var pixelHeatmap = document.createElement("canvas");
  		pixelHeatmap.width = ncols;
  		pixelHeatmap.height = nrows;
  		
  		//store colour of each cell
  		var rgbColour, position;
  		//create an object to store information on each cell of a heatmap
  		var pixelData = new ImageData(ncols, nrows);

  		for(var i = 0; i < nrows; i++)
  			for(var j = 0; j < ncols; j++) {
  					rgbColour = d3.rgb(chart.get_colour(chart.get_value(rowIds[i], 
  																													colIds[j])));
  					position = chart.get_heatmapRow(rowIds[i]) * ncols * 4 +
  						chart.get_heatmapCol(colIds[j]) * 4;
  					pixelData.data[position] = rgbColour.r;
  					pixelData.data[position + 1] = rgbColour.g;
  					pixelData.data[position + 2] = rgbColour.b;
  			}
  		//set opacity of pixels
  		if(chart.marked.length == 0)
  			for(var i = 0; i < ncols * nrows; i++)
  				pixelData.data[i * 4 + 3] = 255
  		else
  			for(var i = 0; i < ncols * nrows; i++)
  				pixelData.data[i * 4 + 3] = 75;
  		for(var i = 0; i < chart.marked.length; i++){
  			position = chart.get_heatmapRow(chart.marked[i][0]) * ncols * 4 +
  						chart.get_heatmapCol(chart.marked[i][1]) * 4;			
  			pixelData.data[position + 3] = 255;
  		}
  		
  		//put a small heatmap on screen and then rescale it
  		pixelHeatmap.getContext("2d").putImageData(pixelData, 0 , 0);

  		ctx.imageSmoothingEnabled = false;
  		//probaly no longer required, but let it stay here just in case
      //heatmapBody.mozImageSmoothingEnabled = false;
  		//heatmapBody.webkitImageSmoothingEnabled = false;
      //heatmapBody.msImageSmoothingEnabled = false;

  		ctx.drawImage(pixelHeatmap, 0, 0, 
  			ncols, nrows,
  			0, 0,	chart.plotWidth(), chart.plotHeight());

  	}

  	chart.getPoints = function(data){
  		if(data.length == 2 && data[0].substr)
  			data = [data];
  		data = data.map(function(e) {return "p" + e.join("_-sep-_")});

  		if(get_mode() == "svg") 
  			return (data.length > 0) ?
  				chart.svg.selectAll("#" + lc.escapeRegExp(data.join(", #").replace(/ /g, "_"))) :
  				chart.svg.selectAll("______");
  		else
  			return data;
  	}

  	var inherited_get_marked = chart.get_marked;
  	chart.get_marked = function(){
  		if(get_mode() == "svg")
  			return inherited_get_marked()
  		else
  			return chart.marked;
  	}

  	var inherited_mark = chart.mark;
  	chart.mark = function(marked){
  		if(get_mode() == "svg")
  			inherited_mark(marked)
  		else {
  			if(marked == "__clear__")
  				chart.marked = []
  			else {
  				if(marked.length && marked[0].substr)
  					marked = marked.map(function(e) {return e.substr(1).split("_-sep-_")});
  				var ids = chart.marked.map(function(e) {return e.join("_")}),
  					ind;
  				for(var i = 0; i < marked.length; i++){
  					ind = ids.indexOf(marked[i].join("_"));
  					if(ind == -1)
  						chart.marked.push(marked[i])
  					else {
  						chart.marked.splice(ind, 1);
  						ids.splice(ind, 1);
  					}
  				}
  			}
  		}

  		if(get_mode() == "canvas")
  			chart.updateCanvas();
  		chart.markedUpdated();
  		return chart;
  	}	
  	
  	chart.pan.move = function(p) {
  		var move = [p[0] - chart.pan.down[0], p[1] - chart.pan.down[1]],
  			addRows = Math.floor(Math.abs(move[1] / chart.cellSize.height)),
  			addCols = Math.floor(Math.abs(move[0] / chart.cellSize.width));
  		chart.pan.down[0] += Math.sign(move[0]) * addCols * chart.cellSize.width;
  		chart.pan.down[1] += Math.sign(move[1]) * addRows * chart.cellSize.height;

  		chart.dispColIds(chart.addLines(-Math.sign(move[0]) * addCols, "right"));
  		chart.dispColIds(chart.addLines(Math.sign(move[0]) * addCols, "left"));
  		chart.dispRowIds(chart.addLines(Math.sign(move[1]) * addRows, "top"));
  		chart.dispRowIds(chart.addLines(-Math.sign(move[1]) * addRows, "bottom"));

  		if(Math.abs(addRows) + Math.abs(addCols) > 0) {
  			chart.updateStarted = true;
  			chart.updateLabels();
  			chart.updateLabelPosition();
  			chart.updateCellColour();
  			chart.updateLabelText();
  			chart.updateStarted = false;
  		}			
  	}

  	chart.update = function() {
  		chart.updateTitle();
  		chart.resetColourScale();
  		chart.dispColIds(function() {return chart.colIds();});
  		chart.dispRowIds(function() {return chart.rowIds();});
  		chart.axes.x_label
  			.text(chart.get_colTitle());
  		chart.axes.y_label
  			.text(chart.get_rowTitle());
  		chart.updateStarted = true;
  		chart.updateLabels()
  			.updateSize()
  			.updateLabelText()
  			.updateCellColour();
  		chart.updateStarted = false;

  		return chart;
  	}

  	return chart;	
  }

  function sigmoid( x, midpoint, slope ) {
    return 1 / ( 1 + Math.exp( -slope * ( x - midpoint ) ) )
  }

  function make_stretched_sigmoid( midpoint, slope, xl, xr ) {
    var yl = sigmoid( xl, midpoint, slope, 0, 1 )
    var yr = sigmoid( xr, midpoint, slope, 0, 1 )
    var ym = Math.min( yl, yr )
    return function(x) { return ( sigmoid( x, midpoint, slope, 1 ) - ym ) / Math.abs( yr - yl ) }
  }

  function sigmoidColorSlider() {

    // for now only horizontal

    var obj = chartBase()
      .add_property( "straightColorScale" )
      .add_property( "midpoint", undefined )
      .add_property( "slopewidth", undefined )
      .add_property( "on_drag", function() {})
  		.add_property( "on_change", function() {})
      .margin( { top: 20, right: 10, bottom: 5, left: 10 } )
      .height( 80 )
      .transitionDuration( 0 );    

    obj.showPanel(false);
    
    obj.straightColorScale(
      d3.scaleLinear()
        .range( [ "white", "darkblue" ] ) );

    obj.clamp_markers = function() {
      var min = d3.min( obj.get_straightColorScale.domain() );
      var max = d3.max( obj.get_straightColorScale.domain() );
      if( obj.get_midpoint() < min )
         obj.midpoint( min );
      if( obj.get_midpoint() > max )
         obj.midpoint( max );
      if( obj.get_slopewidth() > (max-min) )
         obj.slopewidth( max-min );
      if( obj.get_slopewidth() < (min-max) )
         obj.slopewidth( min-max );
    }
  	
    var inherited_put_static_content = obj.put_static_content;
    obj.put_static_content = function( element ) {
      inherited_put_static_content( element );


      var g = obj.svg.append( "g" )
        .attr( "class", "sigmoidColorSlider" )
        .attr( "transform", "translate(" + obj.get_margin().left + ", " + 
  																	obj.get_margin().top + ")" );  // space for axis

      obj.axis = g.append( "g" )
        .attr( "class", "axis" );

      var defs = g.append( "defs" );

      obj.gradient = defs.append( "linearGradient" )
        .attr( "id", "scaleGradient" + Math.random().toString(36).substring(2, 6))
        .attr( "x1", "0%")
        .attr( "y1", "0%")
        .attr( "x2", "100%")
        .attr( "y2", "0%");

      obj.gradient.selectAll( "stop" )
        .data( d3.range(100) )
        .enter().append( "stop" )
          .attr( "offset", function(d) { return d + "%" } )

      var gradId = obj.gradient.attr("id");

      obj.colorBar = g.append( "rect" )
        .attr( "x", "0" )
        .attr( "y", "5" )
        .attr( "height", 20 )
        .attr( "fill", "url(#" + gradId +")" )
        .style( "stroke", "black" )
        .style( "stroke-width", "1");

      defs.append( "path" )
           .attr( "id", "mainMarker" )
           .attr( "d", "M 0 0 L 8 5 L 8 25 L -8 25 L -8 5 Z")
           .style( "fill", "gray" )
           .style( "stroke", "black" )

      defs.append( "path" )
           .attr( "id", "rightMarker" )
           .attr( "d", "M 0 0 L 5 5 L 5 15 L 0 15 Z")
           .style( "fill", "lightgray" )
           .style( "stroke", "black" )

      defs.append( "path" )
           .attr( "id", "leftMarker" )
           .attr( "d", "M 0 0 L -5 5 L -5 15 L 0 15 Z")
           .style( "fill", "lightgray" )
           .style( "stroke", "black" )

      obj.mainMarker = g.append( "use" )
        .attr( "xlink:href", "#mainMarker")
        .attr( "y", 28 )
        .call( d3.drag()
          .on( "drag", function() {
            obj.midpoint( obj.pos_scale.invert( obj.pos_scale( obj.get_midpoint() ) + d3.event.dx ) );
            obj.clamp_markers();
            obj.get_on_drag();
            obj.update();
          } )
          .on("end", function() {
  					obj.get_on_change();
  				})
  			);

      obj.rightMarker = g.append( "use" )
        .attr( "xlink:href", "#rightMarker")
        .attr( "y", 30 )
        .call( d3.drag()
          .on( "drag", function() {
            obj.slopewidth( obj.pos_scale.invert( obj.pos_scale( obj.get_slopewidth() ) + d3.event.dx ) );
            obj.clamp_markers();
            obj.update();        
            obj.get_on_drag();
          } )
  				.on("end", function() {
  					obj.get_on_change();
  				})
  			);

      obj.leftMarker = g.append( "use" )
        .attr( "xlink:href", "#leftMarker")
        .attr( "y", 30 )
        .call( d3.drag()
          .on( "drag", function() {
            obj.slopewidth( obj.pos_scale.invert( obj.pos_scale( obj.get_slopewidth() ) - d3.event.dx ) );
            obj.clamp_markers();
            obj.update();        
            obj.get_on_drag();
          } )
  			  .on("end", function() {
  				  obj.get_on_change();
  			  })
  		  );

    }
  	
    var inherited_update = obj.update;
    obj.update = function() {
      inherited_update();
  		
      var percent_scale = d3.scaleLinear()
        .domain( [0, 100] )
        .range( obj.get_straightColorScale.domain() );

      if( obj.get_midpoint() == undefined )
        obj.midpoint( percent_scale( 50 ) );

      if( obj.get_slopewidth() == undefined )
        obj.slopewidth( Math.abs(percent_scale( 15 )) );

      obj.pos_scale = d3.scaleLinear()
        .range( [ 0, obj.get_plotWidth() ] )
        .domain( obj.get_straightColorScale.domain() )

      d3.axisTop()
        .scale( obj.pos_scale )
        ( obj.axis );

      obj.colorBar
        .attr( "width", obj.get_plotWidth() );

      //obj.the_sigmoid = function(x) { return sigmoid( x, obj.get_midpoint(), 1.38 / obj.get_slopewidth(), 0, 1 ) };
      obj.the_sigmoid = make_stretched_sigmoid( obj.get_midpoint(), 1.38 / obj.get_slopewidth(), 
        obj.get_straightColorScale.domain()[0], obj.get_straightColorScale.domain()[1] );

      obj.gradient.selectAll( "stop" )
        .data( d3.range(100) )
        .style( "stop-color", function(d) { 
          return obj.get_straightColorScale( 
            percent_scale( 100 * obj.the_sigmoid( percent_scale(d) ) ) ) } ) ;

      obj.colourScale = function(val){
        return obj.get_straightColorScale( 
            percent_scale( 100 * obj.the_sigmoid( val ) ) );
      }


      obj.mainMarker
        .attr( "x", obj.pos_scale( obj.get_midpoint() ) );
      obj.rightMarker
        .attr( "x", obj.pos_scale( obj.get_midpoint() + obj.get_slopewidth() ) )
      obj.leftMarker
        .attr( "x", obj.pos_scale( obj.get_midpoint() - obj.get_slopewidth() ) )

  		//obj.get_on_change();

    }

    return obj;

  }

  function simpleTable() {

    var chart = chartBase()
      .add_property( "record", {} )

    var inherited_put_static_content = chart.put_static_content;
    chart.put_static_content = function( element ) {
      inherited_put_static_content(element);
      chart.table = chart.container.append( "table" )
        .attr( "border", 1 );
    }

    var inherited_update = chart.update;
    chart.update = function( ) {

      inherited_update();
      var sel = chart.table.selectAll( "tr" )
        .data( Object.keys( obj.get_record() ) );
      sel.exit()
        .remove();  
      sel.enter().append( "tr" )
      .merge( sel )
        .html( function(k) { return "<td>" + k + "</td><td>" 
           + chart.get_record()[k] + "</td>" } )

      return chart;
    };

    return chart;
  }

  function barChart(id, chart){
  	
  	if(chart === undefined)
  		chart = axisChart();
  	if(id === undefined)
  		id = "layer" + chart.get_nlayers();
  	
  	var layer = chart.create_layer(id).get_layer(id)
  		.add_property("ngroups")
  		.add_property("groupIds")
  		.add_property("nbars")
  		.add_property("barIds")
  		.add_property("nstacks")
  		.add_property("stackIds")
  		.add_property("value")
  		.add_property("groupWidth", 0.6)
  		.add_property("stroke", "#444")
  		.add_property("strokeWidth", 0)
  		.add_property("informText", function(groupId, barId, stackId){
  			var id = groupId;
  			if(layer.nbars() > 1) id += ", " + barId;
  			if(layer.nstacks() > 1) id += ", " + stackId;
  			return "ID: <b>" + id + "</b>;<br>" + 
              "value = " + layer.get_value(groupId, barId, stackId).toFixed(2)
  		});
  	chart.syncProperties(layer);

  	layer.type = "barChart";

  	layer.ngroups("_override_", "groupIds", function(){
  		return d3.range(layer.ngroups()).map(function(e) {return e.toString()});
  	});
  	layer.groupIds("_override_", "ngroups", function(){
  		return layer.groupIds().length;
  	});
  	layer.nbars("_override_", "barIds", function(){
  		return d3.range(layer.nbars()).map(function(e) {return e.toString()});
  	});
  	layer.barIds("_override_", "nbars", function(){
  		return layer.barIds().length;
  	});
  	layer.nstacks("_override_", "stackIds", function(){
  		return d3.range(layer.nbars()).map(function(e) {return e.toString()});
  	});
  	layer.stackIds("_override_", "nstacks", function(){
  		return layer.stackIds().length;
  	});

  	layer.nbars(1);
  	layer.nstacks(1);
  	layer.contScaleX(false);
  	layer.dataIds(function(){
  		if(layer.nbars() == 1)
  			return layer.stackIds();
  		var ids = [], barIds = layer.barIds(), stackIds = layer.stackIds();
  		for(var i = 0; i < layer.nbars(); i++)
  			for(var j = 0; j < layer.nstacks(); j++)
  				ids.push(barIds[i] + ", " + stackIds[j]);
  		return ids;
  	});
  	layer.colourValue(function(id) {return id;});
  	layer.colour(function(gropuId, barId, stackId) {
        if(layer.nbars() == 1)
        	return layer.colourScale(layer.get_colourValue(stackId))
        else
        	return layer.colourScale(layer.get_colourValue(barId + ", " + stackId));
      })

  	layer.layerDomainX(function() {
  		return layer.groupIds();
  	});
  	layer.layerDomainY(function(){
  		//go through all bars and find the highest
  		var barIds = layer.barIds(),
  			groupIds = layer.groupIds(),
  			stackIds = layer.stackIds(),
  			maxHeight = 0, curHeihgt;
  		for(var i = 0; i < layer.ngroups(); i++)
  			for(var j = 0; j < layer.nbars(); j++){
  				curHeihgt = 0;
  				for(var k = 0; k < layer.nstacks(); k++)
  					curHeihgt += layer.get_value(groupIds[i], barIds[j], stackIds[k]);
  				if(curHeihgt > maxHeight) maxHeight = curHeihgt;
  			}

  		return [0, maxHeight];
  	});

    //default hovering behaviour
    layer.pointMouseOver(function(d){
      var pos = d3.mouse(chart.container.node());
      //change colour and class
      d3.select(this)
        .attr("fill", function(d) {
          return d3.rgb(layer.get_colour(d[0], d[1], d[2])).darker(0.5);
        })
        .classed("hover", true);
      //show label
      layer.chart.container.select(".inform")
          .style("left", (pos[0] + 10) + "px")
          .style("top", (pos[1] + 10) + "px")
          .select(".value")
            .html(layer.get_informText(d[0], d[1], d[2]));  
      layer.chart.container.select(".inform")
        .classed("hidden", false);
    });
    layer.pointMouseOut(function(d){
      d3.select(this)
        .attr("fill", function(d) {
          return layer.get_colour(d[0], d[1], d[2]);
        })
        .classed("hover", false);
      layer.chart.container.select(".inform")
        .classed("hidden", true);
    });


  	layer.findPoints = function(lu, rb){
  		return layer.g.selectAll(".data_point")
  			.filter(function(){
  				var x = +d3.select(this).attr("x"),
  					y = +d3.select(this).attr("y"),
  					width = +d3.select(this).attr("width"),
  					height = +d3.select(this).attr("height");

  				return (lu[0] <= x + width && rb[0] > x && 
  								lu[1] <= y + height && rb[1] > y)
  			}).nodes().map(function(e) {return e.getAttribute("id")});
  	}
  	layer.get_position = function(id){
  		//gets id as data (so here we have an array of three ids)
  		return [layer.g.select("#p" + id.join("_-sep-_")).attr("x"),
  						layer.g.select("#p" + id.join("_-sep-_")).attr("y")];
  	}

  	layer.updatePointLocation = function(){
  		var groupWidth = layer.chart.axes.scale_x.step() * layer.groupWidth(),
  			barWidth = groupWidth/layer.nbars(),
  			//for now it's just a linear scale
  			heightMult = Math.abs(layer.chart.axes.scale_y(1) - layer.chart.axes.scale_y(0)),
  			groupScale = d3.scaleLinear()
  				.domain([0, layer.nbars() - 1])
  				.range([-groupWidth/2, groupWidth/2 - barWidth]),
  			barIds = layer.barIds(),
  			stackIds = layer.stackIds();
  		if(typeof layer.chart.transition !== "undefined"){
  			layer.g.selectAll(".data_point").transition(layer.chart.transition)
  				.attr("width", barWidth)
  				.attr("height", function(d){ 
  					return layer.get_value(d[0], d[1], d[2]) * heightMult;
  				})
  				.attr("x", function(d){
  					if(layer.chart.axes.scale_x(d[0]) == undefined)
  						return -500;
  					return groupScale(barIds.indexOf(d[1])) + 
  						layer.chart.axes.scale_x(d[0]);
  				})
  				.attr("y", function(d){
  					var height = 0;
  					for(var i = 0; i <= stackIds.indexOf(d[2]); i++)
  						height += layer.get_value(d[0], d[1], stackIds[i]);
  					return layer.chart.axes.scale_y(height);
  				})
  		}	else {
  			layer.g.selectAll(".data_point")
  				.attr("width", barWidth)
  				.attr("height", function(d){ 
  					return layer.get_value(d[0], d[1], d[2]) * heightMult;
  				})
  				.attr("x", function(d){
  					if(layer.chart.axes.scale_x(d[0]) == undefined)
  						return -500;
  					return groupScale(barIds.indexOf(d[1])) + 
  						layer.chart.axes.scale_x(d[0]);
  				})
  				.attr("y", function(d){
  					var height = 0;
  					for(var i = 0; i <= stackIds.indexOf(d[2]); i++)
  						height += layer.get_value(d[0], d[1], stackIds[i]);
  					return layer.chart.axes.scale_y(height);
  				});
  		}

  		return layer;			
  	}
  	layer.updatePointStyle = function(){
  		layer.resetColourScale();

  		if(typeof layer.chart.transition !== "undefined")
  			layer.g.selectAll(".data_point").transition(chart.transition)
  				.attr("fill", function(d) {
  					return layer.get_colour(d[0], d[1], d[2]);
  				})
  				.attr("stroke", function(d) {
  					return layer.get_stroke(d[0], d[1], d[2]);
  				})
  				.attr("stroke-width", function(d) {
  					return layer.get_strokeWidth(d[0], d[1], d[2]);
  				})
  		else
  			layer.g.selectAll(".data_point")
  				.attr("fill", function(d) {
  					return layer.get_colour(d[0], d[1], d[2]);
  				})
  				.attr("stroke", function(d) {
  					return layer.get_stroke(d[0], d[1], d[2]);
  				})
  				.attr("stroke-width", function(d) {
  					return layer.get_strokeWidth(d[0], d[1], d[2]);
  				});
  	}

  	layer.updatePoints = function(){
  		
  		var groups = layer.g.selectAll(".group")
  			.data(layer.groupIds(), function(d) {return d;});
  		groups.exit()
  			.remove();
  		groups.enter()
  			.append("g")
  				.attr("class", "group");

  		var bars = layer.g.selectAll(".group").selectAll(".bar")
  			.data(function(d) {
  				return layer.barIds().map(function(e){
  					return [d, e];
  				})
  			}, function(d) {return d;});
  		bars.exit()
  			.remove();
  		bars.enter()
  			.append("g")
  				.attr("class", "bar");

  		var stacks = layer.g.selectAll(".group").selectAll(".bar").selectAll(".data_point")
  			.data(function(d){
  				return layer.stackIds().map(function(e){
  					return d.concat(e);
  				})
  			}, function(d) {return d;});
  		stacks.exit()
  			.remove();
  		stacks.enter()
  			.append("rect")
  				.attr("class", "data_point")
  				.merge(stacks)
  					.attr("id", function(d) {return "p" + d.join("_-sep-_").replace(/ /g, "_")})
  					.on( "click", function(d) {layer.get_on_click(d[0], d[1], d[2])} )
          	.on( "mouseover", layer.get_pointMouseOver )
          	.on( "mouseout", layer.get_pointMouseOut );		
  	}

  	//add legend


  	return layer;
  }

  exports.base = base;
  exports.layerBase = layerBase;
  exports.chartBase = chartBase;
  exports.axisChart = axisChart;
  exports.scatterChart = scatterChart;
  exports.xLine = xLine;
  exports.yLine = yLine;
  exports.parametricCurve = parametricCurve;
  exports.heatmapChart = heatmapChart;
  exports.cache = cache;
  exports.separateBy = separateBy;
  exports.getEuclideanDistance = getEuclideanDistance;
  exports.add_click_listener = add_click_listener;
  exports.pearsonCorr = pearsonCorr;
  exports.fillTextBlock = fillTextBlock;
  exports.get_symbolSize = get_symbolSize;
  exports.escapeRegExp = escapeRegExp;
  exports.sigmoidColorSlider = sigmoidColorSlider;
  exports.simpleTable = simpleTable;
  exports.barChart = barChart;
  exports.dendogram = dendogram;

  Object.defineProperty(exports, '__esModule', { value: true });

}));