charts = {};
rlc = {};

rlc.addTable = function(rows, cols) {
  var table = d3.select("body")
    .append("table");
  var rowInd = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  table.selectAll("tr")
    .data(d3.range(rows))
        .enter().append("tr");

  table.selectAll("tr").selectAll("td")
    .data(function(d) {
      return d3.range(cols).map(function(e){
        return rowInd[d] + (e + 1)
      })    
    })
    .enter().append("td")
      .attr("id", function(d) {return d});
}

rlc.prepareContainer = function(place) {
  var container = d3.select("#" + place);
  if(container.empty()) {
    d3.select("body")
      .append("div")
        .attr("id", place);
  }
}

rlc.removeChart = function(id) {
  charts[id].container.remove();
  charts[id] = null;
}

rlc.removeLayer = function(chartId, layerId) {
  charts[chartId].remove_layer(layerId);
}


rlc.addChart = function(id, type, place, layerId) {
  //if(layerId = "") layerId = undefined;
  charts[id] = lc[type](layerId, charts[id]);
  if(charts[id].on_click)
    charts[id].on_click(function(d, d2, d3) {
      if(d2 != undefined)
        d = [d, d2];
      if(d3 != undefined)
        d = d.concat(d3);

      jrc.callFunction("chartEvent", {d: d, id: id, layerId: layerId, event: "click"}, null, "rlc");
    });
  if(layerId == "main")
    charts[id].placeIn = place
  else
    if(charts[id].container) {
      charts[id].place_layer(layerId);
    }
  //inputs, but not a colour slider
  if(charts[id].on_change && !charts[id].on_drag)
    charts[id].on_change(
      function(value) {
        jrc.callFunction("chartEvent", {d: value, id: id, layerId: layerId, event: "click"}, null, "rlc");
      }
    )
}

rlc.setCustomMouseOver = function(id, layerId, pacerStep) {
  if(!charts[id].customMouseOver){
    var pacer = lc.call_pacer(pacerStep); 

    if(layerId != "main")
      charts[id].get_layer(layerId)
        .on_mouseover(function(d) {
          pacer.do(function() {jrc.callFunction("chartEvent", {d: d, id: id, layerId: layerId, event: "mouseover"}, null, "rlc")})
        });
    else
      charts[id]
        .on_mouseover(function(d) {
          pacer.do(function() {jrc.callFunction("chartEvent", {d: d, id: id, layerId: layerId, event: "mouseover"}, null, "rlc")}); 
        });
    charts[id].customMouseOver = true;
  }
}

rlc.setCustomMouseOut = function(id, layerId) {
  if(!charts[id].customMouseOut){
    if(layerId != "main")
      charts[id].get_layer(layerId)
        .on_mouseout(function() {
          jrc.callFunction("chartEvent", {d: "NULL", id: id, layerId: layerId, event: "mouseout"}, null, "rlc");
        })
    else
      charts[id]
        .on_mouseout(function() {
          jrc.callFunction("chartEvent", {d: "NULL", id: id, layerId: layerId, event: "mouseout"}, null, "rlc");
        });      
    charts[id].customMouseOut = true;
  }
}

rlc.setCustomOnMarked = function(id, layerId) {
  if(!charts[id].customOnMarked){
    if(layerId != "main")
      charts[id].get_layer(layerId)
        .on_marked(function() {
          jrc.callFunction("chartEvent", {d: "NULL", id: id, layerId: layerId, event: "marked"}, null, "rlc");
          console.log(new Error().stack)
        })
    else
      charts[id]
        .on_marked(function(d) {
          jrc.callFunction("chartEvent", {d: "NULL", id: id, layerId: layerId, event: "marked"}, null, "rlc");
        });      
    charts[id].customOnMarked = true;
  }
}

rlc.setCustomClickLabel = function(id, type) {
  if(!charts[id]["customClickLabel" + type]){
    charts[id]
      ["on_labelClick" + type](function(d) {
        jrc.callFunction("chartEvent", {d: d, id: id, layerId: "main", event: "labelClick" + type}, null, "rlc")
      });      
    charts[id]["customClickLabel" + type] = true;
  }
}

rlc.setProperty = function(name) {
  var spl = name.split("_");
  var id = spl[0];
  if(spl[1] != "main")
    charts[id].activeLayer(charts[id].get_layer(spl[1]));
  for(pr in window[name]) {
    if(pr == "paddings"){
      if(window[name][pr].top)
        window[name][pr].top = window[name][pr].top[0];
      if(window[name][pr].bottom)
        window[name][pr].bottom = window[name][pr].bottom[0];
      if(window[name][pr].left)
        window[name][pr].left = window[name][pr].left[0];
      if(window[name][pr].right)
        window[name][pr].right = window[name][pr].right[0];
      charts[id].set_paddings(window[name][pr]);
    } else if(pr == "rankRows") {
      orderRow = window[name][pr];
      charts[id].rowIds(d3.range(window[name][pr].length));
      charts[id].reorder("Row", function(a, b) {
        return orderRow[a] - orderRow[b];
      })
    } else if(pr == "rankCols") {
      orderCol = window[name][pr];
      charts[id].colIds(d3.range(window[name][pr].length));
      charts[id].reorder("Col", function(a, b) {
        return orderCol[a] - orderCol[b];
      })
    } else if(Array.isArray(window[name][pr]) && window[name][pr].length == 1)
      charts[id][pr](window[name][pr][0])
    else  
      charts[id][pr](window[name][pr]);
  }
  window[name] = null;
}

rlc.getMarked = function(id, layerId) {
  var marked;
  if(!charts[id])
    throw "Error in 'rlc.getMarked': there is no chart with ID " + id;

  if(layerId == "main")
    marked = charts[id].get_marked()
  else
    marked = charts[id].get_layer(layerId).get_marked();

  if(marked.empty)
    marked = marked.data();

  jrc.sendData("marked", marked);
}

rlc.mark = function(id, layerId, pe) {
  if(markElements[0] == "__clear__")
    markElements = "__clear__";
  if(layerId == "main")
    charts[id].mark(markElements, pe)
  else
    charts[id].get_layer(layerId).mark(markElements, pe);
}

rlc.updateCharts = function(id, updateType, layerId) {
  if(!charts[id]) 
    throw "Error in 'rlc.updateCharts': there is no chart with ID " + id;
  if(layerId == "") layerId = undefined;
  if(layerId && !charts[id].layers[layerId])
    throw "Error in 'rlc.updateCharts': chart " + id + " doesn't have layer with " +
      "ID: " + layerId;

  if(charts[id].placeIn) {
    if(!d3.select(charts[id].placeIn).empty())
      charts[id].place(charts[id].placeIn)
    else
      charts[id].place("#" + charts[id].placeIn);
    charts[id].placeIn = undefined;
  }

  var updateFun = "update" + updateType;

  if(layerId)
    charts[id].activeLayer(charts[id].get_layer(layerId))[updateFun]()
  else
    charts[id][updateFun]();
}
