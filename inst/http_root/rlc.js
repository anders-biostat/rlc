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
    charts[id].on_click(function(d) {
      if(Array.isArray(d))
        d = "c(" + d + ")";
      jrc.sendCommand("rlc:::chartEvent("+ d + ", '" + id + "', '" + layerId + "', 'click')");
    });
  if(layerId == "main")
    charts[id].placeIn = place
  else
    if(charts[id].container) {
      charts[id].place_layer(layerId);
      
    }
}

rlc.setCustomMouseOver = function(id, layerId, parcerStep) {
  if(!charts[id].customMouseOver){
    var pacer = lc.call_pacer(parcerStep); 

    if(layerId != "main")
      charts[id].get_layer(layerId)
        .elementMouseOver(function(d) {
          pacer.do(function() {jrc.sendCommand("rlc:::chartEvent("+ d + ", '" + id + "', '" + layerId + "', 'mouseover')")}); 
        })
    else
      charts[id]
        .elementMouseOver(function(d) {
          pacer.do(function() {jrc.sendCommand("rlc:::chartEvent("+ d + ", '" + id + "', '" + layerId + "', 'mouseover')")}); 
        });      
    charts[id].customMouseOver = true;
  }
}

rlc.setCustomMouseOut = function(id, layerId) {
  if(!charts[id].customMouseOut){
    if(layerId != "main")
      charts[id].get_layer(layerId)
        .elementMouseOut(function() {
          jrc.sendCommand("rlc:::chartEvent(NULL, '" + id + "', '" + layerId + "', 'mouseout')");
        })
    else
      charts[id]
        .elementMouseOut(function(d) {
          jrc.sendCommand("rlc:::chartEvent(NULL, '" + id + "', '" + layerId + "', 'mouseout')");
        });      
    charts[id].customMouseOut = true;
  }
}

rlc.setCustomMarkedUpdated = function(id, layerId) {
  if(!charts[id].customMarkedUpdated){
    if(layerId != "main")
      charts[id].get_layer(layerId)
        .markedUpdated(function() {
          jrc.sendCommand("rlc:::chartEvent(NULL, '" + id + "', '" + layerId + "', 'markedUpdated')");
        })
    else
      charts[id]
        .markedUpdated(function(d) {
          jrc.sendCommand("rlc:::chartEvent(NULL, '" + id + "', '" + layerId + "', 'markedUpdated')");
        });      
    charts[id].customMarkedUpdated = true;
  }
}

rlc.setProperty = function(name) {
  var spl = name.split("_");
  var id = spl[0];
  if(spl[1] != "main")
    charts[id].activeLayer(charts[id].get_layer(spl[1]));
  for(pr in window[name]) {
    if(Array.isArray(window[name][pr]) && window[name][pr].length == 1)
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

rlc.updateChart = function(id, updateType, layerId) {
  if(!charts[id]) 
    throw "Error in 'rlc.updateChart': there is no chart with ID " + id;
  if(layerId == "") layerId = undefined;
  if(layerId && !charts[id].layers[layerId])
    throw "Error in 'rlc.updateChart': chart " + id + " doesn't have layer with " +
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

rlc.html = function(code, place, append) {
  if(!place) {
    place = "body";
    append = true;
  }

  var container = d3.select(place);
  if(container.empty())
    container = d3.select("#" + place);

  if(container.empty()) {
    container = d3.select("body");
    append = true;
  }

  container = container.node();
  
  var oldHtml = container.innerHTML;

  if(!append)
    oldHtml = "";

  container.innerHTML = oldHtml + code;
}