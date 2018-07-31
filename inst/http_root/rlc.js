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
      jrc.sendCommand("chartEvent("+ d + ", '" + id + "', '" + layerId + "', 'click')");
    });
  if(layerId == "main")
    charts[id].placeIn = place
  else
    if(charts[id].container)
      charts[id].place_layer(lyaerId);
}

rlc.setCustomMouseOver = function(id, layerId) {
  if(!charts[id].customMouseOver){
    var pacer = lc.call_pacer(100);
    charts[id].get_layer(layerId)
      .elementMouseOver(function(d) {
        pacer.do(function() {jrc.sendCommand("chartEvent("+ d + ", '" + id + "', '" + layerId + "', 'mouseover')")});
      })
    charts[id].customMouseOver = true;
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
    if(updateType == "")
      charts[id].updateLayer(layerId)
    else
      charts[id].activeLayer(charts[id].get_layer(layerId))[updateFun]();
  else
    charts[id][updateFun]();
}