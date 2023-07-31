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

  table.selectAll("td").nodes()
    .forEach(function(el) {delete el.__data__ });

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
      if(d3 !== undefined && d3.type !== undefined)
        d3 = undefined;
      if(d2 !== undefined && d2.type !== undefined)
        d2 = undefined;      
      if(d2 != undefined)
        d = [d, d2];
      if(d3 != undefined)
        d = d.concat(d3);

      jrc.callFunction("chartEvent", {d: d, chartId: id, layerId: layerId, event: "click", sessionId: jrc.id}, null, "rlc");
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
        jrc.callFunction("chartEvent", {d: value, chartId: id, layerId: layerId, event: "click", sessionId: jrc.id}, null, "rlc");
      }
    )
}

rlc.setCustomEvent = function(type, chartId, layerId, dNull, pacerStep) {
  var obj = {}, args = {};
  if(pacerStep !== undefined ) 
    charts[chartId].pacer = lc.call_pacer(pacerStep)    
  
  if(layerId == "main") {
    obj = charts[chartId]
  } else {
    obj = charts[chartId].get_layer(layerId);
  }

  if(obj.customEvent === undefined)
    obj.customEvent = {};
  if(!obj.customEvent[type]) {
    obj["on_" + type](function() {
      if(dNull) {
        args.d = "NULL"
      } else {
        args.d = Object.values(arguments).filter(e => e.type === undefined);
      }
      args.chartId = chartId;
      args.layerId = layerId;
      args.event = type;
      args.sessionId = jrc.id;
      
      jrc.callFunction("chartEvent", args, null, "rlc")
    })
    obj.customEvent[type] = true;
  }
}

rlc.setCustomClickPosition = function(id) {
  if(charts[id].clickPosition)
    rlc.setCustomEvent("clickPosition", id, "main", false);
}

rlc.setCustomMouseOver = function(id, layerId, pacerStep) {
  rlc.setCustomEvent("mouseover", id, layerId, false, pacerStep);
}

rlc.setCustomMouseOut = function(id, layerId, pacerStep) {
  rlc.setCustomEvent("mouseout", id, layerId, true, pacerStep);
}

rlc.setCustomOnMarked = function(id, layerId) {
  rlc.setCustomEvent("marked", id, layerId, true)
}

rlc.setCustomClickLabel = function(id, type) {
  rlc.setCustomEvent("labelClick" + type, id, "main", false);
}

rlc.setProperty = function(name) {
  var spl = name.split("_sep_");
  var id = spl[0];
  if(spl[1] != "main")
    charts[id].activeLayer(charts[id].get_layer(spl[1]));
  for(pr in window[name]) {
    if(pr == "paddings"){
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
    } else if(pr.substring(0, 7) == "legend_") {
      charts[id].legend[pr.substring(7)](window[name][pr]);
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

  jrc.sendData(".marked", marked);
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
