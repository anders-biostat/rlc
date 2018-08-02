#' @import JsRCom
#' @import stringr
#' @export
lc <- new.env()
lc$pageOpened <- F

lc$props <- list(scatter = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle"),
                barchart = c("ngroups", "groupIds", "nbars", "barIds", "nstacks", "value", "groupWidth", "stroke", "strokeWidth"), 
                beeswarm = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle", "valueAxis"),
                pointLine = c("lineWidth", "dasharray", "x", "y", "nsteps"),
                pointRibbon = c("lineWidth", "dasharray", "x", "y", "nsteps"),
                layer = c("nelements", "elementIds", "elementLabel", "layerDomainX", "layerDomainY", "contScaleX", "contScaleY",
                          "colour", "colourValue", "palette", "colourDomain", "colourLegendTitle", "addColourScaleToLegend", "opacity", "on_click",
                          "informText", "elementMouseOver", "elementMouseOut"))

Layer <- setRefClass("Layer", fields = list(type = "character", id = "character", 
                                            properties = "list", dataFun = "function",
                                            on_click = "function", on_mouseover = "function",
                                            on_mouseout = "function", init = "logical"))
Layer$methods(
  setProperty = function(name, expr) {
    properties[[name]] <<- expr
  }
)
Layer$accessors("type")

Chart <- setRefClass("Chart", fields = list(layers = "list", id = "character", place = "character"))
Chart$methods(
  addLayer = function(layerId, type) {
    if((length(layers) != 0) && (layerId %in% names(layers)))
      stop(str_c("Layer with ID ", layerId, " already exists in chart ", id, ".", 
                 " Use 'chart$removeLayer(layerId)' to remove it."))
    
    layers[[layerId]] <<- Layer$new(type = type, id = layerId, properties = list(), init = F,
                                    dataFun = function(l) l)
    layers[[layerId]]
  },
  getLayer = function(layerId = NULL) {
    if(is.null(layerId))
      if(length(layers) == 2) {
        #1st layer is always 'main', which is a dummy
        layerId <- layers[[2]]$id
      } else {
        stop(str_c("Chart ", id, " has multiple or no layers. You need to specify the 'layerID'."))
      }
    layers[[layerId]]
  },
  removeLayer = function(layerId) {
    if(layerId == "main"){
      warning("You are attempting to remove the main layer of the chart ", id, 
              " . The entire chart will be removed.")
      removeChart(id)
    } else {
      if(layerId %in% names(layers)) {
        stop(str_c("There is no layer with ID ", layerId))
      } else {
        sendCommands(str_interp("rlc.removeLayer('${id}, '${layerId}')"))
        layers[[layerId]] <<- NULL
      }
    }
  },
  nLayers = function() {
    length(layers) - 1
  },
  JSinitialize = function() {
    lapply(layers, function(layer) {
      if(!layer$init) {
        print(layer$id)
        sendCommand(str_interp("rlc.addChart('${id}', '${layer$type}', '${place}', '${layer$id}');"))
        layer$init <- T
      }
    })
  })
                                    
Chart$accessors("place")
lc$charts <- list()

#' @export
#' @importFrom later run_now
openPage <- function(useViewer = T, rootDirectory = NULL, startPage = NULL, layout = NULL) {
  JsRCom::openPage(useViewer = useViewer, rootDirectory = rootDirectory, startPage = startPage)
  srcDir <- system.file("http_root", package = "rlc")

  scriptCount <- 0
  setEnvironment(environment())
  sendCommand(str_c("link = document.createElement('link');", 
                    "link.rel = 'stylesheet';", 
                    "link.href = '", srcDir, "/linked-charts.css';", 
                    "document.head.appendChild(link);", collapse = "\n")) 
  sendCommand(str_c("script = document.createElement('script');", 
                    "script.src = '", srcDir, "/rlc.js';",
                    "script.onload = function() {jrc.sendCommand('scriptCount <- scriptCount + 1')};",
                    "document.head.appendChild(script);", collapse = "\n"))
    
  sendCommand(str_c("script = document.createElement('script');", 
                    "script.src = '", srcDir, "/linked-charts.min.js';",
                    "script.onload = function() {jrc.sendCommand('scriptCount <- scriptCount + 1')};",                    
                    "document.head.appendChild(script);", collapse = "\n"))
  for( i in 1:(10/0.05) ) {
    run_now()
    if( scriptCount == 2) {
      setEnvironment(globalenv())
      break
    } 
    Sys.sleep( .05 )
  }
  
  if( scriptCount < 2 ) {
    closePage()
    stop( "Can't load linked-charts.js or rlc.js" )
  }
  
  if(!is.null(layout)) addDefaultLayout(layout)

  lc$pageOpened <- T
}

#' @export
addDefaultLayout <- function(layoutName) {
  if(grepl("^table", layoutName)){
    size <- as.numeric(str_extract_all(layoutName, "\\d", simplify = T))
    if(length(size) != 2) stop("Size of the table is specified incorrectly")
    sendCommand(str_interp("rlc.addTable(${size[1]}, ${size[2]});"))
      
    invisible(return())
  }
  stop("Unknown default layout name")
}

#Makes sure that a container exists. Otherwise just creates a 'div' with the provided id
prepareContainer <- function(place) {
    sendCommand(str_interp("rlc.prepareContainer('${place}');"))
}

getChart <- function(id) {
  lc$charts[[id]]
}

#' @export
removeChart <- function(id) {
  if(!is.vector(id))
    stop("Error in 'removeChart': 'id' should be a vector of IDs")
  for(i in id){
    lc$charts[[i]] <- NULL
    sendCommand(str_interp("rlc.removeChart('${i}');"))  
  }
}

addChart <- function(id, place) {
  
  chart <- Chart$new(layers = list(), id = id)
  chart$setPlace(place)
  lc$charts[[id]] <- chart
  
  invisible(chart)
}

#' @export
setProperties <- function(data, id, layerId = NULL) {
  chart <- getChart(id)
  if(is.null(chart))
    stop(str_c("Chart with ID ", id, " is not defined."))
  
  if(is.null(layerId))
    if(chart$getLayer("main")$type != "layerChart") {
      layerId <- "main"
    } else {
      if(chart$nLayers() == 1) {
        layerId <- names(chart$layers)[2]
      } else {
        stop("The chart ", chart$id, " has more then one layer. Please, specify the 'layerId'.")
      }
    }
    
  layer <- chart$getLayer(layerId)
  mainLayer <- chart$getLayer("main")
  
  if(is.null(layer))
    stop(str_c("Layer with ID ", id, " is not defined."))
  
  for(prop in names(data))
    if(prop %in% lc$props[[layer$type]] | prop %in% lc$props$layer) {
      layer$setProperty(prop, data[[prop]])
    } else {
      mainLayer$setProperty(prop, data[[prop]])
    }
  
  invisible(chart)
}

#' @export
updateChart <- function(id = NULL, layerId = NULL, updateType = NULL) {
  if(is.null(id)) id <- ls(lc$charts)
  if(!is.vector(id))
    stop("'id' should be a vector of IDs")

  
  if(!is.null(layerId) & length(layerId) != length(id))
    stop("Lengths of 'id' and 'layerId' differ")
  if(!is.null(updateType) & length(updateType) != length(id))
    stop("Lengths of 'id' and 'updateType' differ")
  
  for(i in 1:length(id)){
    chart <- getChart(id[i])
    if(is.null(chart))
      stop(str_c("Chart with ID ", id[i], " is not defined."))
    args <- str_c("'", id[i], "', '")
    
    if(!is.null(updateType)) {
      args <- str_c(args, updateType[i], "', '")
    } else {
      args <- str_c(args, "', '")
    }
    
    if(!is.null(layerId)) {
      args <- str_c(args, layerId[i], "'")
      sendProperties(chart, layerId[i])
    } else {
      sendProperties(chart)
      args <- str_c(args, "'")
    }
    sendCommand(str_interp("rlc.updateChart(${args});"))
  }
}

sendProperties <- function(chart, layerId = ls(chart$layers)){
  for(layerName in layerId){
    layer <- chart$getLayer(layerName)
    if(is.null(layer))
      stop(str_c("There is no layer ", layerName, " in the chart ", chart$id))
    
    e <- new.env()
    tryCatch({
      d <- lapply(layer$properties, function(el) el())
      d <- layer$dataFun(d)
    },
    error = function(e) 
      stop( str_interp( "in data expression for chart '${chart$id}': ${e$message}." ), call.=FALSE ) ) 
    
    if(!is.null(d$on_click)) {
      layer$on_click <- d$on_click
      d$on_click = NULL
    }
    
    if(!is.null(d$elementMouseOver)) {
      layer$on_mouseover <- d$elementMouseOver
      d$elementMouseOver = NULL
      sendCommand(str_interp("rlc.setCustomMouseOver('${chart$id}', '${layerName}');"))
    }
    if(!is.null(d$elementMouseOver)) {
      layer$on_mouseout <- d$elementMouseOut
      d$elementMouseOut = NULL
      sendCommand(str_interp("rlc.setCustomMouseOut('${chart$id}', '${layerName}');"))
    }    
    
    name <- str_c(chart$id, layer$id, sep = "_")
    
    sendData(name, d)
    sendCommand(str_interp("rlc.setProperty('${name}')"))
  }
}

setChart <- function(type, data, place, id, layerId, dataFun) {
  if(!lc$pageOpened) openPage()
  
  if(is.null(place))
    place <- str_c("Chart", length(lc$charts) + 1)
  prepareContainer(place)
  
  if(is.null(id))
    id <- place
  
  chart <- getChart(id)
  if(is.null(chart)) {
    chart <- addChart(id, place)
    if(is.null(layerId) || layerId != "main")
      chart$addLayer("main", "axesChart")
  }
  
  if(is.null(layerId))
    layerId <- str_c("Layer", (chart$nLayers() + 1))
  

  layer <- chart$addLayer(layerId, type)
  layer$dataFun <- dataFun
  
  chart$JSinitialize()
  
  setProperties(data, id, layerId)
  updateChart(id)
  
  invisible(chart)
}

#' @export
chartEvent <- function(d, id, layerId, event) {
  
  chart <- getChart(id)
  if(is.null(chart))
    stop(str_interp("Chart with ID ${id} is not defined"))
  
  layer <- chart$getLayer(layerId)
  if(is.null(layer))
    stop(str_interp("Chart ${id} doesn't have layer ${layerId}"))

  if(event == "click" & !is.null(layer$on_click))
    layer$on_click(d)
  if(event == "mouseover" & !is.null(layer$on_mouseover))
    layer$on_mouseover(d)
  if(event == "mouseout" & !is.null(layer$on_mouseout))
    layer$on_mouseout(d)
}

#' @export
dat <- function( ... ) {
  e <- parent.frame()
  l <- match.call()[-1]
  lapply(l, function(el) {
    function() eval(el, e)
  })
}

#' @export
closePage <- function() {
  lc$charts <- list()
  lc$pageOpened <- F
  JsRCom::closePage()
}

#' @export
lc_scatter <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("scatter", data, place, id, layerId, function(l) {
    l
  })
}

#' @export
lc_beeswarm <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("beeswarm", data, place, id, layerId, function(l) {
    l
  })
}

#' @export
lc_line <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("pointLine", data, place, id, layerId, function(l) {
    if(!is.null(l$x)) l$x <- t(as.matrix(l$x))
    if(!is.null(l$y)) l$y <- t(as.matrix(l$y))
    if(!is.null(l$x) | !is.null(l$y))
      if(nrow(l$x) != nrow(l$y))
        stop("'x' and 'y' define different number of lines")
    
    if(!is.null(l$x)) l$nelements <- nrow(l$x)
    
    l
  })
}

#' @export
lc_ribbon <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("pointRibbon", data, place, id, layerId, function(l) {
    l
  })
}
