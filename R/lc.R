#' @import JsRCom
#' @import stringr
#' @export
lc <- new.env()
lc$pageOpened <- F

lc$props <- list(scatter = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle"),
                barchart = c("ngroups", "groupIds", "nbars", "barIds", "nstacks", "stackIds", "value", "groupWidth", "stroke", "strokeWidth",
                             "nbins"), 
                beeswarm = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle", "valueAxis"),
                pointLine = c("lineWidth", "dasharray", "x", "y", "nsteps", "value"),
                pointRibbon = c("lineWidth", "dasharray", "x", "ymax", "ymin", "nsteps"),
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

lineDataFun <- function(l) {
  if(!is.null(l$x) && !is.vector(l$x)) l$x <- as.matrix(l$x)
  if(!is.null(l$y) && !is.vector(l$y)) l$y <- as.matrix(l$y)
  if(!is.null(l$x) | !is.null(l$y)){
    if(is.matrix(l$x)){
      if(nrow(l$x) != nrow(l$y))
        stop("'x' and 'y' define different number of lines.")
      if(ncol(l$x) != ncol(l$y))
        stop("Lengths of 'x' and 'y' differ.")
      
    } else {
      if(length(l$x) != length(l$y))
        stop("Lengths of 'x' and 'y' differ.")
    }
  }
  
  if(!is.null(l$x)) {
    if(is.matrix(l$x)) {
      l$nelements <- ncol(l$x)
      l$nsteps <- nrow(l$x)      
    } else {
      l$nelements <- 1
      l$nsteps <- length(l$x)
    }
  }
  
  l
}

#' @export
lc_line <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("pointLine", data, place, id, layerId, lineDataFun)
}

#' @export
lc_ribbon <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("pointRibbon", data, place, id, layerId, function(l) {
    if(!is.null(l$x) && !is.vector(l$x)) l$x <- as.matrix(l$x)
    if(!is.null(l$ymax) && !is.vector(l$ymax)) l$ymax <- as.matrix(l$ymax)
    if(!is.null(l$ymin) && !is.vector(l$ymin)) l$ymin <- as.matrix(l$ymin)
    
    if(!is.null(l$ymax) | !is.null(l$ymin) | !is.null(l$x)){
      if(is.matrix(l$x)) {
        if((nrow(l$ymax) != nrow(l$ymin)) | (nrow(l$x) != nrow(l$ymin)))
          stop("'x', 'ymax' and 'ymin' define different number of lines.")
        if((ncol(l$ymax) != ncol(l$ymin)) | (ncol(l$x) != ncol(l$ymin)))
          stop("Lengths of 'x', 'ymax' and 'ymin' differ.")
        
      } else {
        if(length(l$ymax) != length(l$ymin) | length(l$x) != length(l$ymin))
          stop("Lengths of 'x', 'ymax' and 'ymin' differ.")
      }
    }
    
    if(!is.null(l$x)) {
      if(is.matrix(l$x)) {
        l$nelements <- ncol(l$x)
        l$nsteps <- nrow(l$x)      
      } else {
        l$nelements <- 1
        l$nsteps <- length(l$x)
      }
    }
  
    l
  })
}

barDataFun <- function(l) {
  if(!is.null(l$barIds) && length(l$barIds) != length(l$value))
    stop("Number of bar IDs is not equal to the number of provided values.")
  if(!is.null(l$stackIds) && length(l$stackIds) != length(l$value))
    stop("Number of stack IDs is not equal to the number of provided values.")
  if(!is.null(l$groupIds) && length(l$groupIds) != length(l$value))
    stop("Number of group IDs is not equal to the number of provided values.")
  
  if(all(is.null(l$groupIds), is.null(l$barIds), is.null(l$stackIds)))
    l$groupIds <- 1:length(l$value)
  
  if(is.null(l$groupIds)) l$groupIds <- rep(1, length(l$value))
  if(is.null(l$barIds)) l$barIds <- rep(1, length(l$value))
  if(is.null(l$stackIds)) l$stackIds <- rep(1, length(l$value))
  
  ngroups <- length(unique(l$groupIds))
  nbars <- length(unique(l$barIds))
  nstacks <- length(unique(l$stackIds))
  
  inds <- NULL
  
  if(is.numeric(l$groupIds) & !is.integer(l$groupIds)){
    inds <- unique(l$groupIds)
    l$groupIds <- match(l$groupIds, inds)
  }
  
  vals <- list()
  
  for(i in 1:length(l$value)) {
    
    if(is.null(vals[[as.character(l$groupIds[i])]])) vals[[as.character(l$groupIds[i])]] <- list()
    if(is.null(vals[[as.character(l$groupIds[i])]][[as.character(l$barIds[i])]])) 
      vals[[as.character(l$groupIds[i])]][[as.character(l$barIds[i])]] <- list()
    
    vals[[as.character(l$groupIds[i])]][[as.character(l$barIds[i])]][[as.character(l$stackIds[i])]] <- l$value[i]
  }
  
  if(!is.null(inds)) {
    vals$`__inds__` <- inds
    l$groupIds <- inds
  }
    
  
  l$value <- vals
  l$groupIds <- unique(l$groupIds)
  l$barIds <- unique(l$barIds)
  l$stackIds <- unique(l$stackIds)
  
  l
}
#' @export
lc_bars <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("barchart", data, place, id, layerId, barDataFun)
}

#' @export
# has a nbins property. Not implemented in JS
lc_hist <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("barchart", data, place, id, layerId, function(l) {
    if(is.null(l$nbins)) {
      nbins <- 10
    } else {
      nbins <- l$nbins
    }
    if(!is.numeric(nbins) || nbins < 1) 
      stop("'nbins' must be a positive number")
    
    l$nbins <- NULL
    
    l$contScaleX <- TRUE
    l$addColourScaleToLegend <- FALSE
    l$groupWidth <- 1
    
    if(!is.null(l$value)) {
      minV <- min(l$value, na.rm = T)
      maxV <- max(l$value, na.rm = T)
      breaks <- seq(minV, maxV, length.out = nbins + 1)
      step <- breaks[2] - breaks[1]
      groupIds <- breaks - step/2
      groupIds <- groupIds[-1]
      binned <- .bincode(l$value, breaks, include.lowest = T)
      value <- sapply(1:nbins, function(i) {sum(binned == i)})
      
      l$groupIds <- groupIds
      l$value <- value
    }
    barDataFun(l)
  })
}

#' @export
lc_dens <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("pointLine", data, place, id, layerId, function(l) {
    if(!is.null(l$value)) {
      dens <- density.default(l$value)
      l$x <- dens$x
      l$y <- dens$y
      l$value <- NULL
    }
    lineDataFun(l)
  })
}