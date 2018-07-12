#' @import JsRCom
#' @import stringr
#' @export
lc <- new.env()
lc$pageOpened <- F
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

prepareContainer <- function(place) {
    sendCommand(str_interp("rlc.prepareContainer('${place}');"))
}

#' @export
updateChart <- function(id = NULL, layerId = NULL, updateType = NULL) {
  if(!is.vector(id))
    stop("Error in 'updateChart': 'id' should be a vector of IDs")
  if(is.null(id)) id <- ls(lc$charts)
  if(!is.null(layerId) & length(layerId) != length(id))
    stop("Error in 'updateChart': lenths of 'id' and 'layerId' differ")
  if(!is.null(updateType) & length(updateType) != length(id))
    stop("Error in 'updateChart': lenths of 'id' and 'updateType' differ")

  for(i in 1:length(id)){
    args <- str_c("'", id[i], "', '")
    if(!is.null(updateType)) {
      args <- str_c(args, updateType[i], "', '")
    } else {
      args <- str_c(args, "', '")
    }

    if(!is.null(layerId)) {
      args <- str_c(args, layerId[i], "'")
      sendProperties(id[i], layerId[i])
    } else {
      sendProperties(id[i])
      args <- str_c(args, "'")
    }
    sendCommand(str_interp("rlc.updateChart(${args});"))
  }
}

#' @export
removeChart <- function(id) {
  if(!is.vector(id))
    stop("Error in 'removeChart': 'id' should be a vector of IDs")
  for(i in id){
    lc$charts[[i]] = NULL
    sendCommand(str_interp("rlc.removeChart('${i}');"))  
  }
}

#' @export
setProperties <- function(data, id, layerId = NULL) {
  if(is.null(layerId))
    if(length(lc$charts[[id]]$layers) == 1) {
      layerId <- ls(lc$charts[[id]]$layers)[1]
    } else {
      stop("LayerId is not specified")
    }
  if(is.null(lc$charts[[id]]$layers[[layerId]]))
    lc$charts[[id]]$layers[[layerId]] <- list()
  lc$charts[[id]]$layers[[layerId]]$data <- data
  
  class(id) <- "chartId"
  invisible(id)
}

sendProperties <- function(id, layerId = ls(lc$charts[[id]]$layers)){
  for(layer in layerId){
    e <- new.env()
    tryCatch(
      { d <- lc$charts[[id]]$layers[[layer]]$data() },
      error = function(e) 
        stop( str_interp( "in data expression for chart '${id}': ${e$message}." ), call.=FALSE ) ) 
    
    if(!is.null(d$on_click)) {
      lc$charts[[id]]$layers[[layer]]$on_click <- d$on_click
      d$on_click = NULL
    }
    if(!is.null(d$elementMouseOver)) {
      lc$charts[[id]]$layers[[layer]]$on_mouseover <- d$elementMouseOver
      d$elementMouseOver = NULL
      sendCommand(str_interp("rlc.setCustomMouseOver('${id}', '${layer}');"))
    }
    if(!is.null(d$elementMouseOver)) {
      lc$charts[[id]]$layers[[layer]]$on_mouseout <- d$elementMouseOut
      d$elementMouseOut = NULL
      sendCommand(str_interp("rlc.setCustomMouseOut('${id}', '${layer}');"))
    }    
        
    name <- str_c(id, layerId, sep = "_")
    
    sendData(name, d)
    sendCommand(str_interp("rlc.setProperty('${name}')"))
  }
}

#' @export
chartEvent <- function(d, id, layerId, event) {
  
  if(is.null(lc$charts[[id]]))
    stop(str_interp("Chart with ID ${id} is not defined"))
  if(is.null(lc$charts[[id]]$layers[[layerId]]))
    stop(str_interp("Chart ${id} doesn't have layer ${layerId}"))

  if(event == "click" & !is.null(lc$charts[[id]]$layers[[layerId]]$on_click))
    lc$charts[[id]]$layers[[layerId]]$on_click(d)
  if(event == "mouseover" & !is.null(lc$charts[[id]]$layers[[layerId]]$on_mouseover))
    lc$charts[[id]]$layers[[layerId]]$on_mouseover(d)
  if(event == "mouseout" & !is.null(lc$charts[[id]]$layers[[layerId]]$on_mouseout))
    lc$charts[[id]]$layers[[layerId]]$on_mouseout(d)
}


addChart <- function(id, type, place, layerId) {
  if(chartExists(id)) {
    warning(str_interp("Chart with ID ${id} already exists. It will be replaced with a new one."))
    removeChart(id)
  }
  
  prepareContainer(place)
  lc$charts[[id]] <- list()
  lc$charts[[id]]$place <- place
  lc$charts[[id]]$layers <- list()
  
  sendCommand(str_interp("rlc.addChart('${id}', '${type}', '${place}', '${layerId}');"))
}

#' @export
chartExists <- function(id) {
  sum(ls(lc$charts) == id) > 0
}

#' @export
lc_scatter <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("scatter", data, place, id, layerId)
}

#' @export
lc_beeswarm <- function(data, place = NULL, id = NULL, layerId = NULL) {
  setChart("beeswarm", data, place, id, layerId)
}

 
setChart <- function(type, data, place, id, layerId) {
  if(!lc$pageOpened) openPage()

  if(is.null(place))
    place <- str_c("Chart", length(lc$charts) + 1)
  
  if(is.null(id))
    id <- place
  
  if(is.null(layerId))
    if(is.null(lc$charts[[id]])) {
      layerId <- "Layer1"
    } else {
      lyaerId <- str_c("Layer", length(lc$charts[[id]]$layers) + 1)
    }
  
  addChart(id, type, place, layerId)
  setProperties(data, id, layerId)
  updateChart(id)
  
  class(id) <- "chartId"
  invisible(id)
}

#' @export
dat <- function( ... ) {
  e <- parent.frame()
  l <- match.call()[-1]
  function() lapply( l, eval, e )
}

#' @export
closePage <- function() {
  lc$charts <- list()
  lc$pageOpened <- F
  JsRCom::closePage()
}
