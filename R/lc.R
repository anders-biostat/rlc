#' @import jrc
#' @import stringr
#' @importFrom methods setRefClass
#' @importFrom methods new

lc <- new.env()
lc$pageOpened <- F

lc$nameList <- c("labels" = "label", "color" = "colour", "colorValue" = "colourValue",
                 "colourValues" = "colourValue", "colorValues" = "colourValue", "colorDomain" = "colourDomain",
                 "colorLegendTitle" = "colourLegendTitle", "addColorScaleToLegend" = "addColourScaleToLegend",
                 "symbols" = "symbol", "symbolValues" = "symbolValue", "strokes" = "stroke", "values" = "value",
                 "heatmapRows" = "heatmapRow", "heatmapCols" = "heatmapCol", "showValues" = "showValue",
                 "globalColorScale" = "globalColourScale", "steps" = "step")

lc$props <- list(scatter = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle",
                             "jitterX", "jitterY", "shiftX", "shiftY"),
                barchart = c("ngroups", "groupIds", "nbars", "barIds", "nstacks", "stackIds", "value", "groupWidth", "stroke", "strokeWidth",
                             "nbins"), 
                beeswarm = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle", "valueAxis"),
                pointLine = c("lineWidth", "dasharray", "x", "y", "nsteps", "value", "fill"),
                xLine = c("lineWidth", "dasharray", "lineFun", "nsteps", "a", "b", "h", "fill"),
                yLine = c("lineWidth", "dasharray", "lineFun", "nsteps", "v", "fill"),
                pointRibbon = c("lineWidth", "dasharray", "x", "ymax", "ymin", "nsteps"),
                layer = c("nelements", "elementIds", "label", "layerDomainX", "layerDomainY", "contScaleX", "contScaleY",
                          "colour", "colourValue", "palette", "colourDomain", "colourLegendTitle", "addColourScaleToLegend", "opacity", "on_click",
                          "informText", "on_mouseover", "on_mouseout", "on_marked"),
                input = c("step", "min", "max"),
                all = c("width", "height", "plotWidth", "plotHeight", "paddings", "title", "titleX", "titleY", "titleSize",
                        "showLegend", "showPanel", "transitionDuration", "value", "rowLabel", "colLabel", "showDendogramRow",
                        "clusterRows", "clusterCols", "mode", "heatmapRow", "heatmapCol", "showValue", "rowTitle", 
                        "colTitle", "palette", "colourDomain", "on_click", "on_change", "on_mouseover", "on_mouseout", "on_marked", 
                        "chart", "layer", "content", "type", "domainX", "domainY", "apectRatio", "axisTitleX", "axisTitleY",
                        "logScaleX", "logScaleY", "ticksRotateX", "ticksRotateY", "globalColourScale", "aspectRatio",
                        "rankRows", "rankCols", "ticksX", "ticksY", "showDendogramCol", "on_labelClickCol", "on_labelClickRow"))

Layer <- setRefClass("Layer", fields = list(type = "character", id = "character", 
                                            properties = "list", dataFun = "function",
                                            on_click = "function", on_mouseover = "function",
                                            on_mouseout = "function", init = "logical",
                                            on_marked = "function", pacerStep = "numeric",
                                            on_labelClickRow = "function", on_labelClickCol = "function"))
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
    
    layers[[layerId]] <<- Layer$new(type = type, id = layerId, properties = list(), init = FALSE,
                                    dataFun = function(l) l, on_click = function(d) NULL)
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
      warning(str_c("You are attempting to remove the main layer of the chart ", id, 
              ". The entire chart will be removed."))
      removeChart(id)
    } else {
      if(!(layerId %in% names(layers))) {
        stop(str_c("There is no layer with ID ", layerId))
      } else {
        sendCommand(str_interp("rlc.removeLayer('${id}', '${layerId}')"))
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
        if(layer$id != "main")
          message(str_interp("Layer '${layer$id}' is added to chart '${id}'."))
        sendCommand(str_interp("rlc.addChart('${id}', '${layer$type}', '${place}', '${layer$id}');"))
        layer$init <- T
      }
    })
  })
                                    
Chart$accessors("place")
lc$charts <- list()

#' Open a new empty page
#' 
#' \code{openPage} creates a server, establishes a web socket connection between it and the current
#' R session and loads linked-charts JS library with all the dependencies. If there is already an 
#' opened page, it will be automatically closed.
#' 
#' @param useViewer If \code{TRUE}, the page will be opened in the RStudio Viewer. If \code{FALSE}
#' a default web browser will be used.
#' @param rootDirectory A path to the root directory of the server. If \code{rootDirectory} is not 
#' defined, the \code{http_root} in the package directory will be used as a root directory.
#' @param startPage A path to the HTML file that should be opened, when the server is initialized.
#' This can be an absolute path to a local file, or it can be relative from the \code{rootDirectory}
#' or to the current R working directory. If \code{startPage} is not defined, this function opens an 
#' empty HTML page. The file must have \emph{.html} extension.
#' @param layout Adds one of the defaults layouts to the page. Currently, only tables of arbitrary 
#' size are supported. To add a table set this parameter to \code{tableNxM}, where \code{N} is the
#' number of rows and \code{M} is the number of columns. Each cell will get an ID that consists of 
#' a letter (indicating the row) and a number (indicating the column) (e.g. \code{B3} is an ID of 
#' the second row and third column).
#' @param newPage Determines whether or not to open a new page. If \code{FALSE}, one can add 
#' interactive charts to another apps, created the with \code{jrc} package.
#' @param ... Further arguments passed to \code{\link[jrc]{openPage}}.
#' 
#' @examples
#' \donttest{openPage()
#' 
#' openPage(useViewer = FALSE, layout = "table2x3")}
#' 
#' @export
#' @importFrom httpuv service
openPage <- function(useViewer = TRUE, rootDirectory = NULL, startPage = NULL, layout = NULL, newPage = TRUE, ...) {
  
  lc$charts <- list()
  lc$pageOpened <- F
  lc$useViewer <- useViewer
  if(newPage == TRUE){
    jrc::openPage(useViewer = useViewer, rootDirectory = rootDirectory, startPage = startPage, 
                  allowedFunctions = "chartEvent", allowedVariables = c("marked", "s1", "s2"), ...)
    jrc::limitStorage(n = 0)
    
  }
  
  srcDir <- "http_root_rlc"

  s1 <- 0
  s2 <- 0
  setEnvironment(environment())
  sendCommand(str_c("link = document.createElement('link');", 
                    "link.rel = 'stylesheet';", 
                    "link.href = '", srcDir, "/linked-charts.css';", 
                    "document.head.appendChild(link);", collapse = "\n")) 
  sendCommand(str_c("script = document.createElement('script');", 
                    "script.src = '", srcDir, "/rlc.js';",
                    "script.onload = function() {jrc.sendData('s1', 1)};",
                    "document.head.appendChild(script);", collapse = "\n"))
    
  sendCommand(str_c("script = document.createElement('script');", 
                    "script.src = '", srcDir, "/linked-charts.min.js';",
                    "script.onload = function() {jrc.sendData('s2', 1)};",                    
                    "document.head.appendChild(script);", collapse = "\n"))
  for( i in 1:(10/0.05) ) {
    service()
    if( s1 == 1 & s2 == 1) {
      setEnvironment(globalenv())
      break
    } 
    Sys.sleep( .05 )
  }
  
  if( s1 == 0 | s2 == 0 ) {
    closePage()
    stop( "Can't load linked-charts.js or rlc.js" )
  }
  
  sendCommand("d3.select('title').text('R/linked-charts');")
  
  if(!is.null(layout)) addDefaultLayout(layout)
  
  lc$pageOpened <- T
}

#' Add a default layout to the opened web page
#' 
#' \code{addDefaultLayout} adds a layout that can be later used to arrange charts on the page (by
#' default each new chart is added to the bottom of the page).
#' 
#' Currently the only supported type
#' of a default layout is table with arbitrary number of rows and columns.
#' To use it set the layout argument to \code{tableMxN}, where \code{N} is the
#' number of rows and \code{M} is the number of columns. Each cell will get an ID that consists of 
#' a letter (indicating the row) and a number (indicating the column) (e.g. \code{B3} is an ID of 
#' the second row and third column).
#' 
#' @param layoutName Type of the layout. See 'Details' for more information.
#' 
#' @examples
#' \donttest{openPage(useViewer = FALSE)
#' addDefaultLayout("table3x2")}
#' 
#' @export
addDefaultLayout <- function(layoutName) {
  if(grepl("^table", layoutName)){
    size <- as.numeric(str_extract_all(layoutName, "\\d", simplify = TRUE))
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

#' Remove chart from the page
#' 
#' Removes an existing chart.
#' 
#' @param id A vector of IDs of the charts to be removed.
#' 
#' @examples 
#' \donttest{lc_scatter(dat(x = 1:10, y = 1:10 * 2), id = "scatter")
#' removeChart("scatter")}
#' 
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
  
  message(str_interp("Chart '${id}' added."))
  
  invisible(chart)
}

#' Set properties of the chart
#' 
#' Changes already defined properties or sets the new ones for an
#' existing chart.
#' 
#' @param data Set of properties to be redefined for this layer or chart. Created by \code{\link{dat}}
#' function.
#' @param id ID of the chart, whose properties you want to redefine.
#' @param layerId ID of the layer, whose properties you want to redefine. If the chart has a single
#' layer or doesn't have layers, default value (which is NULL) can be used.
#' 
#' @examples
#' \donttest{data("iris")
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Sepal.Width), id = "irisScatter")
#' setProperties(dat(symbolValue = iris$Species, y = iris$Petal.Length), id = "irisScatter")
#' updateCharts("irisScatter")
#' 
#' lc_line(dat(x = iris$Sepal.Length, y = iris$Petal.Length), id = "irisScatter", layerId = "line")
#' setProperties(dat(colour = "red"), id = "irisScatter", layerId = "line")
#' updateCharts("irisScatter")}
#' 
#' @export
#' @importFrom plyr rename
setProperties <- function(data, id, layerId = NULL) {
  chart <- getChart(id)
  if(is.null(chart))
    stop(str_c("Chart with ID ", id, " is not defined."))
  
  if(is.null(layerId))
    if(chart$getLayer("main")$type != "axesChart") {
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
  
  data <- plyr::rename(data, lc$nameList, warn_missing = FALSE)
    
  if(is.null(layer))
    stop(str_c("Layer with ID ", id, " is not defined."))
  
  for(prop in names(data))
    if(prop %in% lc$props[[layer$type]] | prop %in% lc$props$layer) {
      layer$setProperty(prop, data[[prop]])
    } else {
      if(prop %in% lc$props$all) {
        mainLayer$setProperty(prop, data[[prop]])
      } else {
        warning(str_c("In chart '", chart$id, "': Property '", prop, "' doesn't exist."))
      }
    }
  
  invisible(chart)
}

#' Update a chart
#' 
#' \code{updateCharts} redraws a chart or a single layer of the chart to make it up
#' to date with the current state of the environment.
#' 
#' Linked charts of the \emph{rlc} package are based on the idea that the variables
#' used to define a chart are not constant, but can change as a result of user's
#' actions. Each time the \code{updateCharts} function is called, all the properties passed
#' via \code{\link{dat}} function are reevaluated and the chart is changed in accordance with the
#' new state.
#' 
#' @section Update types: 
#' To improve performance you can update only a certain part of the chart (e.g. colours,
#' size, etc.). This can be done by setting the \code{updateOnly} argument. Here are all
#' possible values for this argument.
#' 
#' These are valid for all the charts:
#' \itemize{
#'   \item \code{Size} changes the size of the chart (and consequently the location
#'   of all its elements).
#'   \item \code{Title} changes the title of the chart.
#'   \item \code{Canvas} If number of elements is too high the 
#'   charts switch to the canvas mode and instead of multiple SVG point or cells
#'   a single Canvas image is generated. This type of update redraws the Canvas
#'   image. \emph{It is not recommended to use this function.}
#' }
#' 
#' These can be updated only in heatmaps (\code{\link{lc_heatmap}}):
#' \itemize{
#'   \item \code{Labels} adds new row and column labels and removes those that are no longer
#'   needed. Also updates \code{Cells}.
#'   \item \code{Cells} adds new cells and removes those that are no longer needed.
#'   Also updates \code{Texts} if necessary.
#'   \item \code{Texts} adds or remove text inside cells where needed.
#'   \item \code{LabelPosition} updates coordinates of all existing row and column labels.
#'   Also updates \code{CellPosition}.
#'   \item \code{CellPosition} updates coordinates of all existing cells. Also 
#'   updates \code{TextPosition} if necessary.
#'   \item \code{LabelText} updates text of all existing labels.
#'   \item \code{CellColour} updates colour of all existing cells. Also
#'   updates \code{TextValues} if necessary.
#'   \item \code{TextValues} updates text inside cells to make it up to date with current
#'   data values.
#' }
#' 
#' These aspects are present in all the charts with axes.
#' \itemize{
#'   \item \code{Axes} updates axes of the chart and changes positions 
#'   of all the elements accordingly.
#'   \item \code{Elements} updates (add or removes) all the elements of the layer.
#'   \item \code{ElementPosition} updates positions of all the elements in the layer.
#'   \item \code{ElementStyle} updates the style (colour, opacity, etc.) of all the elements 
#'   of the layer.
#' }
#' 
#' 
#' @param id An ID of the chart to be updated (or vector of IDs). If NULL then all the
#' existing charts will be updated.
#' 
#' @param layerId An ID of the layer to be updated (or vector of IDs). If NULL of the
#' layers of the selected charts will be updated. To update only the selected layers of
#' multiple charts the lengths of \code{id} and \code{layerId} must be the same.
#' 
#' @param updateOnly To improve performance it may be useful to change only certain 
#' aspects of the chart (e.g. location of the points, colour of the heatmap cells,
#' etc.). This argument can specify which part of chart to update. Possible options are
#' \code{Elements}, \code{ElementPosition}, \code{ElementStyle}, \code{Axes}, \code{Labels},
#' \code{Cells}, \code{Texts}, \code{LabelPosition}, \code{CellPosition}, 
#' \code{TextPosition}, \code{LabelText}, \code{CellColour}, \code{TextValues},
#' \code{Canvas}, \code{Size}. See details for more information.
#' 
#' @examples
#' \donttest{data(iris)
#'
#' #store some properties in global variables
#' width <- 300
#' height <- 300
#' colour <- iris$Sepal.Width
#' #create a chart
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length, colourValue = colour,
#'                width = width, height = height), id = "iris")
#' 
#' #change the variables
#' height <- 400
#' colour <- iris$Petal.Width
#' 
#' #this will change colour of points and chart height
#' updateCharts("iris")
#' #this will change only height
#' updateCharts("iris", updateOnly = "Size")
#' 
#' #add another property
#' setProperties(dat(symbolValue = iris$Species), "iris")
#' #this will change only colour and symbols
#' updateCharts("iris", updateOnly = "ElementStyle")}
#' 
#' @export
updateCharts <- function(id = NULL, layerId = NULL, updateOnly = NULL) {
  if(length(lc$charts) == 0) {
    warning("There are no charts yet.")
    return ();
  }
  
  if(is.null(id)) id <- ls(lc$charts)
  if(!is.vector(id))
    stop("'id' should be a vector of IDs")

  
  if(!is.null(layerId) & length(layerId) != length(id))
    stop("Lengths of 'id' and 'layerId' differ")
  if(!is.null(updateOnly) & length(updateOnly) != length(id))
    stop("Lengths of 'id' and 'updateOnly' differ")
  
  for(i in 1:length(id)){
    chart <- getChart(id[i])
    if(is.null(chart))
      stop(str_c("Chart with ID ", id[i], " is not defined."))
    args <- str_c("'", id[i], "', '")
    
    if(!is.null(updateOnly)) {
      args <- str_c(args, updateOnly[i], "', '")
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
    sendCommand(str_interp("rlc.updateCharts(${args});"))
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
    
    if(!is.null(d$ticksX) & !is.vector(d$ticksX))
      d$ticksX <- t(d$ticksX)
    if(!is.null(d$ticksY) & !is.vector(d$ticksY))
      d$ticksY <- t(d$ticksY)
    
    if(!is.null(d$on_click)) {
      layer$on_click <- d$on_click
      d$on_click <- NULL
    }
    
    if(!is.null(d$on_marked)) {
      layer$on_marked <- d$on_marked
      d$maekedUpdated <- NULL
      sendCommand(str_interp("rlc.setCustomOnMarked('${chart$id}', '${layerName}');"))
    }
    
    if(!is.null(d$on_mouseover)) {
      layer$on_mouseover <- d$on_mouseover
      d$on_mouseover <- NULL
      sendCommand(str_interp("rlc.setCustomMouseOver('${chart$id}', '${layerName}', ${layer$pacerStep});"))
    }
    if(!is.null(d$on_mouseout)) {
      layer$on_mouseout <- d$on_mouseout
      d$on_mouseout <- NULL
      sendCommand(str_interp("rlc.setCustomMouseOut('${chart$id}', '${layerName}');"))
    }    
    if(!is.null(d$on_labelClickRow)) {
      layer$on_labelClickRow <- d$on_labelClickRow
      d$on_labelClickRow <- NULL
      sendCommand(str_interp("rlc.setCustomClickLabel('${chart$id}', 'Row');"))
    }    
    if(!is.null(d$on_labelClickCol)) {
      layer$on_labelClickCol <- d$on_labelClickCol
      d$on_labelClickCol <- NULL
      sendCommand(str_interp("rlc.setCustomClickLabel('${chart$id}', 'Col');"))
    }    
    
    name <- str_c(chart$id, layer$id, sep = "_")
    
    sendData(name, d)
    sendCommand(str_interp("rlc.setProperty('${name}')"))
  }
}

setChart <- function(.type, data, ..., place, id, layerId, dataFun, addLayer, pacerStep = 50) {
  if(!lc$pageOpened) openPage()
  
  if(is.null(place))
    place <- str_c("Chart", length(lc$charts) + 1)
  prepareContainer(place)
  
  if(is.null(id))
    id <- place
  
  chart <- getChart(id)
  if(!is.null(chart) && !is.null(layerId) && layerId == "main") {
    removeChart(chart$id)
    chart <- getChart(id)
  }
  
  if(is.null(chart)) {
    chart <- addChart(id, place)
    if(is.null(layerId) || layerId != "main")
      chart$addLayer("main", "axesChart")
  }

  if(is.null(layerId)){
    if(!addLayer & chart$nLayers() > 1) {
      warning(str_c("Chart '", id, "' has ", chart$nLayers(), " layers and the layer ID is not specified. ", 
                    "'addLayer' will be set to TRUE."))
      addLayer <- T
    }
    
    if(addLayer | chart$nLayers() == 0) {
      layerId <- str_c("Layer", (chart$nLayers() + 1))
    } else {
      layerId <- names(chart$layers)[2]
    }
  }
  
  if(!is.null(chart$getLayer(layerId)))
    chart$removeLayer(layerId)
  
  layer <- chart$addLayer(layerId, .type)
  layer$dataFun <- dataFun
  
  layer$pacerStep <- pacerStep
  
  chart$JSinitialize()
  
  l <- list(...)
  nonEv <- lapply(names(l), function(n) {function() l[[n]]})
  names(nonEv) <- names(l)
  setProperties(c(data, nonEv), id, layerId)
  updateCharts(id)
  
  invisible(chart)
}

#' Trigger an event
#' 
#' This function is called whenever user clicks, selects or hover over elements of a chart. In turn,
#' it calls a corresponding callback function, if any was specified by the user. This function
#' is meant to be used internally. However, an experienced user can still use it to customize app
#' behavior in some complicated cases. This function can also emulate events triggered by non-existing
#' elements.
#' 
#' @param d ID of an element that triggered the event. May be index of a point or line, vector or
#' row and column indices for a heatmap, value of an input block (please, check \code{\link{lc_input}}
#' for more details about values). Should be \code{NULL} for \code{mouseout} or \code{marked} events.
#' N.B. This function is called from the web page and therefore all element indices start from zero as it is
#' used in JavaScript.
#' @param id ID of the chart.
#' @param layerId ID of the layer. You can get IDs of all charts and their layers with \code{\link{listCharts}}.
#' @param event Type of event. Must be one of \code{"click", "mouseover", "mouseout", "marked", "labelClickRow", "labelClickCol"}.
#' 
#' @examples 
#' \donttest{x <- rnorm(50)
#' lc_scatter(x = x, y = 2*x + rnorm(50, 0.1), on_click = function(d) print(d))
#' chartEvent(51, "Chart1", "Layer1", "click")}
#' 
#' @export
#' @importFrom utils type.convert
chartEvent <- function(d, id, layerId = "main", event) {
  
  if(length(d) == 1)
    if(d == "NULL")
      d <- NULL
  
  #lame. This also must go to jrc with the nearest update
  d <- type.convert(d, as.is = TRUE)
  if(is.numeric(d)) d <- d + 1
  # should we move that to jrc? And add some parameter, like 'flatten'?
  if(is.list(d))
    if(all(sapply(d, function(el) length(el) == 1)))
      d <- unlist(d)
  
  chart <- getChart(id)
  if(is.null(chart))
    stop(str_interp("Chart with ID ${id} is not defined"))
  
  layer <- chart$getLayer(layerId)
  if(is.null(layer))
    stop(str_interp("Chart ${id} doesn't have layer ${layerId}"))

  if(event == "click")
    layer$on_click(d)
  if(event == "mouseover")
    layer$on_mouseover(d)
  if(event == "mouseout")
    layer$on_mouseout()
  if(event == "marked")
    layer$on_marked()
  if(event == "labelClickRow")
    layer$on_labelClickRow(d)
  if(event == "labelClickCol")
    layer$on_labelClickCol(d)
}

#' List all existing charts and layers
#' 
#' \code{listCharts} prints a list of IDs of all existing charts and layers.
#' 
#' @examples 
#' \donttest{noise <- rnorm(30)
#' x <- seq(-4, 4, length.out = 30)
#' 
#' lc_scatter(dat(x = x,
#'                y = sin(x) + noise,
#'                colourValue = noise), 
#'            id = "plot", layerId = "points")
#' lc_line(dat(x = x, y = sin(x)), id = "plot", addLayer = TRUE)
#' lc_colourSlider(chart = "plot", layer = "points")
#' 
#' listCharts()}
#' @export
listCharts <- function() {
  for(chartId in names(lc$charts)) {
    print(str_interp("Chart: ${chartId}"))
    chart <- getChart(chartId)
    if(chart$nLayers() > 1) {
      print("- Layers:")
      for(layerId in names(chart$layers)){
        layer <- chart$getLayer(layerId)
        if(layer$id != "main")
          print(str_interp("-- ${layer$id} - ${layer$type}"))
      }
    }
  }
}

#' Get currently marked elements
#' 
#' \code{getMarked} returns indices of the chart's elements that are currently
#' marked. To mark elements select them with you mouse while pressing the \emph{Shift} key.
#' Double click on the chart while pressing the \emph{Shift} key will unmark all the 
#' elements.
#' 
#' @param chartId An ID of the chart.
#' @param layerId An ID of the layer. This argument is required, if the chart has more
#' than one layer.
#' 
#' @return a vector of indices or, in case of heatmaps, an \emph{n x 2} matrix were first and
#' second columns contain, respectively, row and column indices of the marked cells.
#' 
#' @examples
#' \donttest{data(iris)
#' 
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length))
#' 
#' #now mark some points by selecting them with your mouse with Shift pressed
#' 
#' getMarked("Chart1")}
#' 
#' @export
getMarked <- function(chartId = NULL, layerId = NULL) {
  if(is.null(chartId))
    if(length(lc$charts)) {
      chartId <- lc$charts[[1]]$id
    } else {
      stop(str_c("There are more than one chart on the page. 'chartId' must be ",
                 "specified. Use 'listCharts()' to get IDs of all existing charts."))
    }
  
  chart <- getChart(chartId)
  if(is.null(chart))
    stop(str_c("Chart ", chartId, " is not defined."))
  
  if(is.null(layerId)) {
    if(chart$nLayers() == 0)
      layerId <- "main"
    if(chart$nLayers() == 1)
      layerId <- names(chart$layers)[2]
    if(chart$nLayers() > 1)
      stop(str_c("There is more than one layer in this chart. 'layerId' must be specified. ",
                 "Use 'listCharts()' to get IDs of all existing charts and their layers."))
  }
  marked <- NULL
  setEnvironment(environment()) 
  sendCommand(str_interp("rlc.getMarked('${chartId}', '${layerId}')"))
  for( i in 1:(5/0.05) ) {
    service()
    if(!is.null(marked)) {
      setEnvironment(globalenv())
      break
    } 
    Sys.sleep( .05 )
  }
  
  if( is.null(marked) ) {
    warning( "Can't load marked elements" )
  }
  
  if(is.numeric(marked)) marked <- marked + 1
  if(length(marked) == 0)
    return (c())
  
  marked
}

#' Mark elements of a chart
#' 
#' \code{mark} selects a set of elements in a given chart. It is equivalent to
#' selecting elements interactively by drawing a rectangle with the mouse 
#' while holding the \code{Shift} key.
#' 
#' @param elements numeric vector of indices of the elements to select.
#' @param chartId ID of the chart where to select elements (can be omitted if 
#' there is only one chart).
#' @param layerId ID of the layer where to select elements (can be omitted if
#' the chart has only one layer).
#' @param preventEvent if \code{TRUE}, \code{on_marked} function will not be
#' called.
#'
#' @examples 
#' \donttest{data("iris")
#' openPage(FALSE, layout = "table1x2")
#' 
#' #brushing example
#' #Hold Shift pressed and select a group of point on one of the charts
#' 
#' lc_scatter(dat(
#'   x = iris$Sepal.Length,
#'   y = iris$Petal.Length,
#'   colourValue = iris$Species,
#'   on_marked = function() {
#'     mark(getMarked("A1"), "A2")
#'   }
#' ), "A1")
#' 
#' lc_scatter(dat(
#'   x = iris$Sepal.Width,
#'   y = iris$Petal.Width,
#'   colourValue = iris$Species,
#'   on_marked = function() {
#'     mark(getMarked("A2"), "A1")
#'   }
#' ), "A2")}
#'
#' @export
mark <- function(elements, chartId = NULL, layerId = NULL, preventEvent = TRUE) {
  if(is.null(chartId))
    if(length(lc$charts)) {
      chartId <- lc$charts[[1]]$id
    } else {
      stop(str_c("There is more than one chart on the page. 'chartId' must be ",
                 "specified. Use 'listCharts()' to get IDs of all existing charts."))
    }
  
  chart <- getChart(chartId)
  if(is.null(chart))
    stop(str_c("Chart ", chartId, " is not defined."))
  
  if(is.null(layerId)) {
    if(chart$nLayers() == 0)
      layerId <- "main"
    if(chart$nLayers() == 1)
      layerId <- names(chart$layers)[2]
    if(chart$nLayers() > 1)
      stop(str_c("There is more than one layer in this chart. 'layerId' must be specified. ",
                 "Use 'listCharts()' to get IDs of all existing charts and their layers."))
  }
  
  if(length(elements) != 0 & !is.vector(elements))
    stop("'elements' must be a vector of indices.")
  if(preventEvent) {
    preventEvent = "true"
  } else {
    preventEvent = "false"
  }
  
  if(length(elements) == 0) {
    elements <- "__clear__"
  } else {
    elements <- elements - 1
  }
  
  sendData("markElements", elements)
  sendCommand(str_c("rlc.mark('", chartId, "', '", layerId, "', ", preventEvent, ");"))
}


#' Link data to the chart
#' 
#' \code{dat} allows to link variables from the current environment to chart's properties.
#' On every \code{\link{updateCharts}} call all the data, provided via the \code{dat} function,
#' will be automatically reevaluated and the chart will be changed accordingly. One can also
#' put properties outside of the \code{dat} function, if they are going to be constant.
#' 
#' @param ... List of name values pair to define the properties. 
#' 
#' @examples 
#' \donttest{lc_scatter(dat(x = rnorm(30)), y = rnorm(30))
#' #note that the Y values remain the same after each updateCharts call
#' updateCharts()}
#' 
#' @export
dat <- function( ... ) {
  e <- parent.frame()
  l <- match.call()[-1]
  lapply(l, function(el) {
    function() eval(el, e)
  })
}

#' Close page
#' 
#' Close an opened web page and clear the list of charts.
#' 
#' @examples 
#' \donttest{openPage(useViewer = FALSE)
#' closePage()}
#' 
#' @export
closePage <- function() {
  lc$charts <- list()
  lc$pageOpened <- F
  jrc::closePage()
}

#' @importFrom stats runif
scatterDataFun <- function(l) {
  if(is.null(l$x) && is.null(l$y))
    stop("Required properties 'x' and 'y' are not defined.")
  
  if(is.factor(l$x))
    l$layerDomainX <- levels(l$x)
  if(is.factor(l$y))
    l$layerDomainY <- levels(l$y)
    
  if(is.null(l$x))
    l$x <- 1:length(l$y)
  if(is.null(l$y))
    l$y <- 1:length(l$x)
  
  if(is.null(l$label)){
    if(!is.null(names(l$y)))
      l$label <- names(l$y)
    if(!is.null(names(l$x)))
      l$label <- names(l$x)
  }
  
  if(!is.null(l$jitterX)) {
    if(!is.numeric(l$jitterX))
      stop("'jitterX' must be a number")
    l$jitterX = abs(l$jitterX)
    
    l$shiftX <- runif(length(l$x), -l$jitterX, l$jitterX)
    l$jitterX <- NULL
  }
  if(!is.null(l$jitterY)) {
    if(!is.numeric(l$jitterY))
      stop("'jitterX' must be a number")
    l$jitterY = abs(l$jitterY)
    
    l$shiftY <- runif(length(l$y), -l$jitterY, l$jitterY)
    l$jitterY <- NULL
  }
  
  if(lc$useViewer)
    l$mode <- "svg"

  l
}

#' Visualize a set of points
#' 
#' These functions plot a set of points with known coordinates that can be either categorical,
#' or continuous. 
#' 
#' @describeIn lc_scatter creates a scatterplot and adds it as a new layer to an existing chart or
#' creates a new one.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param addLayer whether to add a new layer or to replace the existing one. This argument influences the chart only if
#' it has only one layer and the \code{layerId} is not defined. 
#' @param pacerStep Time in ms between two consecutive calls of an \code{onmouseover} event. Prevents overqueuing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{x, y} - vector of x and y coordinates of the points.
#'  \item \code{size} - sizes of the points. Default size is 6.
#'  \item \code{opacity} - opacity of the points in the range from 0 to 1.
#'  \item \code{label} - vector of text labels for each point.
#'  \item \code{valueAxis} - (for \code{lc_beeswarm} only) defines, values along 
#'  which of the axes should not be changed. Must be \code{"x"} or \code{"y"}.} 
#' 
#' Colour and shape settings
#' \itemize{
#'  \item \code{colour} - colour of the points. Must be a colour name or hexadecimal code.
#'  \item \code{colourValue} - grouping values for different colours. Can be numbers or characters.
#'  \item \code{colourDomain} - vector of all possible values for discrete colour scales 
#'  or range of all possible colour values for the continuous ones.
#'  \item \code{palette} - vector of colours to construct the colour scale.
#'  \item \code{colourLegendTitle} - title for the colour legend.
#'  \item \code{addColourScaleToLegend} - whether or not to show colour legend for the current layer.
#'  \item \code{globalColourScale} - whether or not to use one colour scale for all the layers.
#'  \item \code{symbol} - shape of each point. Must be one of \code{"Circle", "Cross", "Diamond", 
#'  "Square", "Star", "Triangle", "Wye"}.
#'  \item \code{symbolValue} - grouping values for different symbols.
#'  \item \code{symbolLegendTitle} - title for the symbol value.
#'  \item \code{stroke} - stroke colour for each element. Must be a colour name or hexadecimal code.
#'  \item \code{strokeWidth} - width of the strokes for each point.} 
#'  
#' Axes settings
#' \itemize{
#'  \item \code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.
#'  \item \code{jitterX, jitterY} - amount of random variation to be added to the position of the
#'  points along one of the axes. 0 means no variation. 1 stands for distance between \code{x} and
#'  \code{x + 1} for linear scale, \code{x} and \code{b*x} for logarithmic scale (\code{b} is a base
#'  of the logarithm), and between neighbouring ticks for categorical scale.
#'  \item \code{shiftX, shiftY} - shift for each point from its original position along one of the
#'  axes. 0 means no shift. 1 stands for distance between \code{x} and
#'  \code{x + 1} for linear scale, \code{x} and \code{b*x} for logarithmic scale (\code{b} is a base
#'  of the logarithm), and between neighbouring ticks for categorical scale.
#'  \item \code{layerDomainX, layerDomainY} - default axes ranges for the given layer.
#'  \item \code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  is automatically set to include all layer domains.
#'  \item \code{contScaleX, consScaleY} - whether or not the axis should be continuous.
#'  \item \code{aspectRatio} - aspect ratio.
#'  \item \code{axisTitleX, axisTitleY} - axes titles.
#'  \item \code{ticksRotateX, ticksRotateY} - degrees of angle to rotate ticks. Must be between 
#'  0 (horizontal ticks, default) and 90 (vertical ticks).
#'  \item \code{ticksX, ticksY} - set of ticks for the axes.} 
#'
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - function, to be called, when one of the points is clicked. Gets an
#'  index of the clicked point as an argument.
#'  \item \code{on_mouseover} - function, to be called, when mouse hovers over one of the points.
#'  Gets an index of the clicked point as an argument.
#'  \item \code{on_mouseout} - function, to be called, when mouse moves out of one of the points.
#'  \item \code{on_marked} - function, to be called, when any of the points are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked points.} 
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels. 
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend.
#'  \item \code{showPanel} - whether of not to show the tools panel.
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.} 
#' 
#' @examples
#' \donttest{data("iris")
#' lc_scatter(dat(x = iris$Sepal.Length, 
#'                y = iris$Petal.Length,
#'                colourValue = iris$Petal.Width,
#'                symbolValue = iris$Species),
#'            title = "Iris dataset",
#'            axisTitleY = "Petal Length",
#'            axisTitleX = "Sepal Length",
#'            colourLegendTitle = "Petal Width",
#'            symbolLegendTitle = "Species")
#' 
#' lc_beeswarm(dat(x = iris$Species,
#'                 y = iris$Sepal.Length,
#'                 colourValue = iris$Sepal.Width),
#'             title = "Iris dataset",
#'             axisTitleY = "Sepal Length",
#'             axisTitleX = "Species",
#'             colourLegendTitle = "Sepal Width")}
#' @export
lc_scatter <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE, pacerStep = 50) {
  setChart("scatter", data, ...,  place = place, id = id, layerId = layerId, dataFun = scatterDataFun, addLayer = addLayer,
           pacerStep = pacerStep)
}

#' @describeIn lc_scatter creates a special kind of scatterplot, where the points are spread along one of 
#' the axes to avoid overlapping.
#' 
#' @export
lc_beeswarm <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE, pacerStep = 50) {
  setChart("beeswarm", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
    if(is.null(l$x) || is.null(l$y))
      stop("Required properties 'x' and 'y' are not defined.")

    if(is.factor(l$x))
      l$layerDomainX <- levels(l$x)
    if(is.factor(l$y))
      l$layerDomainY <- levels(l$y)    
  
    if(is.null(l$label)){
      if(!is.null(names(l$y)))
        l$label <- names(l$y)
      if(!is.null(names(l$x)))
        l$label <- names(l$x)
    }
    
    if(lc$useViewer)
      l$mode <- "svg"    
    
    l   
  }, pacerStep = pacerStep)
}

#TO DO: Add grouping
lineDataFun <- function(l) {
  if(is.null(l$x) && is.null(l$y))
    stop("Required properties 'x' and 'y' are missing.")
  
  if(is.factor(l$x))
    l$layerDomainX <- levels(l$x)
  if(is.factor(l$y))
    l$layerDomainY <- levels(l$y)
  
  if(!is.null(l$x)) l$x <- as.matrix(l$x)
  if(!is.null(l$y)) l$y <- as.matrix(l$y)

  if(is.null(l$y))
    l$y <- matrix(rep(1:nrow(l$x), ncol(l$x)), nrow = ncol(l$x))
  if(is.null(l$x))
    l$x <- matrix(rep(1:nrow(l$y), ncol(l$y)), nrow = ncol(l$y))

  if(!is.matrix(l$x) || !is.matrix(l$y))
    stop("One of the properties 'x' or 'y' can not be converted into matrix")

  if(nrow(l$x) != nrow(l$y))
    stop("Lengths of 'x' and 'y' differ.")
  
  if(ncol(l$x) != ncol(l$y)){
    if(ncol(l$x) == 1)
      l$x <- matrix(rep(l$x, ncol(l$y)), ncol = ncol(l$y))
    if(ncol(l$y) == 1)
      l$y <- matrix(rep(l$y, ncol(l$x)), ncol = ncol(l$x))
    if(ncol(l$x) != ncol(l$y))
      stop("'x' and 'y' define different number of lines.")
  }
    
  if(!is.null(l$x)) {
    l$nelements <- ncol(l$x)
    l$nsteps <- nrow(l$x)      
  }
  
  l
}

#' Lines and ribbons
#' 
#' These functions create different kind of lines. They connect observations or 
#' create filled area, bordered by a line. Each layer may have one or several lines.
#' 
#' @describeIn lc_line connects points in the order of variables on the x axis.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param addLayer whether to add a new layer or to replace the existing one. This argument influences the chart only if
#' it has only one layer and the \code{layerId} is not defined. 
#' 
#' @section Available properties: 
#' You can read more about different properties
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{x, y} - vector of x and y coordinates of the points to connect. Can be 
#'  vectors for a single line or \code{m x n} matrix for \code{n} lines.
#'  \item \code{ymax, ymin} - (only for \code{lc_ribbon}) vectors of maximal and minimal values of the ribbon.
#'  \item \code{a, b} - (only for \code{lc_abLine}) vectors of slope and intercept values respectively.
#'  \item \code{v} - (only for \code{lc_vLine}) vector of x-intercepts.
#'  \item \code{h} - (only for \code{lc_hLine}) vector of y-intercepts.
#'  \item \code{lineWidth} - width of each line.
#'  \item \code{opacity} - opacity of the lines in the range from 0 to 1.
#'  \item \code{label} - vector of text labels for each line.
#'  \item \code{dasharray} - defines pattern of dashes and gaps for each line. }
#' 
#' Colour settings
#' \itemize{
#'  \item \code{colour} - colour of the lines. Must be a colour name or hexadecimal code. For
#'  \code{lc_ribbon} this property defined the colour of the ribbon, not the strokes.
#'  \item \code{fill} - colour with which to fill area inside the line. 
#'  Must be a colour name or hexadecimal code.
#'  \item \code{colourValue} - grouping values for different colours. Can be numbers or characters.
#'  \item \code{colourDomain} - vector of all possible values for discrete colour scales 
#'  or range of all possible colour values for the continuous ones.
#'  \item \code{palette} - vector of colours to construct the colour scale.
#'  \item \code{colourLegendTitle} - title for the colour legend.
#'  \item \code{addColourScaleToLegend} - whether or not to show colour legend for the current layer.
#'  \item \code{globalColourScale} - whether or not to use one colour scale for all the layers.
#'  \item \code{stroke} - (only for \code{lc_ribbon}) stroke colour for each ribbon. 
#'  Must be a colour name or hexadecimal code.
#'  \item \code{strokeWidth} - (only for \code{lc_ribbon}) width of the strokes for each ribbon. }
#'  
#' Axes settings
#' \itemize{
#'  \item \code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.
#'  \item \code{layerDomainX, layerDomainY} - default axes ranges for the given layer.
#'  \item \code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  is automatically set to include all layer domains.
#'  \item \code{contScaleX, consScaleY} - whether or not the axis should be continuous.
#'  \item \code{aspectRatio} - aspect ratio.
#'  \item \code{axisTitleX, axisTitleY} - axes titles.
#'  \item \code{ticksRotateX, ticksRotateY} - degrees of angle to rotate ticks. Must be between 
#'  0 (horizontal ticks, default) and 90 (vertical ticks).
#'  \item \code{ticksX, ticksY} - set of ticks for the axes.} 
#'
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - function, to be called, when one of the lines is clicked. Gets an
#'  index of the clicked line as an argument.
#'  \item \code{on_mouseover} - function, to be called, when mouse hovers over one of the lines.
#'  Gets an index of the clicked line as an argument.
#'  \item \code{on_mouseout} - function, to be called, when mouse moves out of one of the lines.
#'  \item \code{on_marked} - function, to be called, when any of the lines are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked lines.} 
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend.
#'  \item \code{showPanel} - whether of not to show the tools panel.
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.
#' } 
#' 
#' @examples 
#' \donttest{x <- seq(0, 8, 0.2)
#' lc_line(dat(x = x, y = cbind(cos(x), sin(x)),
#'             aspectRatio = 1,
#'             colour = c("blue", "red"),
#'             dasharray = c("5", "1 5 5")))
#'             
#' points <- seq(0, 6.5, 0.1)
#' x <- cos(points)
#' y <- sin(points)
#' lc_path(dat(x = sapply(0:2, function(i) x + i), 
#'             y = sapply(0:2, function(i) y + i),
#'             fill = c("blue", "red", "black"),
#'             opacity = c(0.3, 0.5, 0.7)))
#'             
#' x <- seq(0, 5, 0.1)
#' y <- x*3 + rnorm(length(x), sd = 2)
#' fit <- lm(y ~ x)
#' pred <- predict(fit, data.frame(x = x), se.fit = TRUE)
#' lc_ribbon(dat(ymin = pred$fit - 1.96 * pred$se.fit,
#'               ymax = pred$fit + 1.96 * pred$se.fit,
#'               x = x,
#'               colour = "#555555"), id = "ribbonTest")
#' lc_scatter(dat(x = x, y = y), size = 2, id = "ribbonTest", addLayer = TRUE)
#' lc_abLine(dat(a = fit$coefficients[2], b = fit$coefficients[1]), id = "ribbonTest", addLayer = TRUE)
#' 
#' lc_hLine(dat(h = seq(1, 9, 1), domainX = c(0, 10), domainY = c(0, 10)), id = "grid")
#' lc_vLine(dat(v = seq(1, 9, 1)), id = "grid", addLayer = TRUE)}
#' 
#' @export
lc_line <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("pointLine", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer,
           dataFun = function(l) {
             l <- lineDataFun(l)
             
             for(i in 1:ncol(l$x)) {
               l$y[, i] <- l$y[order(l$x[, i]), i]
               l$x[, i] <- l$x[order(l$x[, i]), i]
             }
             
             l
           })
}

#' @describeIn lc_line connects points in the order they are given.
#' @export
lc_path <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("pointLine", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = lineDataFun)
}

#' @describeIn lc_line displays a filled area, defined by \code{ymax} and \code{ymin} values.
#' @export
lc_ribbon <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("pointRibbon", data, ...,  place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
    if(is.factor(l$x))
      l$layerDomainX <- levels(l$x)
    if(is.factor(l$ymax))
      l$layerDomainY <- levels(l$ymax)
    if(is.factor(l$ymin))
      l$layerDomainY <- levels(l$ymin)
    
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
  
  if(all(is.null(l$groupIds), !is.null(l$barIds), is.null(l$stackIds))) {
    l$groupIds <- l$barIds
    l$barIds <- NULL
  }
  if(is.null(l$groupIds) &is.null(l$barIds))
    if(is.null(names(l$value))) {
      l$groupIds <- 1:length(l$value)  
    } else {
      l$groupIds <- names(l$value)
    }
  
  if(is.null(l$barIds) & is.null(l$stackIds))
    l$addColourScaleToLegend <- FALSE
  
  if(is.null(l$groupIds)) l$groupIds <- rep("group", length(l$value))
  if(is.null(l$barIds)) l$barIds <- rep(1, length(l$value))
  if(is.null(l$stackIds)) l$stackIds <- rep(1, length(l$value))
  
  ngroups <- length(unique(l$groupIds))
  nbars <- length(unique(l$barIds))
  nstacks <- length(unique(l$stackIds))
  
  if(is.factor(l$groupIds))
    l$layerDomainX <- levels(l$groupIds)
  
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

#' Create a barplot
#' 
#' \code{lc_bars} creates a new barplot and adds it on the page
#' as a new chart or as a new layer of an existing chart.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param addLayer whether to add a new layer or to replace the existing one. This argument influences the chart only if
#' it has only one layer and the \code{layerId} is not defined. 
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{value} - heights of bars/stacks.
#'  \item \code{stackIds} - IDs for all stacks (if necessary). Must be the same size as \code{values}.
#'  \item \code{barIds} - IDs for all bars (if necessary). Must be the same size as \code{values}.
#'  \item \code{groupIds} - IDs for all groups (if necessary). Must be the same size as \code{values}.
#'  \item \code{groupWidth} - ratio of width of a group of bars to the space, available to the group. }
#' 
#' Style settings
#' \itemize{
#'  \item \code{opacity} - opacity of each bar|stack in the range from 0 to 1.
#'  \item \code{colour} - colour of each bar|stack. Must be a colour name or hexadecimal code.
#'  \item \code{colourValue} - grouping values for different colours. Can be numbers or characters.
#'  \item \code{colourDomain} - vector of all possible values for discrete colour scales 
#'  or range of all possible colour values for the continuous ones.
#'  \item \code{palette} - vector of colours to construct the colour scale.
#'  \item \code{colourLegendTitle} - title for the colour legend.
#'  \item \code{addColourScaleToLegend} - whether or not to show colour legend for the current layer.
#'  \item \code{globalColourScale} - whether or not to use one colour scale for all the layers.
#'  \item \code{stroke} -  stroke colour of each bar|stack. Must be a colour name or hexadecimal code.
#'  \item \code{strokeWidth} - width of the strokes of each bar|stack. }
#'  
#' Axes settings
#' \itemize{
#'  \item \code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.
#'  \item \code{layerDomainX, layerDomainY} - default axes ranges for the given layer.
#'  \item \code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  is automatically set to include all layer domains.
#'  \item \code{contScaleX, consScaleY} - whether or not the axis should be continuous.
#'  \item \code{aspectRatio} - aspect ratio.
#'  \item \code{axisTitleX, axisTitleY} - axes titles.
#'  \item \code{ticksRotateX, ticksRotateY} - degrees of angle to rotate ticks. Must be between 
#'  0 (horizontal ticks, default) and 90 (vertical ticks).
#'  \item \code{ticksX, ticksY} - set of ticks for the axes.}
#'
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - function, to be called, when one of the bars is clicked. Gets an
#'  index of the clicked bar as an argument.
#'  \item \code{on_mouseover} - function, to be called, when mouse hovers over one of the bars.
#'  Gets an index of the clicked bar as an argument.
#'  \item \code{on_mouseout} - function, to be called, when mouse moves out of one of the bars.
#'  \item \code{on_marked} - function, to be called, when any of the bars are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked bars.} 
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend. 
#'  \item \code{showPanel} - whether of not to show the tools panel.
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.} 
#'   
#' @examples 
#' \donttest{data("esoph")
#' 
#' lc_bars(dat(value = tapply(esoph$ncases, esoph$agegp, sum), 
#'             title = "Number of cases per age group",
#'             axisTitleX = "Age group", 
#'             axisTitleY = "Number of esophageal cases"))
#' 
#' lc_bars(dat(value = c(tapply(esoph$ncases, esoph$agegp, sum), 
#'                       tapply(esoph$ncontrols, esoph$agegp, sum)),
#'             stackIds = c(rep("case", 6), rep("control", 6))))
#' 
#' #It is easy to put data in a convenient form for barplots using tidyverse
#' library(tidyverse)
#' 
#' esoph %>%
#'   gather(type, cases, (ncases:ncontrols)) %>%
#'   mutate(type = str_sub(type, 2, -2)) %>%
#'   group_by(agegp, alcgp, type) %>%
#'   summarise(ncases = sum(cases)) -> newData
#' 
#' lc_bars(dat(value = newData$ncases,
#'             stackIds = newData$type,
#'             barIds = newData$alcgp,
#'             groupIds = newData$agegp))}
#' 
#' @export
lc_bars <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("barchart", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = barDataFun)
}

#' Histograms and density plots
#' 
#' These functions make either a histogram or a density plot of the given data 
#' and either add them as a new layer to an existing chart or create a new chart.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param addLayer whether to add a new layer or to replace the existing one. This argument influences the chart only if
#' it has only one layer and the \code{layerId} is not defined. 
#' 
#' @section Available properties:
#' 
#' \itemize{
#'  \item \code{value} - vector of data. 
#'  \item \code{nbins} - (only for \code{lc_hist}) number of bins.} 
#' 
#' These functions are extensions of \code{\link{lc_line}} (\code{lc_dens}) or \code{\link{lc_bars}} 
#' (\code{lc_hist}) and therefore can also understand their properties.
#' 
#' @describeIn lc_hist makes a histogram. It is an extension of \code{\link{lc_bars}}.
#' 
#' @examples
#' \donttest{
#' lc_hist(dat(value = rnorm(1000), nbins = 30, height = 300))
#' lc_dens(dat(value = rnorm(1000), height = 300)) }
#' 
#' @export 
lc_hist <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  # has a nbins property. Not implemented in JS
  setChart("barchart", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
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
      minV <- min(l$value, na.rm = TRUE)
      maxV <- max(l$value, na.rm = TRUE)
      breaks <- seq(minV, maxV, length.out = nbins + 1)
      step <- breaks[2] - breaks[1]
      groupIds <- breaks - step/2
      groupIds <- groupIds[-1]
      binned <- .bincode(l$value, breaks, include.lowest = TRUE)
      value <- sapply(1:nbins, function(i) {sum(binned == i)})
      
      l$groupIds <- groupIds
      l$value <- value
    }
    barDataFun(l)
  })
}

#' @describeIn lc_hist makes a density plot. Is an extension of \code{\link{lc_line}}.
#' @export
#' @importFrom stats density.default
lc_dens <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("pointLine", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
    if(!is.null(l$value)) {
      dens <- density.default(l$value)
      l$x <- dens$x
      l$y <- dens$y
      l$value <- NULL
    }
    lineDataFun(l)
  })
}

#' Create a heatmap
#' 
#' \code{lc_heatmap} creates a new heatmaps. Unlike charts with axes, heatmaps do not have
#' any layers.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param pacerStep Time in ms between two consecutive calls of an \code{onmouseover} event. Prevents overqueuing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{value} - matrix of values.
#'  \item \code{rowLabel, colLabel} - vector of labels for all rows or columns.
#'  \item \code{showDendogramRow, showDendogramCol} - whether to show dendograms when rows or columns are
#'  clustered. Even if these properties are set to \code{FALSE}, rows and columns can still be clustered. 
#'  \item \code{clusterRows, clusterCols} - whether rows or columns should be clustered. If these
#'  properties are set to \code{FALSE}, rows and columns can still be clustered later using the instrument
#'  panel.
#'  \item \code{mode} - one of \code{"default", "svg", "canvas"}. Defines, whether to display heatmap as
#'  an SVG or Canvas object. \code{"default"} mode switches between the two, turning heatmap into Canvas 
#'  image, when there are too many cell, and into SVG object otherwise.
#'  \item \code{heatmapRow, heatmapCol} - default order of rows and columns of the heatmap.
#'  \item \code{showValue} - if \code{TRUE}, than in the values will be shown as text in each cell.} 
#' 
#' Style settings
#' \itemize{
#'  \item \code{rowTitle, colTilte} - titles of rows and columns.
#'  \item \code{palette} - vector of colours to construct the colour scale.
#'  \item \code{colourDomain} - domain of the colour scale. All values outside it will
#'  be clamped to its edges.} 
#'  
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - function, to be called, when one of the cells is clicked. Gets row and column indices 
#'  of the clicked cell as its arguments.
#'  \item \code{on_mouseover} - function, to be called, when mouse hovers over one of the cells.
#'  Gets row and column indices of the clicked cell as its arguments.
#'  \item \code{on_mouseout} - function, to be called, when mouse moves out of one of the cells.
#'  \item \code{on_marked} - function, to be called, when any of the cells are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked cells.} 
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend.
#'  \item \code{showPanel} - whether of not to show the tools panel.
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.} 
#' @examples 
#' \donttest{
#' library(RColorBrewer)
#' #create a test matrix
#' test <- cbind(sapply(1:10, function(i) c(rnorm(10, mean = 1, sd = 3), 
#'                                          rnorm(6, mean = 5, sd = 2), 
#'                                          runif(14, 0, 8))),
#'               sapply(1:10, function(i) c(rnorm(10, mean = 3, sd = 2), 
#'                                          rnorm(6, mean = 1, sd = 2), 
#'                                          runif(14, 0, 8))))
#' test[test < 0] <- 0
#' rownames(test) <- paste0("Gene", 1:30)
#' colnames(test) <- paste0("Sample", 1:20)
#' 
#' lc_heatmap(dat(value = test))
#' 
#' # when you want to cluster rows or columns, it can be
#' # a good idea to make bottom and right paddings larger to
#' # fit labels
#' lc_heatmap(dat(value = test),
#'            clusterRows = TRUE,
#'            clusterCols = TRUE,
#'            paddings = list(top = 50, left = 30, bottom = 75, right = 75))
#' 
#' lc_heatmap(dat(value = cor(test), 
#'                colourDomain = c(-1, 1),
#'                palette = brewer.pal(11, "RdYlBu")))}
#' @export
lc_heatmap <- function(data = list(), place = NULL, ..., id = NULL, pacerStep = 50) {
  setChart("heatmap", data, ..., place = place, id = id, layerId = "main", dataFun = function(l) {
    if(!is.null(l$value)) {
      l$nrows <- nrow(l$value)
      l$ncols <- ncol(l$value)
      
      l$value <- as.matrix(l$value)
      
      if(is.null(l$rowLabel) & !is.null(rownames(l$value)))
        l$rowLabel <- rownames(l$value)
      if(is.null(l$colLabel) & !is.null(colnames(l$value)))
        l$colLabel <- colnames(l$value)
    }
    
    if(lc$useViewer)
      l$mode <- "svg"
    
    l
  }, pacerStep = pacerStep)
}

#' Add a colour slider
#' 
#' Colour slider provides an easy way to change any continuous colour scale
#' interactively. If your chart uses a continuous colour scale, you can just 
#' link a colour slider and it will be automatically synchronized with your
#' chart's colour scale.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' 
#' @section Available properties: 
#' You can read more about different properties
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{chart} - id of the chart whose colour scale should be linked to the colour slider.
#'  \item \code{layer} - id of the layer whose colour scale should be linked to the colour slider.
#'  If chart has only one layer, this property can be omitted.}
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.} 
#' 
#' @examples 
#' \donttest{data("iris")
#' lc_scatter(dat(x = iris$Sepal.Length, 
#'                y = iris$Petal.Length,
#'                colourValue = iris$Petal.Width,
#'                symbolValue = iris$Species),
#'            title = "Iris dataset",
#'            axisTitleY = "Petal Length",
#'            axisTitleX = "Sepal Length",
#'            colourLegendTitle = "Petal Width",
#'            symbolLegendTitle = "Species",
#'            showLegend = FALSE,
#'            id = "scatter")
#' 
#' lc_colourSlider(chart = "scatter")}
#' 
#' @export
lc_colourSlider <- function(data = list(), place = NULL, ..., id = NULL) {
  setChart("colourSlider", data, ..., place = place, id = id, layerId = "main", dataFun = function(l) {
    if(!is.null(l$chart)) {
      l$linkedChart <- str_c("charts.", l$chart)
      if(is.null(l$layer) && getChart(l$chart)$nLayers() == 1) 
        l$layer <- names(getChart(l$chart)$layers)[2]
      if(!is.null(l$layer))
        l$linkedChart <- str_c(l$linkedChart, ".layers.", l$layer)
      
    }
    l$chart <- NULL
    l$layer <- NULL
    l
  })
}

#' @describeIn lc_line creates straight lines by intercept and slope values
#' @export
#' @importFrom jsonlite toJSON
lc_abLine <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("xLine", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
    if(is.null(l$a) || is.null(l$b))
      stop("Required properties 'a' and 'b' are not defined.");
    if(length(l$a) != length(l$b))
      stop("Lengths of 'a' and 'b' differ.")
    
    l$a <- as.vector(l$a)
    l$b <- as.vector(l$b)
    
    l$nelements <- length(l$a)
    l$lineFun <- str_c("function(x, d) { a = ", toJSON(l$a, digits = NA), ";",
                       "b = ", toJSON(l$b, digits = NA), ";", 
                       "return a[d] * x + b[d]; }")
    l$a <- NULL
    l$b <- NULL
    
    l
  })
}

#' @describeIn lc_line creates horizontal lines by y-intercept values
#' @export
lc_hLine <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("xLine", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
    if(is.null(l$h))
      stop("Required property 'h' is not defined.");

    l$h <- as.vector(l$h)

    l$nelements <- length(l$h)
    l$lineFun <- str_c("function(x, d) { h = ", toJSON(l$h, digits = NA), ";",
                         "return h[d]; }")
    l$h <- NULL

    l
  })
}

#' @describeIn lc_line creates vertical lines by x-intercept values
#' @export
lc_vLine <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, addLayer = FALSE) {
  setChart("yLine", data, ..., place = place, id = id, layerId = layerId, addLayer = addLayer, dataFun = function(l) {
    if(is.null(l$v))
      stop("Required property 'v' is not defined.");
    
    l$v <- as.vector(l$v)
      
    l$nelements <- length(l$v)
    l$lineFun <- str_c("function(x, d) { v = ", toJSON(l$v, digits = NA), ";",
                       "return v[d]; }")
    l$v <- NULL
        
    l
  })
}

#' Add HTML code to the page
#' 
#' \code{lc_html} adds a block with HTML code. It uses \code{\link[hwriter]{hwrite}} function
#' to transform some data structures (e.g. data frames) to HTML tables.
#'
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#'
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{content} - HTML code to display on the page. Can also be a vector, data.frame or
#'  any other structure, that can be transformed by \code{\link[hwriter]{hwrite}}. } 
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels. By default, the entire content will be displayed.
#'  If width is defined and it's smaller than content's width, scrolling will be possible.
#'  \item \code{heigth} - height of the chart in pixels. By default, the entire content will be displayed.
#'  If height is defined and it's smaller than content's height, scrolling will be possible.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}. }
#'  
#' @examples
#' \donttest{lc_html(content = "Some <b>HTML</b> <br> <i>code</i>.")
#' lc_html(dat(content = matrix(1:12, nrow = 4)))
#' data(iris)
#' lc_html(content = iris, height = 200)}
#' 
#' @export
#' @importFrom hwriter hwrite
lc_html <- function(data = list(), place = NULL, ..., id = NULL) {
  setChart("html", data, ..., place = place, id = id, layerId = "main", dataFun = function(l) {
    if(!is.character(l$content) || length(l$content) != 1)
      l$content <- hwrite(l$content)
    
    #l$content <- gsub("[\r\n]", "", l$content)
    #l$content <- str_replace_all(l$content, "(\\W)", "\\\\\\1")
    
    l
  })
}

#' Add input forms to the page
#'
#' \code{lc_input} adds an input form. This function is an rlc wrapper for an
#' HTML \code{<input>} tag. Five types of input are supported: \code{"text", "range", "checkbox", "radio" and "button"}.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{type} - type of input. Must be one of \code{"text", "range", "checkbox", "radio", "button"}
#'  \item \code{value} - current state of the input block. For radio buttons it is an idex of the checked
#'  button. For checkboxes - a vector of \code{TRUE} (for each checked box) and \code{FALSE} (for each unchecked one),
#'  for ranges and textfiels - a vector of values for each text field or slider.
#'  \item \code{step} (only for \code{type = "range"}) - stepping interval for values that can be selected with a slider.
#'  Must be a numeric vector with one value for each slider in the input block.
#'  \item \code{min, max} (only for \code{type = "range"}) - minimal and maximal values that can be selected with a slider.
#'  Must be a numeric vector with one value for each slider in the input block.
#'  }
#'  
#' Interactivity settings
#' \itemize{
#'   \item \code{on_click, on_change} - function, to be called, when user clicks on a button, enters text in a text field
#'   or moves a slider. The two properties are complete synonymes and can replace one another.
#' }
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{title} - title of the input block.
#'  \item \code{width} - width of the chart in pixels. By default, the entire content will be displayed.
#'  If width is defined and it's smaller than content's width, scrolling will be possible.
#'  \item \code{heigth} - height of the chart in pixels. By default, the entire content will be displayed.
#'  If height is defined and it's smaller than content's height, scrolling will be possible.
#'  \item \code{paddings} - paddings size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'
#'@examples
#' \donttest{lc_input(type = "checkbox", labels = paste0("el", 1:5), on_click = function(value) print(value),
#' value = TRUE)
#' lc_input(type = "radio", labels = paste0("el", 1:5), on_click = function(value) print(value),
#'          value = 1)
#' lc_input(type = "text", labels = paste0("el", 1:5), on_click = function(value) print(value),
#'          value = c("a", "b", "c", "e", "d"))
#' lc_input(type = "range", labels = paste0("el", 1:5), on_click = function(value) print(value),
#'          value = 10, max = c(10, 20, 30, 40, 50), step = c(0.5, 0.1, 1, 5, 25))
#' lc_input(type = "button", labels = paste0("el", 1:5), on_click = function(value) print(value))}
#'
#' @export
lc_input <- function(data = list(), place = NULL, ..., id = NULL) {
  setChart("input", data, ..., place = place, id = id, layerId = "main", dataFun = function(l){
    if(!(l$type %in% c("checkbox", "radio", "text", "button", "range")))
       stop("Unsupported type of input. Please, use one of \"checkbox\", \"radio\", \"text\", \"button\", \"range\"")
    
    if(!is.null(l$label)) {
      l$nelements <- length(l$label)
    } else {
      l$nelements <- 1
    }
    
    if(l$type == "checkbox")
      if(!is.null(l$values))
        if(length(l$value) != 1 & length(l$value) != l$nelements)
          stop("Length of 'values' vector must be either 1 or equal to the number of checkboxes.")
    
    if(l$type == "radio")
      if(!is.null(l$value))
        if(length(l$value) != 1 | !is.numeric(l$value)) {
          stop("'value' must be a number of the checked radio button")
        } else {
          l$value = l$value - 1
        }
          
    
    if(l$type == "text")
      if(!is.null(l$value) & length(l$value) != l$nelements)
        stop("Length of 'values' must be equal to the number of text fields.")
    
    if(l$type == "range") {
      if(!is.null(l$value) & length(l$value) != 1 & length(l$value) != l$nelements)
        stop("Length of 'values' vector must be either 1 or equal to the number of ranges.")
      if(!is.null(l$step) & length(l$step) != 1 & length(l$step) != l$nelements)
        stop("Length of 'step' vector must be either 1 or equal to the number of ranges.")
      if(!is.null(l$min) & length(l$min) != 1 & length(l$min) != l$nelements)
        stop("Length of 'min' vector must be either 1 or equal to the number of ranges.")
      if(!is.null(l$max) & length(l$max) != 1 & length(l$max) != l$nelements)
        stop("Length of 'max' vector must be either 1 or equal to the number of ranges.")
    }
    
    if(!is.null(l$on_change)) {
      l$on_click <- l$on_change
      l$on_change <- NULL
    }
    l
  })
}
