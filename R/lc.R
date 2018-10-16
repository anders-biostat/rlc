#' @import JsRCom
#' @import stringr
#' 
lc <- new.env()
lc$pageOpened <- F

lc$props <- list(scatter = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle"),
                barchart = c("ngroups", "groupIds", "nbars", "barIds", "nstacks", "stackIds", "value", "groupWidth", "stroke", "strokeWidth",
                             "nbins"), 
                beeswarm = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle", "valueAxis"),
                pointLine = c("lineWidth", "dasharray", "x", "y", "nsteps", "value"),
                xLine = c("lineWidth", "dasharray", "lineFun", "nsteps", "a", "b", "h"),
                yLine = c("lineWidth", "dasharray", "lineFun", "nsteps", "v"),
                pointRibbon = c("lineWidth", "dasharray", "x", "ymax", "ymin", "nsteps"),
                layer = c("nelements", "elementIds", "elementLabel", "layerDomainX", "layerDomainY", "contScaleX", "contScaleY",
                          "colour", "colourValue", "palette", "colourDomain", "colourLegendTitle", "addColourScaleToLegend", "opacity", "on_click",
                          "informText", "elementMouseOver", "elementMouseOut", "markedUpdated"))

Layer <- setRefClass("Layer", fields = list(type = "character", id = "character", 
                                            properties = "list", dataFun = "function",
                                            on_click = "function", on_mouseover = "function",
                                            on_mouseout = "function", init = "logical",
                                            markedUpdated = "function", parcerStep = "numeric"))
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
      warning("You are attempting to remove the main layer of the chart ", id, 
              " . The entire chart will be removed.")
      removeChart(id)
    } else {
      if(layerId %in% names(layers)) {
        stop(str_c("There is no layer with ID ", layerId))
      } else {
        sendCommands(str_interp("rlc.removeLayer('${id}', '${layerId}')"))
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

#' Open a new empty page
#' 
#' \code{openPage} creates a server, establishes a websocket connection between it and the current
#' R session and loads linked-charts JS library with all the dependencies. If there is already an 
#' opened page, it will be automatically closed.
#' 
#' @param useViewer If \code{TRUE}, the page will be opened in the RStudio Viewer. If \code{FALSE}
#' a default web browser will be used.
#' @param rootDirectory A path to the root directory of the server. If \code{rootDirectory} is not 
#' defined, the \code{http_root} in the package directory will be used as a root directory.
#' @param startPage A path to the HTML file that should be opened, when the server is initialised.
#' This can be an absolute path to a local file, or it can be relative from the \code{rootDirectory}
#' or to the current R working directory. If \code{startPage} is not defined, this function opens an 
#' empty HTML page. The file must have \emph{.html} extension.
#' @param layout Adds one of the defaults layouts to the page. Currently, only tables of arbitrary 
#' size are supported. To add a table set this parameter to \code{tableNxM}, where \code{N} is the
#' number of rows and \code{M} is the number of columns. Each cell will get an ID that consists of 
#' a letter (inticating the row) and a number (indicating the column) (e.g. \code{B3} is an ID of 
#' the second row and third column).
#' 
#' @examples
#' openPage()
#' 
#' openPage(useViewer = F, layout = "table2x3")
#' 
#' @export
#' @importFrom later run_now
openPage <- function(useViewer = T, rootDirectory = NULL, startPage = NULL, layout = NULL) {
  
  lc$charts <- list()
  lc$pageOpened <- F
  lc$useViewer <- useViewer
  JsRCom::openPage(useViewer = useViewer, rootDirectory = rootDirectory, startPage = startPage)
  srcDir <- "http_root_rlc"

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

#' Add a default layout to the opened web page
#' 
#' \code{addDefaultLayout} adds a layout that can be later used to arrange charts on the page (by
#' default each new chart is added to the bottom of the page).
#' 
#' Currently the only supported type
#' of a default layout is table with arbitrary number of rows and columns.
#' To use it set the layout argument to \code{tableMxN}, where \code{N} is the
#' number of rows and \code{M} is the number of columns. Each cell will get an ID that consists of 
#' a letter (inticating the row) and a number (indicating the column) (e.g. \code{B3} is an ID of 
#' the second row and third column).
#' 
#' @param layout Type of the layout. See 'Details' for more information.
#' 
#' @examples 
#' addDefaultLayout("table3x2")
#' 
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

#' Remove cahrt from the page
#' 
#' Removes an existing chart.
#' 
#' @param id A vector of IDs of the charts to be removed.
#' 
#' @examples 
#' lc_scatter(dat(x = 1:10, y = 1:10 * 2), id = "scatter")
#' removeChart("scatter") 
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
#' data("iris")
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Sepal.Width), id = "irisScatter")
#' setProperties(dat(symbolValue = iris$Species, y = iris$Petal.Length), id = "irisScatter")
#' updateChart("irisScatter")
#' 
#' lc_line(dat(x = iris$Sepal.Length, y = iris$Petal.Length), id = "irisScatter", layerId = "line")
#' setProperties(dat(colour = "red"), id = "irisScatter", layerId = "line")
#' updateChart("irisScatter")
#' 
#' @export
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

#' Update a chart
#' 
#' \code{updateChart} redraws a chart or a single layer of the chart to make it up
#' to date with current state of the environment.
#' 
#' Linked charts of the \emph{rlc} package are based on the idea that variables
#' used to define the chart are not constant, but can change as a result of user's
#' actions. Each time the \code{updateChart} is called, all the properties passed
#' via \code{\link{dat}} function are reevaluated and cahrt is changed in accordance with the
#' new state.
#' 
#' @section Update types: 
#' Defining a type of update allows you to change only some aspects of the chart
#' which may improve the performance speed. 
#' 
#' Some of the update types are valid for all the charts:
#' \itemize{
#'   \item{\code{Size} changes the size of the chart (and consequently the location
#'   of all its elements).}
#'   \item{\code{Title}changes the title of the chart.}
#'   \item{\code{Canvas}If number of elements is too high the 
#'   charts switch to the canvas mode and istead of multiple SVG point or cells
#'   a single Canvas image is generated. This type of update redraws the Canvas
#'   image. \emph{It is not recommended to use this function.}}
#' }
#' 
#' These types are specific for heatmaps only (\code{\link{lc_heatmap}}):
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
#' These types are valid for all other types of charts except heatmaps.
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
#' @param id An ID of the cahrt to be updated (or vector of IDs). If NULL then all the
#' existing charts will be updated.
#' 
#' @param layerId An ID of the layer to be updated (or vector of IDs). If NULL of the
#' layers of the selected charts will be updated. To update only the selected layers of
#' multiple charts the lengths of \code{id} and \code{layerId} must be the same.
#' 
#' @param updateType To improve performance it may be useful to change only certain 
#' aspects of the chart (e.g. location of the points, colour of the heatmap cells,
#' etc.). This argument can specify which part of chart to update. Possible options are
#' \code{Elements}, \code{ElementPosition}, \code{ElementStyle}, \code{Axes}, \code{Labels},
#' \code{Cells}, \code{Texts}, \code{LabelPosition}, \code{CellPosition}, 
#' \code{TextPosition}, \code{LabelText}, \code{CellColour}, \code{TextValues},
#' \code{Canvas}, \code{Size}. See details for more information.
#' 
#' @examples
#' data(iris)
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
#' updateChart("iris")
#' #this will change only height
#' updateChart("iris", updateType = "Size")
#' 
#' #add another property
#' setProperties(dat(symbolValue = iris$Species), "iris")
#' #this will change only colour and symbols
#' updateChart("iris", updateType = "ElementStyle")
#' 
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
      d$on_click <- NULL
    }
    
    if(!is.null(d$markedUpdated)) {
      layer$markedUpdated <- d$markedUpdated
      d$maekedUpdated <- NULL
      sendCommand(str_interp("rlc.setCustomMarkedUpdated('${chart$id}', '${layerName}');"))
    }
    
    if(!is.null(d$elementMouseOver)) {
      layer$on_mouseover <- d$elementMouseOver
      d$elementMouseOver <- NULL
      sendCommand(str_interp("rlc.setCustomMouseOver('${chart$id}', '${layerName}', ${layer$parcerStep});"))
    }
    if(!is.null(d$elementMouseOver)) {
      layer$on_mouseout <- d$elementMouseOut
      d$elementMouseOut <- NULL
      sendCommand(str_interp("rlc.setCustomMouseOut('${chart$id}', '${layerName}');"))
    }    
    
    name <- str_c(chart$id, layer$id, sep = "_")
    
    sendData(name, d)
    sendCommand(str_interp("rlc.setProperty('${name}')"))
  }
}

setChart <- function(type, data, ..., place, id, layerId, dataFun, parcerStep = 50) {
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
  
  layer$parcerStep <- parcerStep
  
  chart$JSinitialize()
  
  l <- list(...)
  nonEv <- lapply(names(l), function(n) {function() l[[n]]})
  names(nonEv) <- names(l)
  setProperties(c(data, nonEv), id, layerId)
  updateChart(id)
  
  invisible(chart)
}

chartEvent <- function(d, id, layerId, event) {
  
  if(is.numeric(d)) d <- d + 1 
  
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
  if(event == "markedUpdated")
    layer$markedUpdated()
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
#' second columns contain, respectively, row and colunm indices of the marked cells.
#' 
#' @examples
#' data(iris)
#' 
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length))
#' 
#' #now mark some points by selecting them with your mouse with Shift pressed
#' 
#' getMarked("Chart1")
#' 
#' @export
getMarked <- function(chartId, layerId = NULL) {
  chart <- getChart(chartId)
  if(is.null(chart))
    stop(str_c("Chart ", chartId, " is not defined."))
  
  if(is.null(layerId)) {
    if(chart$nLayers() == 0)
      layerId <- "main"
    if(chart$nLayers() == 1)
      layerId <- names(chart$layers)[2]
    if(chart$nLayers() > 1)
      stop("'layerId' is not defined")
  }
  marked <- NULL
  setEnvironment(environment()) 
  sendCommand(str_interp("rlc.getMarked('${chartId}', '${layerId}')"))
  for( i in 1:(10/0.05) ) {
    run_now()
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
  
  marked
}

#' Link data to the chart
#' 
#' \code{dat} allows to link variables from the current environment to chart's properties.
#' On every \code{\link{updateChart}} call all the data, provided via the \code{dat} function,
#' will be automatically reevaluated and the chart will be changed accordingly. One can also
#' put properties outside of the \code{dat} function, if they are going to be constant.
#' 
#' @param ... List of name values pair to define the properties. 
#' 
#' @examples 
#' lc_scatter(dat(x = rnorm(30)), y = rnorm(30))
#' #note that the Y values remain the same after each updateChart call
#' updateChart()
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
#' openPage(useViewer = F)
#' closePage()
#' 
#' @export
closePage <- function() {
  lc$charts <- list()
  lc$pageOpened <- F
  JsRCom::closePage()
}

scatterDataFun <- function(l) {
  if(is.null(l$x) && is.null(l$y))
    stop("Required properties 'x' and 'y' are not defined.")
  if(is.null(l$x))
    l$x <- 1:length(l$y)
  if(is.null(l$y))
    l$y <- 1:length(l$x)
  
  if(is.null(l$elementLabel)){
    if(!is.null(names(l$y)))
      l$elementLabel <- names(l$y)
    if(!is.null(names(l$x)))
      l$elementLabel <- names(l$x)
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
#' properties will be reevaluated on each \code{\link{updateChart}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. An error will be thrown
#' if a layer with this ID already exists in the chart. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param parcerStep Time in ms between to consequentive calls of onmouseover event. Prevents overqueuing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties in the vignette.
#' 
#' \itemize{
#'  \item{\code{x, y} - vector of x and y coordinates of the points.} 
#'  \item{\code{size} - sizes of the points. Default size is 6.}
#'  \item{\code{opacity} - opacity of the points in the range from 0 to 1.}
#'  \item{\code{elementLabel} - vector of text labels for each point.} 
#'  \item{\code{valueAxis} - (for \code{lc_beeswarm} only) defines, values along 
#'  which of the axes should not be changed. Must be \code{"x"} or \code{"y"}.} }
#' 
#' Colour and shape settings
#' \itemize{
#'  \item{\code{colour} - colour of the points. Must be a colour name or hexidecimal code.}
#'  \item{\code{colourValue} - grouping values for different colours. Can be numbers or charachters.}
#'  \item{\code{colourDomain} - vector of all possible values for discrete colour scales 
#'  or range of all possible colour values for the continuous ones.}
#'  \item{\code{palette} - vector of colours to construct the colour scale.}
#'  \item{\code{colourLegendTitle} - title for the colour legend.}
#'  \item{\code{addColourScaleToLegend} - whether or not to show colour legend for the current layer.} 
#'  \item{\code{symbol} - shape of each point. Must be one of \code{"Circle", "Cross", "Diamond", 
#'  "Square", "Star", "Triangle", "Wye"}.}
#'  \item{\code{symbolValue} - grouping values for different symbols.}
#'  \item{\code{symbolLegendTitle} - title for the symbol value.}
#'  \item{\code{stroke} - stroke colour for each element. Must be a colour name or hexidecimal code.}
#'  \item{\code{strokeWidth} - width of the strokes for each point.} }
#'  
#' Axes settings
#' \itemize{
#'  \item{\code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.}
#'  \item{\code{layerDomainX, layerDomainY} - default axes ranges for the given layer.}
#'  \item{\code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  is automatically set to include all layer domains.}
#'  \item{\code{contScaleX, consScaleY} - whether or not the axis should be continuous.}
#'  \item{\code{aspectRatio} - aspect ratio.}
#'  \item{\code{axisTitleX, axisTitleY} - axes titles.}
#'  \item{\code{ticksX, ticksY} - set of ticks for the axes.} }
#'
#' Interactivity settings
#' \itemize{
#'  \item{\code{on_click} - function, to be called, when one of the points is clicked. Gets an
#'  index of the clicked point as an argument.}
#'  \item{\code{elementMouseOver} - function, to be called, when mouse hovers over one of the points.
#'  Gets an index of the clicked point as an argument.}
#'  \item{\code{elementMouseOut} - function, to be called, when mouse moves out of one of the points.} }
#'  
#' Global chart settings
#' \itemize{
#'  \item{\code{width} - width of the chart in pixels.}
#'  \item{\code{heigth} - height of the chart in pixels.}
#'  \item{\code{plotWidth} - width of the plotting area in pixels.}
#'  \item{\code{plotHeight} - height of the plotting area in pixels.}
#'  \item{\code{margins} - margins size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'  \item{\code{title} - title of the chart.}
#'  \item{\code{titleX, titleY} - coordinates of the chart title.}
#'  \item{\code{titleSize} - font-size of the chart title.}
#'  \item{\code{showLegend} - whether or not to show the legend.}
#'  \item{\code{showPanel} - whether of not to show the tools panel.}
#'  \item{\code{transitionDuration} - duration of the transtions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.}
#' } 
#' 
#' @examples
#' data("iris")
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
#'             colourLegendTitle = "Sepal Width")
#' @export
lc_scatter <- function(data = list(), place = NULL, ..., id = NULL, layerId = NULL, parcerStep = 50) {
  setChart("scatter", data, ...,  place = place, id = id, layerId = layerId, dataFun = scatterDataFun, parcerStep = parcerStep)
}

#' @describeIn lc_scatter creates a special kind of scatterplot, where the points are spread along one of 
#' the axes to avoid overlapping.
#' 
#' @export
lc_beeswarm <- function(data, place = NULL, ..., id = NULL, layerId = NULL, parcerStep = 50) {
  setChart("beeswarm", data, ..., place = place, id = id, layerId = layerId, dataFun = function(l) {
    if(is.null(l$x) || is.null(l$y))
      stop("Required properties 'x' and 'y' are not defined.")

    if(is.null(l$elementLabel)){
      if(!is.null(names(l$y)))
        l$elementLabel <- names(l$y)
      if(!is.null(names(l$x)))
        l$elementLabel <- names(l$x)
    }
    
    if(lc$useViewer)
      l$mode <- "svg"    
    
    l   
  }, parcerStep = parcerStep)
}

#TO DO: Add grouping
lineDataFun <- function(l) {
  if(is.null(l$x) && is.null(l$y))
    stop("Required properties 'x' and 'y' are missing.")
  
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
#' properties will be reevaluated on each \code{\link{updateChart}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. An error will be thrown
#' if a layer with this ID already exists in the chart. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param parcerStep Time in ms between to consequentive calls of onmouseover event. Prevents overqueuing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties in the vignette.
#' 
#' \itemize{
#'  \item{\code{x, y} - vector of x and y coordinates of the points to connect. Can be 
#'  vectors for a single line or \code{m x n} matrix for \code{n} lines.}
#'  \item{\code{ymax, ymin} - (only for \code{lc_ribbon}) vectors of maximal and minimal values of the ribbon.} 
#'  \item{\code{lineWidth} - width of each line.}
#'  \item{\code{opacity} - opacity of the lines in the range from 0 to 1.}
#'  \item{\code{elementLabel} - vector of text labels for each line.} 
#'  \item{\code{dasharray} - defines pattern of dashes and gaps for each line.} }
#' 
#' Colour settings
#' \itemize{
#'  \item{\code{colour} - colour of the lines. Must be a colour name or hexidecimal code. For
#'  \code{lc_ribbon} this property defined the colour of the ribbon, not the strokes.}
#'  \item{\code{fill} - colour with wich to fill area inside the line. 
#'  Must be a colour name or hexidecimal code.}
#'  \item{\code{colourValue} - grouping values for different colours. Can be numbers or charachters.}
#'  \item{\code{colourDomain} - vector of all possible values for discrete colour scales 
#'  or range of all possible colour values for the continuous ones.}
#'  \item{\code{palette} - vector of colours to construct the colour scale.}
#'  \item{\code{colourLegendTitle} - title for the colour legend.}
#'  \item{\code{addColourScaleToLegend} - whether or not to show colour legend for the current layer.} 
#'  \item{\code{stroke} - (only for \code{lc_ribbon}) stroke colour for each ribbon. 
#'  Must be a colour name or hexidecimal code.}
#'  \item{\code{strokeWidth} - (only for \code{lc_ribbon}) width of the strokes for each ribbon.} }
#'  
#' Axes settings
#' \itemize{
#'  \item{\code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.}
#'  \item{\code{layerDomainX, layerDomainY} - default axes ranges for the given layer.}
#'  \item{\code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  is automatically set to include all layer domains.}
#'  \item{\code{contScaleX, consScaleY} - whether or not the axis should be continuous.}
#'  \item{\code{aspectRatio} - aspect ratio.}
#'  \item{\code{axisTitleX, axisTitleY} - axes titles.}
#'  \item{\code{ticksX, ticksY} - set of ticks for the axes.} }
#'
#' Interactivity settings
#' \itemize{
#'  \item{\code{on_click} - function, to be called, when one of the lines is clicked. Gets an
#'  index of the clicked point as an argument.}
#'  \item{\code{elementMouseOver} - function, to be called, when mouse hovers over one of the lines.
#'  Gets an index of the clicked point as an argument.}
#'  \item{\code{elementMouseOut} - function, to be called, when mouse moves out of one of the lines.} }
#'  
#' Global chart settings
#' \itemize{
#'  \item{\code{width} - width of the chart in pixels.}
#'  \item{\code{heigth} - height of the chart in pixels.}
#'  \item{\code{plotWidth} - width of the plotting area in pixels.}
#'  \item{\code{plotHeight} - height of the plotting area in pixels.}
#'  \item{\code{margins} - margins size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'  \item{\code{title} - title of the chart.}
#'  \item{\code{titleX, titleY} - coordinates of the chart title.}
#'  \item{\code{titleSize} - font-size of the chart title.}
#'  \item{\code{showLegend} - whether or not to show the legend.}
#'  \item{\code{showPanel} - whether of not to show the tools panel.}
#'  \item{\code{transitionDuration} - duration of the transtions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.}
#' } 
#' 
#' @examples 
#' x <- seq(0, 8, 0.2)
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
#' x <- seq(0, 5, 0.1, sd = 2)
#' y <- x*3 + rnorm(length(x))
#' fit <- lm(y ~ x)
#' pred <- predict(fit, data.frame(x = x), se.fit = T)
#' lc_ribbon(dat(ymin = pred$fit - 1.96 * pred$se.fit,
#'               ymax = pred$fit + 1.96 * pred$se.fit,
#'               x = x,
#'               colour = "#555555"), id = "ribbonTest")
#' lc_scatter(dat(x = x, y = y), size = 2, id = "ribbonTest")
#' lc_abLine(dat(a = fit$coefficients[2], b = fit$coefficients[1]), id = "ribbonTest")
#' @export
lc_line <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("pointLine", data, ..., place = place, id = id, layerId = layerId, 
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
lc_path <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("pointLine", data, ..., place = place, id = id, layerId = layerId, dataFun = lineDataFun)
}

#' @describeIn lc_line displays a filled area, defined by \code{ymax} and \code{ymin} values.
#' @export
lc_ribbon <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("pointRibbon", data, ...,  place = place, id = id, layerId = layerId, dataFun = function(l) {
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

#' Create a barplot
#' 
#' \code{lc_bars} creates a new barplot and adds it on the page
#' as a new chart or as a new layer of an existing chart.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateChart}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. An error will be thrown
#' if a layer with this ID already exists in the chart. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param parcerStep Time in ms between to consequentive calls of onmouseover event. Prevents overqueuing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties in the vignette.
#' 
#' \itemize{
#'  \item{\code{value} - heights of bars/stacks.}
#'  \item{\code{stackIds} - IDs for all stacks (if necessary). Must be the same size as \code{values}.}
#'  \item{\code{barIds} - IDs for all bars (if necessary). Must be the same size as \code{values}.}
#'  \item{\code{groupIds} - IDs for all groups (if necessary). Must be the same size as \code{values}.} 
#'  \item{\code{groupWidth} - ratio of width of a group of bars to the space, available to the group.} }
#' 
#' Style settings
#' \itemize{
#'  \item{\code{opacity} - opacity of each bar|stack in the range from 0 to 1.}
#'  \item{\code{colour} - colour of each bar|stack. Must be a colour name or hexidecimal code.}
#'  \item{\code{colourValue} - grouping values for different colours. Can be numbers or charachters.}
#'  \item{\code{colourDomain} - vector of all possible values for discrete colour scales 
#'  or range of all possible colour values for the continuous ones.}
#'  \item{\code{palette} - vector of colours to construct the colour scale.}
#'  \item{\code{colourLegendTitle} - title for the colour legend.}
#'  \item{\code{addColourScaleToLegend} - whether or not to show colour legend for the current layer.} 
#'  \item{\code{stroke} -  stroke colour of each bar|stack. Must be a colour name or hexidecimal code.}
#'  \item{\code{strokeWidth} - width of the strokes of each bar|stack.} }
#'  
#' Axes settings
#' \itemize{
#'  \item{\code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.}
#'  \item{\code{layerDomainX, layerDomainY} - default axes ranges for the given layer.}
#'  \item{\code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  is automatically set to include all layer domains.}
#'  \item{\code{contScaleX, consScaleY} - whether or not the axis should be continuous.}
#'  \item{\code{aspectRatio} - aspect ratio.}
#'  \item{\code{axisTitleX, axisTitleY} - axes titles.}
#'  \item{\code{ticksX, ticksY} - set of ticks for the axes.} }
#'
#' Interactivity settings
#' \itemize{
#'  \item{\code{on_click} - function, to be called, when one of the lines is clicked. Gets an
#'  index of the clicked point as an argument.}
#'  \item{\code{elementMouseOver} - function, to be called, when mouse hovers over one of the lines.
#'  Gets an index of the clicked point as an argument.}
#'  \item{\code{elementMouseOut} - function, to be called, when mouse moves out of one of the lines.} }
#'  
#' Global chart settings
#' \itemize{
#'  \item{\code{width} - width of the chart in pixels.}
#'  \item{\code{heigth} - height of the chart in pixels.}
#'  \item{\code{plotWidth} - width of the plotting area in pixels.}
#'  \item{\code{plotHeight} - height of the plotting area in pixels.}
#'  \item{\code{margins} - margins size in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'  \item{\code{title} - title of the chart.}
#'  \item{\code{titleX, titleY} - coordinates of the chart title.}
#'  \item{\code{titleSize} - font-size of the chart title.}
#'  \item{\code{showLegend} - whether or not to show the legend.}
#'  \item{\code{showPanel} - whether of not to show the tools panel.}
#'  \item{\code{transitionDuration} - duration of the transtions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.}
#' } 
#' @examples 
#' data("esoph")
#' 
#' lc_bars(dat(value = tapply(esoph$ncases, esoph$agegp, sum), 
#'             title = "Number of cases per age group",
#'             axisTitleX = "Age group", 
#'             axisTitleY = "Number of esophageal cases"))
#' 
#' lc_bars(dat(value = c(tapply(esoph$ncases, esoph$agegp, sum), tapply(esoph$ncontrols, esoph$agegp, sum)),
#'             stackIds = c(rep("case", 6), rep("control", 6))))
#' 
#' #It is ease to put data in a convenient form for barplots using tidyverse
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
#'             groupIds = newData$agegp))
#' 
#' @export
lc_bars <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("barchart", data, ..., place = place, id = id, layerId = layerId, dataFun = barDataFun)
}

#' Histograms and density plots
#' 
#' These functions make either a histogram or a density plot of the given data 
#' and either add them as a new layer to an existing chart or create a new chart.
#' 
#' @param data Name value pairs of properties, passed through the \code{\link{dat}} function. These
#' properties will be reevaluated on each \code{\link{updateChart}} call. 
#' @param place An ID of a container, where to place the chart. Will be ignored if the chart already
#' exists. If not defined, the chart will be placed directly in the body of the opened page.
#' @param ... Name value pairs of properties that can be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function
#' @param id An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, a new layer will be added to it. If you want to replace one chart with another, use \code{\link{removeChart}}
#' first. If not defined, the ID will be set to \code{ChartN}, where \code{N - 1} is the number of currently existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. An error will be thrown
#' if a layer with this ID already exists in the chart. If not defined, will be set to \code{LayerN}, where \code{N - 1} 
#' is the number of currently existing layers in this chart.
#' @param parcerStep Time in ms between to consequentive calls of onmouseover event. Prevents overqueuing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties:
#' 
#' \itemize{
#'  \item{\code{value} - vector of data.}
#'  \item{\code{nbins} - (only for \code{lc_hist}) number of bins.} }
#' 
#' These functions are extensions of \code{\link{lc_line}} (\code{lc_dens}) or \code{\link{lc_bars}} 
#' (\code{lc_hist}) and therefore can also understand their properties.
#' 
#' @describeIn lc_hist makes a histogram. It is an extension of \code{\link{lc_bars}}.
#' 
#' @examples
#' lc_hist(dat(value = rnorm(1000), nbins = 30, height = 300))
#' lc_dens(dat(value = rnorm(1000), height = 300))
#' 
#' @export 
lc_hist <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  # has a nbins property. Not implemented in JS
  setChart("barchart", data, ..., place = place, id = id, layerId = layerId, dataFun = function(l) {
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

#' @describeIn lc_hist makes a density plot. Is an extension of \code{\link{lc_line}}.
#' @export
lc_dens <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("pointLine", data, ..., place = place, id = id, layerId = layerId, dataFun = function(l) {
    if(!is.null(l$value)) {
      dens <- density.default(l$value)
      l$x <- dens$x
      l$y <- dens$y
      l$value <- NULL
    }
    lineDataFun(l)
  })
}

#' @export
lc_heatmap <- function(data, place = NULL, ..., id = NULL) {
  setChart("heatmap", data, ..., place = place, id = id, layerId = "main", dataFun = function(l) {
    if(!is.null(l$value)) {
      l$nrows <- nrow(l$value)
      l$ncols <- ncol(l$value)
    }
    l
  })
}

#' @export
lc_colourSlider <- function(data, place = NULL, ..., id = NULL) {
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

#' @export
#' @importFrom jsonlite toJSON
lc_abLine <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("xLine", data, ..., place = place, id = id, layerId = layerId, dataFun = function(l) {
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
  
#' @export
lc_hLine <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("xLine", data, ..., place = place, id = id, layerId = layerId, dataFun = function(l) {
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

#' @export
lc_vLine <- function(data, place = NULL, ..., id = NULL, layerId = NULL) {
  setChart("yLine", data, ..., place = place, id = id, layerId = layerId, dataFun = function(l) {
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

#' @export
#' @importFrom hwriter hwrite
lc_html <- function(code = "", place = "undefined", append = F) {
  append <- ifelse(append, "true", "false")
  if(!is.character(code) || length(code) != 1)
    code <- hwrite(code)
  
  code <- gsub("[\r\n]", "", code)
  code <- str_replace_all(code, "(\\W)", "\\\\\\1")
  sendCommand(str_c("rlc.html('", code, "', '", place, "', ", append, ")"))
}