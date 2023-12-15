#' @import jrc
#' @import stringr
#' @import R6
#' @importFrom stats runif
pkg.env <- new.env()
.app <- NULL
.id <- NULL

pkg.env$dataFun <- list(
  scatter = function(l) {
    # if(is.null(l$x) && is.null(l$y))
    #   stop("Required properties 'x' and 'y' are not defined.")
    
    if(is.factor(l$x))
      l$layerDomainX <- levels(l$x)
    if(is.factor(l$y))
      l$layerDomainY <- levels(l$y)
    
    if(is.null(l$x))
      l$x <- 1:length(l$y)
    if(is.null(l$y))
      l$y <- 1:length(l$x)
    
    l$nelements <- length(l$x)
    
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
        stop("'jitterY' must be a number")
      l$jitterY = abs(l$jitterY)
      
      l$shiftY <- runif(length(l$y), -l$jitterY, l$jitterY)
      l$jitterY <- NULL
    }
    
    if(is.null(l$symbolLegendTitle)) 
      l$symbolLegendTitle <- ""
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""
    
    l
  },
  beeswarm = function(l) {
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

    if(is.null(l$symbolLegendTitle)) 
      l$symbolLegendTitle <- ""
  
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""    
    
    l   
  },
  
  path = function(l) {
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
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""
    
    l
  },
  
  line = function(l) {
    l <- pkg.env$dataFun$path(l)
    
    for(i in 1:ncol(l$x)) {
      l$y[, i] <- l$y[order(l$x[, i]), i]
      l$x[, i] <- l$x[order(l$x[, i]), i]
    }
    l
  },
  
  ribbon = function(l) {
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
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""    
    
    l
  },
  
  bars = function(l) {
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
    
    if(is.null(l$hist)) {
      if(!(length(l$barIds) == 1 && l$barIds == 1)) l$barLabel <- unique(l$barIds)
      if(!(length(l$stackIds) == 1 && l$stackIds == 1)) l$stackLabel <- unique(l$stackIds)
      if(!(length(l$groupIds) == 1 && l$groupIds == "group")) l$groupLabel <- unique(l$groupIds)
      
      l$barIds <- as.numeric(as.factor(l$barIds)) - 1
      l$stackIds <- as.numeric(as.factor(l$stackIds)) - 1
      l$groupIds <- as.numeric(as.factor(l$groupIds)) - 1    
    } else {
      l$hist <- NULL
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
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""    
    
    l
  },
  
  hist = function(l) {
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
    
    l$hist <- TRUE
    
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
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""
    
    pkg.env$dataFun$bars(l)
  },
  
  dens = function(l) {
    if(!is.null(l$value)) {
      dens <- density.default(l$value)
      l$x <- dens$x
      l$y <- dens$y
      l$value <- NULL
    }
    pkg.env$dataFun$path(l)
  },
  
  heatmap = function(l) {
    if(!is.null(l$value)) {
      l$nrows <- nrow(l$value)
      l$ncols <- ncol(l$value)
      
      l$value <- as.matrix(l$value)
      
      if(is.null(l$rowLabel) & !is.null(rownames(l$value)))
        l$rowLabel <- rownames(l$value)
      if(is.null(l$colLabel) & !is.null(colnames(l$value)))
        l$colLabel <- colnames(l$value)
    }

    l
  },
  
  colourSlider = function(l) {
    if(!is.null(l$chart)) {
      l$linkedChart <- str_c("charts.", l$chart)
      
      if(is.null(l$layer) && l$app$getChart(l$chart)$nLayers() == 1) 
        l$layer <- l$app$getChart(l$chart)$getLayerIds()[2]
      if(!is.null(l$layer))
        l$linkedChart <- str_c(l$linkedChart, ".layers.", l$layer)
      
    }
    l$chart <- NULL
    l$layer <- NULL
    l$app <- NULL
    l
  },
  
  abLine = function(l) {
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

    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""
    
    l
  },
  
  hLine = function(l) {
    if(is.null(l$h))
      stop("Required property 'h' is not defined.");
    
    l$h <- as.vector(l$h)
    
    l$nelements <- length(l$h)
    l$lineFun <- str_c("function(x, d) { h = ", toJSON(l$h, digits = NA), ";",
                       "return h[d]; }")
    l$h <- NULL
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""    
    
    l
  },
  
  vLine = function(l) {
    if(is.null(l$v))
      stop("Required property 'v' is not defined.");
    
    l$v <- as.vector(l$v)
    
    l$nelements <- length(l$v)
    l$lineFun <- str_c("function(x, d) { v = ", toJSON(l$v, digits = NA), ";",
                       "return v[d]; }")
    l$v <- NULL
    
    if(is.null(l$colourLegendTitle))
      l$colourLegendTitle <- ""
    
    l
  },
  
  html = function(l) {
    if(!is.character(l$content) || length(l$content) != 1)
      l$content <- hwrite(l$content)
    
    #l$content <- gsub("[\r\n]", "", l$content)
    #l$content <- str_replace_all(l$content, "(\\W)", "\\\\\\1")
    
    l
  },
  
  input = function(l){
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
  },
  axesChart = function(l) {
    if(!is.null(l$axisTitlePosX)){
      l$axisTitlePos <- list()
      l$axisTitlePos$x <- l$axisTitlePosX
      l$axisTitlePosX <- NULL
    }
    if(!is.null(l$axisTitlePosY)) {
      if(is.null(l$axisTitlePos))
        l$axisTitlePos <- list()
      l$axisTitlePos$y <- l$axisTitlePosY
      l$axisTitlePosY <- NULL
    }

    l
  },
  image = function(l) {

    if(is.null(l$img) & is.null(l$src))
      stop("At least one of 'img' or 'src' must be specified")
    
    if(!is.null(l$img) & !is.null(l$src))
      warning("Both 'img' and 'src' are given. 'src' will be ignored.")
    
    if(!is.null(l$img)) {
      l$src <- tempfile(fileext = ".png")
      
      if(is.null(l$width)) l$width <- 500
      if(is.null(l$height)) l$height <- 500
      if(is.null(l$paddings)) l$paddings <- list(top = 35, right = 10,
                                                 bottom = 10, left = 10)
      
      png(filename = l$src, 
          width = l$width - l$paddings$left - l$paddings$right, 
          height = l$height - l$paddings$top - l$paddings$bottom)
        print(l$img)
      dev.off()
      l$img <- NULL
    }
    
    l$src <- normalizePath(l$src, winslash = "/")
    for(p in l$paths) 
      if(grepl(p, l$src, fixed = TRUE))
        l$src <- str_remove(l$src, p)
    
    l$paths <- NULL
    
    l
  }
)

#' @name LCApp
#' @title LCApp class
#' 
#' @description Object of this class represents the entire linked-charts app. It stores all charts, client sessions and
#' local variables. You can create and manage interactive apps solely by creating new instances of this class and utilizing
#' their methods. There are no limitations on the number of apps simultaneously running in one R session.
#' However, it is also possible to create and manage app via the wrapper functions provided in this package. In this case an
#' instance of \code{\link{LCApp}} class is initialized and stored in the package's namespace. Therefore, only one app can be active simultaneously.
#' You can always retrieve the active app with the \code{\link{getPage}} function. The \code{LCApp} class inherits from 
#' the \code{\link[jrc]{App}} class of the \code{jrc} package.
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{removeChart(chartId)}}{
#'       Removes a chart with the given ID from the app. See also \code{\link{removeChart}}.
#'    }
#'    \item{\code{removeLayer(chartId, layerId)}}{
#'       Removes a layer from a chart by their IDs. See also \code{\link{removeLayer}}.
#'    }
#'    \item{\code{setProperties(data, chartId, layerId = NULL)}}{
#'       Changes or sets properties for a given chart and layer. For more information, please, check \code{\link{setProperties}}.
#'    }
#'    \item{\code{updateCharts(chartId = NULL, layerId = NULL, updateOnly = NULL, sessionId = NULL)}}{
#'       Updates charts or specific layers for one or multiple users. For more information on the arguments,
#'       please, check \code{\link{updateCharts}}.
#'    }
#'    \item{\code{chartEvent(d, chartId, layerId = "main", event, sessionId = NULL)}}{
#'       Triggers a reaction to mouse event on a web page. Generally, this method is not supposed to be 
#'       called explicitly. It is called internally each time, client clicks or hovers over an interactive chart element.
#'       However, experienced users can use this method to simulate mouse events on the R side. For more information
#'       on the arguments, please, check \code{\link{chartEvent}}.
#'    }
#'    \item{\code{listCharts()}}{
#'       Prints a list of all existing charts and their layers. See also \code{\link{listCharts}}.
#'    }
#'    \item{\code{getMarked(chartId = NULL, layerId = NULL, sessionId = NULL)}}{
#'       Returns a vector of indices of all currently marked elements of a certain chart and layer and from a given client.
#'       For more information, please, check \code{\link{getMarked}}.
#'    }
#'    \item{\code{mark(elements, chartId = NULL, layerId = NULL, preventEvent = TRUE, sessionId = NULL)}}{
#'       Marks elements of a given chart and layer on one of the currently opened web pages. Please, check
#'       \code{\link{mark}} for more information on the arguments.
#'    }
#'    \item{\code{setChart(chartType, data, ..., place = NULL, chartId = NULL, layerId = NULL, [...])}}{
#'       Adds a new chart (or replaces an existing one) to the app. This is the main method of the package, that
#'       allows to define any chart and all its properties. There are multiple wrappers for this method - one for each type of 
#'       chart. Here is a full list:
#'          \itemize{
#'             \item \code{\link{lc_scatter}}
#'             \item \code{\link{lc_beeswarm}}
#'             \item \code{\link{lc_line}}
#'             \item \code{\link{lc_path}}
#'             \item \code{\link{lc_ribbon}}
#'             \item \code{\link{lc_bars}}
#'             \item \code{\link{lc_hist}}
#'             \item \code{\link{lc_dens}}
#'             \item \code{\link{lc_heatmap}}
#'             \item \code{\link{lc_colourSlider}}
#'             \item \code{\link{lc_abLine}}
#'             \item \code{\link{lc_vLine}}
#'             \item \code{\link{lc_html}}
#'             \item \code{\link{lc_input}}
#'          }
#'      You can check the wrapper functions for information about arguments and available properties. Compared to them, this 
#'      method gets additional argument \code{chartType}, which is always the same as the second part of the name of a 
#'      corresponding wrapper function (\code{lc_'chartType'}). In all other aspects, wrapper functions and the \code{setChart} 
#'      method are the same.
#'    }
#'    \item{\code{new(layout = NULL, beforeLoad = function(s) {}, afterLoad = function(s) {}, ...)}}{
#'       Creates new instance of class \code{LCApp}. Most of its arguments are inherited from method \code{new} of
#'       class \code{\link[jrc]{App}} from the \code{jrc} package. There are only three arguments specific for the
#'       \code{LCApp} class. \code{layout} sets a default
#'       layout for each new webpage (currently only tables of arbitrary size are supported).
#'       \code{beforeLoad} and \code{afterLoad} replace \code{onStart} from the \code{\link[jrc]{App}}
#'       class. For more information, please, check \code{\link{openPage}}.
#'    }
#' }
#'
NULL

#' @export
LCApp <- R6Class("LCApp", inherit = App, public = list(
  removeChart = function(chartId) {
    if(!is.vector(chartId) | !is.character(chartId))
      stop("Chart ID must be a vector of characters")

    for(id in chartId){
      private$charts[[id]] <- NULL
      for(session in private$sessions)
        if(!is.null(session))
          session$sendCommand(str_interp("rlc.removeChart('${chartId}');"))
    }
    
    invisible(self)
  },
  
  removeLayer = function(chartId, layerId) {
    chart <- self$getChart(chartId)
    chart$removeLayer(layerId)
    
    invisible(self)
  },
  
  setLayout = function(layout) {
    if(grepl("^table", layout)){
      size <- as.numeric(str_extract_all(layout, "\\d", simplify = TRUE))
      if(length(size) != 2) stop("Size of the table is specified incorrectly")
      private$layout <- c("Table", size)
    } else {
      stop("Unknown default layout name")        
    }
    invisible(self)
  },

  setProperties = function(data, chartId, layerId = NULL, with = NULL){
    #if(!is.null(with) && !is.language(with)) with <- with

    if(!is.null(with) && is.list(with) && is.language(with[[1]]))
      with <- with[[1]]

    chart <- self$getChart(chartId)
    if(is.null(chart))
      stop(str_c("Chart with ID ", chartId, " is not defined."))
    chart$data.frame <- with
    
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
    
    data <- plyr::rename(data, private$nameList, warn_missing = FALSE)
    if(is.null(layer))
      stop(str_c("Layer with ID ", layerId, " is not defined."))
    
    
    for(prop in names(data)){
      if(prop %in% private$props[[layer$type]] | prop %in% private$props$layer) {
        layer$setProperty(prop, data[[prop]])
      } else {
        if(prop %in% private$props$all) {
          mainLayer$setProperty(prop, data[[prop]])
        } else {
          warning(str_c("In chart '", chart$id, "': Property '", prop, "' doesn't exist."))
        }
      }
    }

    invisible(self)
  },
  
  updateCharts = function(chartId = NULL, layerId = NULL, updateOnly = NULL, sessionId = NULL) {
    if(length(private$charts) == 0) {
      stop("There are no charts yet.")
    }
    
    if(is.null(chartId)) chartId <- names(private$charts)
    if(!is.vector(chartId))
      stop("'chartId' should be a vector of IDs")

    maxL <- max(length(chartId), length(layerId), length(updateOnly))
    if(maxL > 1) {
      if(length(chartId) == 1) chartId <- rep(chartId, maxL)
      if(length(layerId) == 1) layerId <- rep(layerId, maxL)
      if(length(updateOnly) == 1) updateOnly <- rep(updateOnly, maxL)
    }
    
    if(!is.null(layerId) & length(layerId) != length(chartId))
      stop("Lengths of 'chartId' and 'layerId' differ")
    if(!is.null(updateOnly) & length(updateOnly) != length(chartId))
      stop("Lengths of 'chartId' and 'updateOnly' differ")        
      
    if(is.null(sessionId))
      sessionId <- names(private$sessions)

    for(i in 1:length(chartId)) {
      chart <- self$getChart(chartId[i])
      if(!is.null(chart)) {
        chart$update(sessionId, layerId[i], updateOnly[i])
      } else {
        warning(str_c("There is no chart with ID ", chartId[i]))
      }
    }
  },
  
  getChart = function(id) {
    if(!is.character(id))
      stop("Chart ID must be a character")
    if(length(id) > 1) {
      warning("Attempt to supply several chart IDs at once. Only the first one will be used.")
      id <- id[1]
    }
    
    private$charts[[id]]
  },
  
  chartEvent = function(d, chartId, layerId = "main", event, sessionId = NULL) {

    #lame. This also must go to jrc with the nearest update
    if(!is.null(d))
      d <- type.convert(d, as.is = TRUE)
    if(is.numeric(d) & event != "clickPosition") d <- d + 1
    # should we move that to jrc? And add some parameter, like 'flatten'?
    if(is.list(d))
      if(all(sapply(d, function(el) length(el) == 1)))
        d <- unlist(d)

    chart <- self$getChart(chartId)
    if(is.null(chart))
      stop(str_interp("Chart with ID ${id} is not defined"))
 
    layer <- chart$getLayer(layerId)
    if(is.null(layer))
      stop(str_interp("Chart ${id} doesn't have layer ${layerId}"))
    if(names(layer$type) == "hist" & event != "clickPosition")
      d <- d[1] - 1
    
    session <- super$getSession(sessionId)
    if(is.null(session))
      stop("Can't retreive the session")
    
    env <- session$sessionVariables()
    env$.chartId <- chartId
    env$.layerId <- layerId
    
    f <- layer[[paste0("on_", event)]]
    environment(f) <- env
    if(is.null(d)) {
      do.call(f, list())
    } else {
      do.call(f, list(d))
    }

    invisible(self)  
  },
  
  listCharts = function() {
    for(chartId in names(private$charts)) {
      print(str_interp("Chart: ${chartId}"))
      chart <- self$getChart(chartId)
      if(chart$nLayers() > 1) {
        print("- Layers:")
        for(layerId in chart$getLayerIds()){
          layer <- chart$getLayer(layerId)
          if(layer$id != "main")
            print(str_interp("-- ${layer$id} - ${layer$type}"))
        }
      }
    }
    
    invisible(self)
  },
  
  getMarked = function(chartId = NULL, layerId = NULL, sessionId = NULL) {
    session <- super$getSession(sessionId)
    if(is.null(session))
      stop("Can't retreive the session")

    if(is.null(chartId))
      if(length(private$charts) == 1) {
        chartId <- private$charts[[1]]$id
      } else {
        stop(str_c("There are more than one chart on the page. 'chartId' must be ",
                   "specified. Use 'listCharts()' to get IDs of all existing charts."))
      }
    
    chart <- self$getChart(chartId)
    if(is.null(chart))
      stop(str_c("Chart ", chartId, " is not defined."))
    
    if(is.null(layerId)) {
      if(chart$nLayers() == 0)
        layerId <- "main"
      if(chart$nLayers() == 1)
        layerId <- chart$getLayerIds()[2]
      if(chart$nLayers() > 1)
        stop(str_c("There is more than one layer in this chart. 'layerId' must be specified. ",
                   "Use 'listCharts()' to get IDs of all existing charts and their layers."))
    }
    session$sessionVariables(list(.marked = NULL))
    session$sendCommand(str_interp("rlc.getMarked('${chartId}', '${layerId}')"), wait = 5)
    marked <- session$sessionVariables(varName = ".marked", remove = ".marked")
    
    if( is.null(marked) ) {
      warning( "Can't load marked elements" )
    }
    
    if(is.numeric(marked)) marked <- marked + 1
    if(length(marked) == 0)
      return (c())
    
    marked
  },
  
  mark = function(elements = NULL, chartId = NULL, layerId = NULL, 
                  preventEvent = TRUE, clear = FALSE, sessionId = NULL) {
    session <- super$getSession(sessionId)
    if(is.null(session))
      stop("Can't retreive the session")

    if(is.null(chartId))
      if(length(private$charts) == 1) {
        chartId <- private$charts[[1]]$id
      } else {
        stop(str_c("There is more than one chart on the page. 'chartId' must be ",
                   "specified. Use 'listCharts()' to get IDs of all existing charts."))
      }
    
    chart <- self$getChart(chartId)
    if(is.null(chart))
      stop(str_c("Chart ", chartId, " is not defined."))

    if(is.null(layerId)) {
      if(chart$nLayers() == 0)
        layerId <- "main"
      if(chart$nLayers() == 1)
        layerId <- chart$getLayerIds()[2]
      if(chart$nLayers() > 1)
        stop(str_c("There is more than one layer in this chart. 'layerId' must be specified. ",
                   "Use 'listCharts()' to get IDs of all existing charts and their layers."))
    }
    
    if(chart$getLayer(layerId)$type == "heatmap") {
      if(length(elements) != 0) {
        elements <- as.matrix(elements)
        if(!is.matrix(elements))
          stop("'elements' must be a matrix with 2 columns or a vector of length 2")
        if(length(elements) == 2) 
          elements <- matrix(elements, ncol = 2)
        if(ncol(elements) != 2)
          stop("'elements' must have two columns (row and column indices)")
      }
    } else {
      if(length(elements) != 0 & !is.vector(elements))
        stop("'elements' must be a vector of indices.")
    }
    
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
    
    if(clear) {
      session$sendData("markElements", "__clear__", keepAsVector = TRUE)
      session$sendCommand(str_c("rlc.mark('", chartId, "', '", layerId, "', ", preventEvent, ");"))    
    }
    
    session$sendData("markElements", elements, keepAsVector = TRUE)
    session$sendCommand(str_c("rlc.mark('", chartId, "', '", layerId, "', ", preventEvent, ");"))    
  },
  
  setChart = function(chartType, data = list(), ..., place = NULL, chartId = NULL, layerId = NULL, addLayer = FALSE, with = NULL, pacerStep = 50) {
    if(is.null(private$serverHandle)){
      super$startServer()
#      super$openPage()
    }

    if(!is.null(with) && !is.language(with)) with <- with
    
    if(length(chartType) > 1){
      warning("Attempt supply several types for one chart. Only the first one will be used.")
      chartType <- chartType[1]
    }
    
    if(!(chartType %in% names(private$jsTypes)))
      stop("Unknown chart type")
    
    if(chartType == "colourSlider")
      data$app <- expression(.app)
    
    if(chartType == "image")
      data$paths <- self$allowDirectories()
      
    
    if(is.null(place) && is.null(chartId))
      place <- str_c("Chart", length(private$charts) + 1)
    
    if(is.null(chartId))
      chartId <- place
    if(is.null(place))
      place <- chartId
    
    chart <- self$getChart(chartId)
    if(!is.null(chart) && ((!is.null(layerId) && layerId == "main") || 
                           chart$nLayers() == 0)) {
      self$removeChart(chart$id)
      chart <- self$getChart(chartId)
    }
    
    if(is.null(chart)) {
      chart <- private$addChart(chartId, place)
      if(is.null(layerId) || layerId != "main"){
        layer <- chart$addLayer("main", c("axesChart" = "axesChart"))
        layer$dataFun <- pkg.env$dataFun$axesChart
      }
    }

    if(is.null(layerId)){
      if(!addLayer & chart$nLayers() > 1) {
        warning(str_c("Chart '", chartId, "' has ", chart$nLayers(), " layers and the layer ID is not specified. ", 
                      "'addLayer' will be set to TRUE."))
        addLayer <- T
      }
      
      if(addLayer | chart$nLayers() == 0) {
        layerId <- str_c("Layer", (chart$nLayers() + 1))
      } else {
        layerId <- chart$getLayerIds()[2]
      }
    }
    if(!is.null(chart$getLayer(layerId)))
      chart$removeLayer(layerId)
    
    layer <- chart$addLayer(layerId, private$jsTypes[chartType])
    if(!is.null(pkg.env$dataFun[[chartType]])){
      layer$dataFun <- pkg.env$dataFun[[chartType]]
    }

    layer$pacerStep <- pacerStep
    
    self$setProperties(c(data, list(...)), chartId, layerId, with)
    
    for(session in private$sessions){
      chart$JSInitialize(session)
      chart$update(session)
    }
    
    invisible(self)
 
  },  
  
  initialize = function(layout = NULL, beforeLoad = function(session) {}, afterLoad = function(session) {}, 
                        allowedVariables = NULL, allowedFunctions = NULL, 
                        allowedDirectories = getwd(), ...){
    
    allowedFunctions <- c("chartEvent", allowedFunctions)
    allowedVariables <- c(".marked", "s1", "s2", allowedVariables)
    allowedDirectories <- c(tempdir(), allowedDirectories)
    
    onStart_lc <- function(session) {

      srcDir <- "http_root_rlc"
      reqPage <- system.file( "/http_root/linked-charts.css", package = "rlc" )
      
      session$sessionVariables(list(s1 = 0, s2 = 0))
      session$sendCommand(str_c("link = document.createElement('link');", 
                        "link.rel = 'stylesheet';", 
                        "link.href = '", srcDir, "/linked-charts.css';", 
                        "document.head.appendChild(link);", collapse = "\n")) 
      session$sendCommand(str_c("script = document.createElement('script');", 
                        "script.src = '", srcDir, "/rlc.js';",
                        "script.onload = function() {jrc.sendData('s1', 1)};",
                        "document.head.appendChild(script);", collapse = "\n"), wait = 5)
      
      session$sendCommand(str_c("script = document.createElement('script');", 
                        "script.src = '", srcDir, "/linked-charts.min.js';",
                        "script.onload = function() {jrc.sendData('s2', 1)};",                    
                        "document.head.appendChild(script);", collapse = "\n"), wait = 5)

      if( session$sessionVariables(varName = "s1") == 0 || 
          session$sessionVariables(varName = "s2") == 0 ) {
        super$closeSession(session)
        stop( "Can't load linked-charts.js or rlc.js" )
      }
      session$sessionVariables(vars = list(.app = self), remove = c("s1", "s2"))
      
      session$sendCommand("d3.select('title').text('R/linked-charts');")
      
      beforeLoad(session)
      
      private$addLayout(session)
      for(chart in private$charts){
        chart$JSInitialize(session)
        chart$update(session)
      }
      
      afterLoad(session)
    }

    super$initialize(..., onStart = onStart_lc, 
                     allowedFunctions = allowedFunctions, 
                     allowedVariables = allowedVariables,
                     allowedDirectories = allowedDirectories)
    if(!is.null(layout))
      self$setLayout(layout)
    
    private$envir <- parent.frame(n = 2)
    invisible(self)
  }
), private = list(
  props = list(scatter = c("x", "y", "size", "stroke", "strokeWidth", "symbol", "symbolValue", "symbolLegendTitle",
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
                         "informText", "on_mouseover", "on_mouseout", "on_marked", "on_clickPosition", "mode"),
               input = c("step", "min", "max", "ncols", "nrows", "fontSize"),
               image = c("img", "src", "paths"),
               all = c("width", "height", "plotWidth", "plotHeight", "paddings", "title", "titleX", "titleY", "titleSize",
                       "showLegend", "showPanel", "transitionDuration", "value", "rowLabel", "colLabel", "showDendogramRow",
                       "clusterRows", "clusterCols", "mode", "heatmapRow", "heatmapCol", "showValue", "rowTitle", 
                       "colTitle", "palette", "colourDomain", "on_click", "on_change", "on_", "on_mouseout", "on_marked", 
                       "chart", "app", "layer", "content", "type", "domainX", "domainY", "apectRatio", "axisTitleX", "axisTitleY",
                       "logScaleX", "logScaleY", "ticksRotateX", "ticksRotateY", "globalColourScale", "aspectRatio",
                       "rankRows", "rankCols", "ticksX", "ticksY", "showDendogramCol", "on_labelClickCol", "on_labelClickRow",
                       "axisTitlePosX", "axisTitlePosY", "legend_width", "legend_height", "legend_sampleHeight", "legend_ncol",
                       "legend_titles", "legend_container", "informText", "valueTextColour", "legendTitle")),
  nameList = c("labels" = "label", "color" = "colour", "colorValue" = "colourValue",
               "colourValues" = "colourValue", "colorValues" = "colourValue", "colorDomain" = "colourDomain",
               "colorLegendTitle" = "colourLegendTitle", "addColorScaleToLegend" = "addColourScaleToLegend",
               "symbols" = "symbol", "symbolValues" = "symbolValue", "strokes" = "stroke", "values" = "value",
               "heatmapRows" = "heatmapRow", "heatmapCols" = "heatmapCol", "showValues" = "showValue",
               "globalColorScale" = "globalColourScale", "steps" = "step", "valueTextColor" = "valueTextColour"),
  jsTypes = c(scatter = "scatter", beeswarm = "beeswarm", line = "pointLine", path = "pointLine",
              ribbon = "pointRibbon", bars = "barchart", hist = "barchart", dens = "pointLine",
              heatmap = "heatmap", colourSlider = "colourSlider", abLine = "xLine", hLine = "xLine",
              vLine = "yLine", html = "html", input = "input", image = "image"),
  charts = list(),
  layout = NULL,

  addLayout = function(session) {
    if(!is.null(private$layout)) {
      pars <- str_c(private$layout[-1], collapse = ", ")
      session$sendCommand(str_interp("rlc.add${private$layout[1]}(${pars});"))
    }
    invisible(self)
  },
  
  addChart = function(chartId, place) {
    chart <- Chart$new(chartId, place, self)
    private$charts[[chartId]] <- chart
    
    message(str_interp("Chart '${chartId}' added."))
    
    invisible(chart)
  }
))

Layer <- R6Class("Layer", public = list(
  type = NULL,
  id = NULL,
  pacerStep = 50,
  init = c(),
  
  setProperty = function(name, expr = NULL) {
    if(is.null(expr))
      return(self$properties[[name]])
    
    self$properties[[name]] <- expr
  },
  initialize = function(type, id) {
    self$type <- type
    self$id <- id
    self$on_click <- function(d) {}
    self$dataFun <- function(l) {l}

    invisible(self)
  },

  properties = list(),
  dataFun = NULL,
  on_click = NULL,
  on_mouseover = NULL,
  on_mouseout = NULL,
  on_marked = NULL,
  on_labelClickRow  = NULL,
  on_labelClickCol = NULL,
  on_clickPosition = NULL
))


Chart <- R6Class("Chart", public = list(
  id = NULL,
  place = NULL,
  data.frame = NULL,
  addLayer = function(layerId, type){
    if(layerId %in% names(private$layers))
      stop(str_c("Layer with ID ", layerId, " already exists in chart ", self$id, ".", 
                 " Use 'chart$removeLayer(layerId)' to remove it."))
    
    private$layers[[layerId]] <- Layer$new(type, layerId)
    invisible(private$layers[[layerId]])    
  },
  
  getLayer = function(layerId = NULL) {
    if(is.null(layerId))
      if(length(private$layers) == 2) {
        #1st layer is always 'main', which is a dummy
        layerId <- private$layers[[2]]$id
      } else {
        stop(str_c("Chart ", self$id, " has multiple or no layers. You need to specify the 'layerID'."))
      }
    private$layers[[layerId]]
  },
  
  getLayerIds = function() {
    names(private$layers)
  }, 
  
  nLayers = function() {
    length(private$layers) - 1
  },
  
  removeLayer = function(layerId) {
    if(layerId == "main"){
      warning(str_c("You are attempting to remove the main layer of the chart ", self$id, 
                    ". The entire chart will be removed."))
      private$app$removeChart(self$id)
    } else {
      if(!(layerId %in% names(private$layers))) {
        stop(str_c("There is no layer with ID ", layerId))
      } else {
        sessionIds <- private$app$getSessionIds()
        for(id in sessionIds)
          private$app$getSession(id)$sendCommand(str_interp("rlc.removeLayer('${self$id}', '${layerId}')"))
        private$layers[[layerId]] <- NULL
      }
    }
  },
  
  update = function(sessionId, layerId = NULL, updateOnly = NULL) {
    if("Session" %in% class(sessionId))
      sessionId <- sessionId$id
    
    args <- str_c("'", self$id, "', '")
      
    if(!is.null(updateOnly)) {
        args <- str_c(args, updateOnly, "', '")
    } else {
      args <- str_c(args, "', '")
    }
      
    if(!is.null(layerId)) {
      args <- str_c(args, layerId, "'")
      private$sendProperties(sessionId, layerId)
    } else {
      private$sendProperties(sessionId)
      args <- str_c(args, "'")
    }
    if(is.vector(sessionId)){
      for(id in sessionId){
        session <- private$app$getSession(id)
        if(!is.null(session))
          session$sendCommand(str_interp("rlc.updateCharts(${args});"))
      }
    } else {
      sessionId$sendCommand(str_interp("rlc.updateCharts(${args});"))
    }
    invisible(self)
  },
  JSInitialize = function(session) {
    session$sendCommand(str_interp("rlc.prepareContainer('${self$place}');"))
    
    lapply(private$layers, function(layer) {
      if(!(session$id %in% layer$init)) {
        if(layer$id != "main")
          message(str_interp("Layer '${layer$id}' is added to chart '${self$id}'."))
        session$sendCommand(str_interp("rlc.addChart('${self$id}', '${layer$type}', '${self$place}', '${layer$id}');"))
        layer$init <- intersect(c(session$id, layer$init), private$app$getSessionIds())
      }
    })
  },  
    
  initialize = function(id, place, app) {
    self$id <- id
    self$place <- place
    private$app <- app
    invisible(self)
  }
), private = list(
  layers = list(),
  app = NULL,
  
  sendProperties = function(sessionId, layerId = ls(private$layers)){
    
    for(layerName in layerId){
      layer <- self$getLayer(layerName)
      if(is.null(layer))
        stop(str_c("There is no layer ", layerName, " in the chart ", self$id))

      name <- str_c(self$id, layer$id, sep = "_sep_")

      for(id in sessionId){
        session <- private$app$getSession(id)
        if(!is.null(session)) {

          env <- session$sessionVariables()
          
          data <- data.frame()
          if(!is.null(self$data.frame))
            tryCatch({data <- eval(self$data.frame, env)},
                     error = function(e) {
                       warning("Data table hasn't been found and will be ignored")
                     })
          
          stopifnot(is.list(data))
          
          tryCatch({
            d <- lapply(layer$properties, function(expr) eval(expr, data, enclos = env))
            d <- layer$dataFun(d)
          },
          error = function(e) 
            stop( str_interp( "in data expression for chart '${self$id}': ${e$message}." ), call.=FALSE ) )
          
          if(!is.null(d$ticksX) & !is.vector(d$ticksX))
            d$ticksX <- t(d$ticksX)
          if(!is.null(d$ticksY) & !is.vector(d$ticksY))
            d$ticksY <- t(d$ticksY)
          if(!is.null(d[["on_click"]])) {
            layer$on_click <- d$on_click
            d$on_click <- NULL
          }
          
          if(!is.null(d$on_marked)) {
            layer$on_marked <- d$on_marked
            d$maekedUpdated <- NULL
            session$sendCommand(str_interp("rlc.setCustomOnMarked('${self$id}', '${layerName}');"))
          }
          
          if(!is.null(d$on_mouseover)) {
            layer$on_mouseover <- d$on_mouseover
            d$on_mouseover <- NULL
            session$sendCommand(str_interp("rlc.setCustomMouseOver('${self$id}', '${layerName}', ${layer$pacerStep});"))
          }
          if(!is.null(d$on_mouseout)) {
            layer$on_mouseout <- d$on_mouseout
            d$on_mouseout <- NULL
            session$sendCommand(str_interp("rlc.setCustomMouseOut('${self$id}', '${layerName}', ${layer$pacerStep});"))
          }    
          if(!is.null(d$on_labelClickRow)) {
            layer$on_labelClickRow <- d$on_labelClickRow
            d$on_labelClickRow <- NULL
            session$sendCommand(str_interp("rlc.setCustomClickLabel('${self$id}', 'Row');"))
          }    
          if(!is.null(d$on_labelClickCol)) {
            layer$on_labelClickCol <- d$on_labelClickCol
            d$on_labelClickCol <- NULL
            session$sendCommand(str_interp("rlc.setCustomClickLabel('${self$id}', 'Col');"))
          }    
          
          if(!is.null(d$on_clickPosition)) {
            mLayer <- self$getLayer("main")
            mLayer$on_clickPosition <- d$on_clickPosition
            d$on_clickPosition <- NULL
            session$sendCommand(str_interp("rlc.setCustomClickPosition('${self$id}')"))
          }
          
          session$sendData(name, d)
          session$sendCommand(str_interp("rlc.setProperty('${name}')"))
        }
      }
    }
  }
))                 
                   
#' Open a new empty page
#' 
#' \code{openPage} starts a server, establishes a web socket connection between it and the current
#' R session and loads linked-charts JS library with all the dependencies. This function initializes
#' an instance of class \code{\link{LCApp}} and stores it in the namespace of the package. If another
#' instance has already been stored (i.e. another app has been started with this function), the existing
#' app will be closed.
#' 
#' Argument \code{onStart} of \code{jrc} \code{\link[jrc]{openPage}} function is replaced in \code{rlc}
#' with \code{beforeLoad} and \code{afterLoad}. The reason for that is when the page opens, \code{rlc}
#' has to put there all the existing charts. Different situations may require some code be loaded before or after
#' that happens. \code{beforeLoad} and \code{afterLoad} provide a way to define two callback functions, each 
#' receiving a \code{\link[jrc]{Session}} object as an argument and is called once for each new page.
#' \code{beforeLoad} runs before anything else has happened, while \code{afterLoad} is called after all the
#' existing charts have been added to the page.
#' 
#' This function initializes a new instance of class \code{\link{LCApp}} and wraps around methods 
#' \code{startServer} and \code{openPage} of its parent class \code{\link[jrc]{App}}.
#' 
#' @param useViewer If \code{TRUE}, a page will be opened in the RStudio Viewer. If \code{FALSE},
#' a default web browser will be used.
#' @param rootDirectory A path to the root directory for the server. Any file, requested by the server
#' will be searched for in this directory. If \code{rootDirectory} is not 
#' defined, the \code{http_root} in the package directory will be used as a root directory.
#' @param startPage A path to an HTML file that should be used as a starting page of the app.
#' It can be an absolute path to a local file, or it can be relative to the \code{rootDirectory}
#' or to the current R working directory. If \code{startPage} is not defined, an empty page will be used.
#' The file must have \emph{.html} extension.
#' @param layout Adds one of the defaults layouts to each new page. Currently, only tables of arbitrary 
#' size are supported. To add a table, this parameter must be equal to \code{"tableNxM"}, where \code{N} is the
#' number of rows and \code{M} is the number of columns. Each cell will get an ID that consists of 
#' a letter (indicating the row) and a number (indicating the column) (e.g. \code{B3} is an ID of 
#' the second row and third column).
#' @param port Defines which TCP port the server will listen to. If not defined, random available port
#' will be used (see \code{\link[httpuv]{randomPort}}).
#' @param browser A browser in which to open a new web page.
#' If not defined, default browser will be used. For more information check \code{\link[utils]{browseURL}}.
#' If this argument is specified, \code{useViewer} will be ignored.
#' @param onlyServer If \code{TRUE}, then an app will initialise without trying to open a new page in a browser.
#' @param ... Further arguments passed to \code{\link[jrc]{openPage}}. Check details for more information.
#' 
#' @return A new instance of class \code{\link{LCApp}}.
#' 
#' @examples
#' \dontrun{openPage()
#' 
#' openPage(useViewer = FALSE, layout = "table2x3")}
#' 
#' @export
openPage <- function(useViewer = TRUE, rootDirectory = NULL, startPage = NULL, layout = NULL, port = NULL, 
                     browser = NULL, onlyServer = FALSE, ...) {
  if(!is.null(pkg.env$app)) 
    closePage()
  
  app <- LCApp$new(rootDirectory = rootDirectory, startPage = startPage, layout = layout, ...)
  app$startServer(port)
  if(!onlyServer)
    app$openPage(useViewer, browser)
  app$setEnvironment(parent.frame())
  
  pkg.env$app <- app
  
  invisible(app)
}

getAppAndSession <- function(sessionId = NULL, app = NULL, all = TRUE) {
  
  if(identical(parent.frame(n = 2), .GlobalEnv)){
    env <- .GlobalEnv
  } else {
    env <- parent.env(parent.frame(n = 2))
  }
    
  
  if(is.null(app))
    tryCatch(app <- get(".app", env, inherits = FALSE), 
           error = function(e) {
             if(is.null(pkg.env$app))
               stop("There is no opened page. Please, use 'openPage()' function to create one.") 
             app <<- pkg.env$app
           })
  
  if(is.null(sessionId))
    tryCatch(sessionId <- get(".id", env, inherits = FALSE), 
      error = function(e) {
        if(!all & length(app$getSessionIds()) != 1)
          stop("There are several active sessions. Please, specify the session ID")
        sessionId <<- app$getSessionIds()
      })

  list(app = app, sessionId = sessionId)
}

#' Remove chart from the page
#' 
#' Removes an existing chart. Changes will be applied to all currently opened and future pages.
#' This function is a wrapper around method \code{removeChart} of
#' class \code{\link{LCApp}}. 
#' 
#' @param chartId A vector of IDs of the charts to be removed.
#' 
#' @examples 
#' \dontrun{lc_scatter(dat(x = 1:10, y = 1:10 * 2), chartId = "scatter")
#' removeChart("scatter")}
#' 
#' @export
removeChart <- function(chartId) {
  workWith <- getAppAndSession()
  
  workWith$app$removeChart(chartId)
}

#' Remove a layer from a chart
#' 
#' Removes a layer from an existing chart. Changes will be applied to all currently opened and future pages.
#' This function is a wrapper around method \code{removeLayer} of
#' class \code{\link{LCApp}}.
#' 
#' @param chartId ID of the chart from which to remove a layer.
#' @param layerId ID of the layer to remove.
#' 
#' @examples 
#' \dontrun{lc_scatter(dat(x = 1:10, y = 1:10 * 2), chartId = "scatter")
#' lc_abLine(a = 2, b = 0, chartId = "scatter", addLayer = TRUE)
#' removeLayer("scatter", "Layer1")}
#' 
#' @export
removeLayer <- function(chartId, layerId) {
  workWith <- getAppAndSession()
  
  workWith$app$removeLayer(chartId, layerId)
}

#' Set properties of the chart
#' 
#' Sets or resets properties for an
#' existing chart. Changes will be applied to all currently opened and future pages.
#' This function is a wrapper around method \code{setProperties} of class \code{\link{LCApp}}.
#' 
#' @param data List of properties to be redefined for this layer or chart. Created by the \code{\link{dat}}
#' function.
#' @param chartId ID of the chart, for which to redefine properties.
#' @param layerId ID of the layer, for which to redefine properties. If the chart has a single
#' layer or doesn't have layers, default value (which is NULL) can be used.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' 
#' @examples
#' \dontrun{data("iris")
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Sepal.Width), chartId = "irisScatter")
#' setProperties(dat(symbolValue = iris$Species, y = iris$Petal.Length), chartId = "irisScatter")
#' updateCharts("irisScatter")
#' 
#' lc_line(dat(x = iris$Sepal.Length, y = iris$Petal.Length), chartId = "irisScatter", 
#'         layerId = "line")
#' setProperties(dat(colour = "red"), chartId = "irisScatter", layerId = "line")
#' updateCharts("irisScatter")}
#' 
#' @export
#' @importFrom plyr rename
setProperties <- function(data, chartId, layerId = NULL, with = NULL) {
  workWith <- getAppAndSession()
  workWith$app$setProperties(data, chartId, layerId, with)
}

#' Update a chart
#' 
#' \code{updateCharts} redraws a chart or a single layer of a chart to make it up
#' to date with the current state of the environment variables.
#' 
#' Linked charts of the \emph{rlc} package are based on the idea that the variables that are
#' used to define a chart are not constant, but can change as a result of user's
#' actions. Each time the \code{updateCharts} function is called, all the properties that were set inside
#' the \code{\link{dat}} function are re-evaluated and the chart is redrawn in accordance with the
#' new state.
#' 
#' If this function is called from the R session, changes will be applied
#' to all currently opened pages. If it is used as a part of any \code{rlc} callback, only the page
#' that triggered the call will be affected.
#' 
#' This function is a wrapper around method \code{updateCharts} of class \code{\link{LCApp}}.
#' 
#' @section Update types: 
#' To improve performance you can update only a certain part of a chart (e.g. colours,
#' size, etc.). This can be done by setting the \code{updateOnly} argument. Here are all
#' possible values for this argument.
#' 
#' These are valid for all the charts:
#' \itemize{
#'   \item \code{Size} changes the size of the chart (and consequently position
#'   of all its elements).
#'   \item \code{Title} changes the title of the chart.
#'   \item \code{Canvas} If number of elements is too high the 
#'   charts switch to the canvas mode and instead of multiple SVG point or cells
#'   a single Canvas image is generated. This type of update redraws the Canvas
#'   image. \emph{It is not recommended to use this option, since it will be used automatically when necessary.}
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
#'   \item \code{Axes} updates axes of a chart and changes position 
#'   of its elements (points, lines, etc.) accordingly.
#'   \item \code{Elements} updates (add or removes) all the elements of the layer.
#'   \item \code{ElementPosition} updates positions of all the elements in the layer.
#'   \item \code{ElementStyle} updates the style (colour, opacity, etc.) of all the elements 
#'   of the layer.
#' }
#' 
#' 
#' @param chartId ID of the chart to be updated (or vector of IDs). If \code{NULL}, all the
#' existing charts will be updated.
#' 
#' @param layerId ID of the layer to be updated (or vector of IDs). If \code{NULL}, all the
#' layers of the selected charts will be updated. To update only some layers of
#' multiple charts the lengths of \code{chartId} and \code{layerId} must be the same.
#' 
#' @param updateOnly To improve performance it may be useful to change only certain 
#' aspects of a chart (e.g. positions of points, colour of heatmap cells,
#' etc.). This argument can specify which part of chart to update. Possible options are
#' \code{Elements}, \code{ElementPosition}, \code{ElementStyle}, \code{Axes}, \code{Labels},
#' \code{Cells}, \code{Texts}, \code{LabelPosition}, \code{CellPosition}, 
#' \code{TextPosition}, \code{LabelText}, \code{CellColour}, \code{TextValues},
#' \code{Canvas}, \code{Size}. See details for more information.
#' 
#' @examples
#' \dontrun{data(iris)
#'
#' #store some properties in global variables
#' width <- 300
#' height <- 300
#' colour <- iris$Sepal.Width
#' #create a chart
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length, colourValue = colour,
#'                width = width, height = height), chartId = "iris")
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
updateCharts <- function(chartId = NULL, layerId = NULL, updateOnly = NULL) {
  workWith <- getAppAndSession()
  
  workWith$app$updateCharts(chartId, layerId, updateOnly, workWith$sessionId)
}

#' Trigger an event
#' 
#' This function is called whenever any interactive element of a chart is activated by clicking, marking,
#' hovering, etc. In turn,
#' it calls a corresponding callback function, if any has been specified. This function
#' is meant to be used internally. However, an experienced user can still use it to simulate mouse events, 
#' even those triggered by non-existing elements. This function is a wrapper around method 
#' \code{chartEvent} of class \code{\link{LCApp}}.
#' 
#' @param d Value that is used to identify an interactive element or its state. 
#' A single numeric index for a point or a line, vector or row and column indices 
#' of a cell for a heatmap, value for an input block (please, check \code{\link{lc_input}} for more 
#' details about input blocks and their values). It should be \code{NULL} for \code{mouseout} or 
#' \code{marked} events. NB: This function is called from the web page, and therefore all element indices 
#' start from zero as it happens in JavaScript.
#' @param chartId ID of the chart.
#' @param layerId ID of the layer. You can print IDs of all charts and their layers with \code{\link{listCharts}}.
#' @param event Type of event. Must be one of \code{"click"}, \code{"mouseover"}, \code{"mouseout"}, \code{"marked"}, 
#' \code{"labelClickRow"}, \code{"labelClickCol"}, \code{"clickPosition"}.
#' @param sessionId ID of the session (opened client page) that triggered the event. The default value uses a local session
#' variable. This must be a single session ID. You can get a list of IDs of all currently active with the
#' method \code{getSessionIds} inherited from class \code{\link{App}} by \code{\link{LCApp}}. Possible errors in the evaluation of 
#' this argument are ignored.
#' @param app Object of class \code{\link{LCApp}} for which the event was triggered. Note that this argument is here for
#' internal use, and its default value is a variable stored in each session locally. If you are not using wrapper functions, 
#' it is preferred to call method \code{chartEvent} of an object of class \code{\link{LCApp}}.
#' @examples 
#' \dontrun{x <- rnorm(50)
#' lc_scatter(x = x, y = 2*x + rnorm(50, 0.1), on_click = function(d) print(d))
#' chartEvent(51, "Chart1", "Layer1", "click")}
#' 
#' @export
#' @importFrom utils type.convert
chartEvent <- function(d, chartId, layerId = "main", event, sessionId = .id, app = .app) {
  workWith <- list()
  tryCatch(workWith$app <- app, error = function(e) {})
  tryCatch(workWith$sessionId <- sessionId, error = function(e) {})
  workWith <- getAppAndSession(app = workWith$app, sessionId = workWith$sessionId, all = FALSE)

  if(length(d) == 1)
    if(d == "NULL")
      d <- NULL
  
  workWith$app$chartEvent(d, chartId, layerId, event, workWith$sessionId)
}

#' List all existing charts and layers
#' 
#' \code{listCharts} prints a list of IDs of all existing charts and layers.
#' This function is wrapper around method \code{listCharts} of class \code{\link{LCApp}}.
#' 
#' @examples 
#' \dontrun{noise <- rnorm(30)
#' x <- seq(-4, 4, length.out = 30)
#' 
#' lc_scatter(dat(x = x,
#'                y = sin(x) + noise,
#'                colourValue = noise), 
#'            chartId = "plot", layerId = "points")
#' lc_line(dat(x = x, y = sin(x)), chartId = "plot", addLayer = TRUE)
#' lc_colourSlider(chart = "plot", layer = "points")
#' 
#' listCharts()}
#' @export
listCharts <- function() {
  workWith <- getAppAndSession()
  
  workWith$app$listCharts()
}

#' Get currently marked elements
#' 
#' \code{getMarked} returns indices of the chart's elements that are currently
#' marked. To mark elements select them with your mouse while holding the \emph{Shift} key.
#' Double click on the chart with the \emph{Shift} key pressed will deselect all the 
#' elements. This function is a wrapper of method \code{getMarked} of class \code{\link{LCApp}}.
#' 
#' @param chartId An ID of the chart. This argument is optional if there is only one chart.
#' @param layerId An ID of the layer. This argument is optional if there is only one chart with
#' a single layer.
#' @param sessionId An ID of the session from which to get the marked elements. It can be \code{NULL}
#' if there is only one active session. Otherwise must be a valid session ID. Check \code{\link[jrc]{Session}}
#' for more information on client sessions. If a call to this function was triggered from a web page, the ID of 
#' the corresponding session would be used automatically.
#' 
#' @return a vector of indices or, in the case of heatmaps, an \emph{n x 2} matrix where first and
#' second columns contain row and column indices of the marked cells, respectively.
#' 
#' @examples
#' \dontrun{data(iris)
#' 
#' lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length))
#' 
#' #now mark some points by selecting them with your mouse with Shift pressed
#' 
#' getMarked("Chart1")}
#' 
#' @export
getMarked <- function(chartId = NULL, layerId = NULL, sessionId = NULL) {
  workWith <- getAppAndSession(sessionId = sessionId, all = FALSE)
  
  workWith$app$getMarked(chartId, layerId, workWith$sessionId)
}

#' Mark elements of a chart
#' 
#' \code{mark} selects a set of elements in a given chart. It is equivalent to
#' selecting elements interactively by drawing a rectangle with the mouse 
#' while holding the \code{Shift} key. This function is a wrapper of method \code{mark} of
#' class \code{\link{LCApp}}.
#' 
#' @param elements numeric vector of indices of the elements to select.
#' @param clear if \code{TRUE}, all previously marked elements will be unmarked,
#' otherwise new elements will be added to a set of currently marked ones.
#' @param chartId ID of the chart where to select elements (can be omitted if 
#' there is only one chart).
#' @param layerId ID of the layer where to select elements (can be omitted if
#' the chart has only one layer).
#' @param preventEvent if \code{TRUE}, \code{on_marked} callback function will not be
#' called. Can be used to prevent endless stacks of calls.
#' @param sessionId An ID of the session for which to mark elements. Can be \code{NULL}
#' if there is only one active session. Otherwise must be a valid session ID. Check \code{\link[jrc]{Session}}
#' for more information on client sessions. If a call to this function was triggered from an opened web page, ID of 
#' the corresponding session will be used automatically.
#'
#' @examples 
#' \dontrun{data("iris")
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
mark <- function(elements = NULL, chartId = NULL, layerId = NULL, 
                 preventEvent = TRUE, clear = FALSE, sessionId = NULL) {
  workWith <- getAppAndSession(sessionId)

  workWith$app$mark(elements, chartId, layerId, preventEvent, clear, workWith$sessionId)
}

#' Get the currently running app
#' 
#' \code{rlc} offers two ways to control an interactive app. One is by using methods of class
#' \code{\link{LCApp}}. This allows one to have any number of apps within one
#' R session but requires some understanding of object oriented-programming. Another way is to use
#' provided wrapper functions that are exported by the package. These functions internally work with 
#' the \code{\link{LCApp}} object stored in the package namespace upon initialization with the
#' \code{\link{openPage}} function. \code{getPage} returns this object, if any.
#' 
#' Note that the \code{rlc} package is based on the \code{jrc} library. Both packages are similarly organized.
#' Both have a central class representing the entire app and can be fully managed with their methods (\code{\link{LCApp}}
#' and \code{\link[jrc]{App}}, respectively). And both also provide a set of wrapper functions that can be used instead of
#' the methods. However, wrapper functions of the \code{jrc} package can't be used for \code{rlc} apps, while \code{\link{LCApp}} 
#' inherits all the methods of class \code{\link[jrc]{App}}. Therefore, if you want to get more low-level
#' control over your app, such as managing client sessions, local variables and memory usage, you should use methods of the
#' \code{\link[jrc]{App}} class. 
#' 
#' @return An object of class \code{\link{LCApp}} or \code{NULL} if there is no active app.
#' 
#' @export
getPage <- function(){
  pkg.env$app
}

#' Listen to the server
#' 
#' When R session is not interactive, messages from the server are not processed automatically. In this case, one needs to 
#' keep this function running.
#' This function, is a wrapper around \code{\link[jrc]{listen}}.
#' 
#' @param time Time (in seconds), during which the R session should listen to the server. By default, the function runs until
#' it is not interrupted (\code{time = Inf}).
#' @param activeSessions The function runs, until there is at least one active session in the provided app. If there is only
#' one active app, this argument can be set to \code{TRUE} for the same effect.
#' @param condition Custom condition. This argument must be a function that returns \code{TRUE} or \code{FALSE}. R session will 
#' listen to the server, while the condition function returns \code{TRUE}.
#' 
#' @importFrom jrc listen
#' @export
listen <- function(time = Inf, activeSessions = NULL, condition = NULL) {
  if(isTRUE(activeSessions)) {
    if(is.null(pkg.env$app)) 
      stop("There is no opened page. Please, use 'openPage()' function to create one.")
    activeSessions <- pkg.env$app
  }
  
  jrc::listen(time = time, activeSessions = activeSessions, condition = condition)
}

#' Link data to the chart
#' 
#' \code{dat} allows linking variables from the current environment to chart's properties.
#' On every \code{\link{updateCharts}} call, all the data provided via the \code{dat} function
#' will be automatically re-evaluated, and the chart will be changed accordingly. One can also
#' put properties outside of the \code{dat} function to prevent their re-evaluation. It
#' can also be used to ensure re-evaluation of the \code{with} argument of any plotting function.
#' 
#' @param ... List of name-value pairs to define the properties. 
#' 
#' @examples 
#' \dontrun{lc_scatter(dat(x = rnorm(30)), y = rnorm(30))
#' #note that the Y values remain the same after each updateCharts call
#' updateCharts()
#' 
#' #This way the dataset is not strored inside the chart and will be re-evaluated
#' data("iris")
#' lc_scatter(dat(x = Sepal.Length, y = Petal.Length), with = dat(iris))
#' 
#' iris <- iris[1:10, ]
#' updateCharts()
#' }
#' 
#' @export
dat <- function( ... ) {
  as.list(match.call()[-1])
}

#' Stop server
#' 
#' Stops the server and closes all currently opened pages (if any). This function is a 
#' wrapper of the \code{stopServer} method inherited by the \code{\link{LCApp}} class from the \code{\link[jrc]{App}} class.
#' 
#' @examples 
#' \dontrun{openPage(useViewer = FALSE)
#' closePage()}
#' 
#' @export
closePage <- function() {
  if(!is.null(pkg.env$app)) {
    pkg.env$app$stopServer()
    pkg.env$app <- NULL
  } else {
    message("There is no opened page.")
  }
}

#' Visualize a set of points
#' 
#' These functions plot a set of points with known coordinates that can be either categorical,
#' or continuous. 
#' 
#' @describeIn lc_scatter creates a scatterplot and adds it as a new layer to an existing chart or
#' creates a new one.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced unless \code{addLayer = TRUE}. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, it will be set to \code{LayerN}, where \code{N - 1} 
#' is the current number of layers in this chart.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' @param addLayer If there is already a chart with the same ID, this argument defines whether to replace it or to add a
#' new layer to it. This argument is ignored if both \code{place} and \code{chartId} are \code{NULL} or if there is no
#' chart with the given ID. 
#' @param pacerStep Time in ms between two consecutive calls of an \code{onmouseover} event. Prevents over-queueing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{x, y} - vector of x and y coordinates of the points.
#'  \item \code{size} - sizes of the points. Default size is 6.
#'  \item \code{opacity} - a vector of opacity values for each point in the range from 0 to 1.
#'  \item \code{label} - vector of text labels for each point (labels by default are shown, when mouse hovers over a point).
#'  \item \code{valueAxis} - (for \code{lc_beeswarm} only) defines axis with values that will
#'  not be changed. Must be \code{"x"} or \code{"y"} (default).
#'  \item \code{informText} - text that appears when the mouse cursor moves over an element. Unlike \code{label},
#'  completely overwrites the tooltip content with a custom HTML code. Must be a vector of characters (HTML code
#'  for each element).} 
#' 
#' Colour and shape settings
#' \itemize{
#'  \item \code{colour} - colour of the points. Must be a colour name or a hexadecimal code.
#'  \item \code{colourValue} - grouping values for different colours. Can be numbers or characters.
#'  \item \code{colourDomain} - a vector of all possible values for discrete colour scales 
#'  or a range of all possible colour values for the continuous ones.
#'  \item \code{palette} - a vector of colours to construct the colour scale.
#'  \item \code{colourLegendTitle} - a title for the colour legend.
#'  \item \code{addColourScaleToLegend} - whether or not to show the colour legend for the current layer.
#'  \item \code{globalColourScale} - whether or not to use one colour scale for all the layers.
#'  \item \code{symbol} - shape of each point. Must be one of \code{"Circle"}, \code{"Cross"}, \code{"Diamond"}, 
#'  \code{"Square"}, \code{"Star"}, \code{"Triangle"}, \code{"Wye"}.
#'  \item \code{symbolValue} - grouping values for different symbols.
#'  \item \code{symbolLegendTitle} - a title for the symbol value.
#'  \item \code{stroke} - stroke colour for each element. Must be a colour name or a hexadecimal code.
#'  \item \code{strokeWidth} - width of the strokes for each point.} 
#'  
#' Axes settings
#' \itemize{
#'  \item \code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.
#'  \item \code{jitterX, jitterY} - amount of random variation to be added to the position of the
#'  points along one of the axes. 0 means no variation. 1 stands for distance between \code{x} and
#'  \code{x + 1} for linear scale, \code{x} and \code{b*x} for logarithmic scale (\code{b} is a base
#'  of the logarithm), or between neighbouring ticks for categorical scale.
#'  \item \code{shiftX, shiftY} - shift for each point from its original position along one of the
#'  axes. 0 means no shift. 1 stands for distance between \code{x} and
#'  \code{x + 1} for linear scale, \code{x} and \code{b*x} for logarithmic scale (\code{b} is a base
#'  of the logarithm), or between neighbouring ticks for categorical scale.
#'  \item \code{layerDomainX, layerDomainY} - default axes ranges for the given layer.
#'  \item \code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  it is automatically set to include all layer domains.
#'  \item \code{contScaleX, contScaleY} - whether or not the axis should be continuous.
#'  \item \code{aspectRatio} - an aspect ratio for the chart.
#'  \item \code{axisTitleX, axisTitleY} - axis titles.
#'  \item \code{axisTitlePosX, axisTitlePosY} - positions of the axis titles. For each axis, one can specify a title position
#'  across or along the corresponding axis. Possible options are \code{"up"} (for title inside the plotting area)
#'  or \code{"down"} (outside the plotting area, under the axis), and
#'  \code{"start"}, \code{"middle"}, \code{"end"}. This property must be a string with one or two of the aforementioned options
#'  (e.g. \code{"middle down"}, \code{"start"}, etc.).
#'  \item \code{ticksRotateX, ticksRotateY} - angles by which to rotate ticks (in degrees). Must be between 
#'  0 (horizontal ticks, default) and 90 (vertical ticks).
#'  \item \code{ticksX, ticksY} - sets of ticks for the axes.} 
#'
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - a function, to be called when one of the points is clicked. Gets an
#'  index of the clicked point as an argument.
#'  \item \code{on_clickPosition} - a function, to be called when any point of the chart is clicked. Unlike
#'  \code{on_click}, which is called only when an element of the chart (point, line, etc.) is clicked, this
#'  function reacts to any click on the chart. As an argument, it receives a vector of x and y coordinates of
#'  the click (based on the current axes scales). If one of the axes is categorical, the function will
#'  get the closest tick to the clicked position.
#'  \item \code{on_mouseover} - a function, to be called when the mouse hovers over one of the points.
#'  Gets an index of the clicked point as an argument.
#'  \item \code{on_mouseout} - a function, to be called when the mouse moves out of one of the points.
#'  \item \code{on_marked} - a function, to be called when any of the points are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked points. To mark points,
#'  select them with your mouse while holding the \emph{Shift} key.} 
#'
#' Legend settings
#' \itemize{
#'  \item \code{legend_width} - width of the legend in pixels. The default value is 200.
#'  \item \code{legend_height} - height of the legend in pixels. By default, it is equal to the height of the chart.
#'  \item \code{legend_sampleHeight} - height of a single key of the legend in pixels. The default value is 20.
#'  \item \code{legend_ncol} - number of columns to order several legends. By default, this is defined from the number 
#'  of legends to reach close to a square shape.
#'  \item \code{legend_container} - a DOM element of the web page where to place the legend. By default, the legend is 
#'  positioned to the right from the chart in a table cell specifically made for it. This should be a valid CSS selector.
#'  If the specified element does not exist, the legend will be added to the web page's body.} 
#'    
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels. 
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - a title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend.
#'  \item \code{showPanel} - whether of not to show the instrument panel (grey triangle in the upper-left corner of the chart).
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.} 
#' 
#' @examples
#' \dontrun{data("iris")
#' lc_scatter(dat(x = Sepal.Length, 
#'                y = Petal.Length,
#'                colourValue = Petal.Width,
#'                symbolValue = Species),
#'            with = iris,
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
lc_scatter <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("scatter", data, ...,  place = place, chartId = chartId, layerId = layerId, addLayer = addLayer,
           with = with, pacerStep = pacerStep)
}

#' @describeIn lc_scatter creates a special kind of scatterplot, where the points are spread along one of 
#' the axes to avoid overlapping.
#' 
#' @export
lc_beeswarm <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("beeswarm", data, ..., place = place, chartId = chartId, layerId = layerId, with = with,
                       addLayer = addLayer, pacerStep = pacerStep)
}

#' Lines and ribbons
#' 
#' These functions create various kinds of lines. They connect observations or 
#' create filled areas with customized border. Each layer may have one or several lines.
#' 
#' @describeIn lc_line connects points in the order of variables on the x axis.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced unless \code{addLayer = TRUE}. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, it will be set to \code{LayerN}, where \code{N - 1} 
#' is the current number of layers in this chart.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' @param addLayer If there is already a chart with the same ID, this argument defines whether to replace it or to add a
#' new layer to it. This argument is ignored if both \code{place} and \code{chartId} are \code{NULL} or if there is no
#' chart with the given ID.
#' @param pacerStep Time in ms between two consecutive calls of an \code{on_mouseover} event. Prevents over-queueing in case
#' of cumbersome computations. May be important when the chart works in canvas mode. 
#' 
#' @section Available properties: 
#' You can read more about different properties
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{x, y} - vector of x and y coordinates of the points to connect. Can be 
#'  vectors for a single line or \code{m x n} matrix for \code{n} lines.
#'  \item \code{ymax, ymin} - (only for \code{lc_ribbon}) vectors of maximal and minimal values for a ribbon.
#'  \item \code{a, b} - (only for \code{lc_abLine}) vectors of slope and intercept values respectively.
#'  \item \code{v} - (only for \code{lc_vLine}) vector of x-intercepts.
#'  \item \code{h} - (only for \code{lc_hLine}) vector of y-intercepts.
#'  \item \code{lineWidth} - (nor for \code{lc_ribbon}) width of each line.
#'  \item \code{opacity} - a vector of opacity values for each line in the range from 0 to 1.
#'  \item \code{label} - vector of text labels for each line (labels by default are shown, when mouse hovers over a line).
#'  \item \code{dasharray} - defines pattern of dashes and gaps for each line.
#'  \item \code{informText} - text that appears when the mouse cursor moves over an element. Unlike \code{label},
#'  completely overwrites the tooltip content with a custom HTML code. Must be a vector of characters (HTML code
#'  for each element).}
#' 
#' Colour settings
#' \itemize{
#'  \item \code{colour} - colour of the lines. Must be a colour name or a hexadecimal code. For
#'  \code{lc_ribbon} this property defines colour of the ribbon, not the strokes.
#'  \item \code{fill} - (not for \code{lc_ribbon}) colour with which to fill area inside the line. 
#'  Must be a colour name or a hexadecimal code.
#'  \item \code{colourValue} - grouping values for different colours. Can be numbers or characters.
#'  \item \code{colourDomain} - a vector of all possible values for discrete colour scales 
#'  or a range of all possible colour values for the continuous ones.
#'  \item \code{palette} - a vector of colours to construct the colour scale.
#'  \item \code{colourLegendTitle} - a title for the colour legend.
#'  \item \code{addColourScaleToLegend} - whether or not to show the colour legend for the current layer.
#'  \item \code{globalColourScale} - whether or not to use one colour scale for all the layers.
#'  \item \code{stroke} - (only for \code{lc_ribbon}) stroke colour for each ribbon. 
#'  Must be a colour name or a hexadecimal code.
#'  \item \code{strokeWidth} - (only for \code{lc_ribbon}) width of the strokes for each ribbon. }
#'  
#' Axes settings
#' \itemize{
#'  \item \code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.
#'  \item \code{layerDomainX, layerDomainY} - default axes ranges for the given layer.
#'  \item \code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  it is automatically set to include all layer domains.
#'  \item \code{contScaleX, contScaleY} - whether or not the axis should be continuous.
#'  \item \code{aspectRatio} - an aspect ratio for the chart.
#'  \item \code{axisTitleX, axisTitleY} - axis titles.
#'  \item \code{axisTitlePosX, axisTitlePosY} - positions of the axis titles. For each axis, one can specify a title position
#'  across or along the corresponding axis. Possible options are \code{"up"} (for title inside the plotting area)
#'  or \code{"down"} (outside the plotting area, under the axis), and
#'  \code{"start"}, \code{"middle"}, \code{"end"}. This property must be a string with one or two of the aforementioned options
#'  (e.g. \code{"middle down"}, \code{"start"}, etc.).
#'  \item \code{ticksRotateX, ticksRotateY} - angles by which to rotate ticks (in degrees). Must be between 
#'  0 (horizontal ticks, default) and 90 (vertical ticks).
#'  \item \code{ticksX, ticksY} - sets of ticks for the axes.} 
#'
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - a function, to be called when one of the lines is clicked. Gets an
#'  index of the clicked line as an argument.
#'  \item \code{on_clickPosition} - a function, to be called when any point of the chart is clicked. Unlike
#'  \code{on_click}, which is called only when an element of the chart (point, line, etc.) is clicked, this
#'  function reacts to any click on the chart. As an argument, it receives a vector of x and y coordinates of
#'  the click (based on the current axes scales). If one of the axes is categorical, the function will
#'  get the closest tick to the clicked position.
#'  \item \code{on_mouseover} - a function, to be called when the mouse hovers over one of the lines.
#'  Gets an index of the clicked line as an argument.
#'  \item \code{on_mouseout} - a function, to be called when the mouse moves out of one of the lines.
#'  \item \code{on_marked} - a function, to be called when any of the lines are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked lines. To mark lines,
#'  select them with your mouse while holding the \emph{Shift} key.} 
#'  
#' Legend settings
#' \itemize{
#'  \item \code{legend_width} - width of the legend in pixels. The default value is 200.
#'  \item \code{legend_height} - height of the legend in pixels. By default, it is equal to the height of the chart.
#'  \item \code{legend_sampleHeight} - height of a single key of the legend in pixels. The default value is 20.
#'  \item \code{legend_ncol} - number of columns to order several legends. By default, this is defined from the number 
#'  of legends to reach close to a square shape.
#'  \item \code{legend_container} - a DOM element of the web page where to place the legend. By default, the legend is 
#'  positioned to the right from the chart in a table cell specifically made for it. This should be a valid CSS selector.
#'  If the specified element does not exist, the legend will be added to the web page's body.}\
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - a title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend.
#'  \item \code{showPanel} - whether of not to show the instrument panel (grey triangle in the upper-left corner of the chart).
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.
#' } 
#' 
#' @examples 
#' \dontrun{x <- seq(0, 8, 0.2)
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
#'               colour = "#555555"), chartId = "ribbonTest")
#' lc_scatter(dat(x = x, y = y), size = 2, chartId = "ribbonTest", addLayer = TRUE)
#' lc_abLine(dat(a = fit$coefficients[2], b = fit$coefficients[1]), 
#'           chartId = "ribbonTest", addLayer = TRUE)
#' 
#' lc_hLine(dat(h = seq(1, 9, 1), domainX = c(0, 10), domainY = c(0, 10)), chartId = "grid")
#' lc_vLine(dat(v = seq(1, 9, 1)), chartId = "grid", addLayer = TRUE)}
#' 
#' @export
lc_line <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, 
                    with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("line", data, ..., place = place, chartId = chartId, layerId = layerId, 
                       with = with, addLayer = addLayer, pacerStep = pacerStep)
}

#' @describeIn lc_line connects points in the order they are given.
#' @export
lc_path <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, 
                    with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("path", data, ..., place = place, chartId = chartId, layerId = layerId, 
                       with = with, addLayer = addLayer, pacerStep = pacerStep)
}

#' @describeIn lc_line displays a filled area, defined by \code{ymax} and \code{ymin} values.
#' @export
lc_ribbon <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, with = NULL, addLayer = FALSE) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("ribbon", data, ...,  place = place, chartId = chartId, layerId = layerId, with = with, addLayer = addLayer)
}

#' Create a barplot
#' 
#' \code{lc_bars} creates a new barplot and adds it to the app and all currently opened pages
#' as a new chart or a new layer of an existing chart.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced unless \code{addLayer = TRUE}. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, it will be set to \code{LayerN}, where \code{N - 1} 
#' is the current number of layers in this chart.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' @param addLayer If there is already a chart with the same ID, this argument defines whether to replace it or to add a
#' new layer to it. This argument is ignored if both \code{place} and \code{chartId} are \code{NULL} or if there is no
#' chart with the given ID.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{values} - heights of bars/stacks.
#'  \item \code{stackIds} - IDs of all stacks (\emph{optional}). Must be the same size as \code{values}.
#'  \item \code{barIds} - IDs of all bars (\emph{optional}). Must be the same size as \code{values}.
#'  \item \code{groupIds} - IDs of all groups (\emph{optional}). Must be the same size as \code{values}.
#'  \item \code{groupWidth} - a ratio of the width of a group of bars to the space available to the group. }
#' 
#' Style settings
#' \itemize{
#'  \item \code{opacity} - a vector of opacity values for each bar or stack in the range from 0 to 1.
#'  \item \code{colour} - a vector of colours for each bar or stack. Must be a colour name or a hexadecimal code.
#'  \item \code{colourValue} - grouping values for different colours. Can be numbers or characters.
#'  \item \code{colourDomain} - a vector of all possible values for discrete colour scales 
#'  or a range of all possible colour values for the continuous ones.
#'  \item \code{palette} - a vector of colours to construct the colour scale.
#'  \item \code{colourLegendTitle} - a title for the colour legend.
#'  \item \code{addColourScaleToLegend} - whether or not to show the colour legend for the current layer.
#'  \item \code{globalColourScale} - whether or not to use one colour scale for all the layers.
#'  \item \code{stroke} - a vector of stroke colours for each bar or stack. Must be a colour name or a hexadecimal code.
#'  \item \code{strokeWidth} - a vector of stroke widths for each bar or stack. }
#'  
#' Axes settings
#' \itemize{
#'  \item \code{logScaleX, logScaleY} - a base of logarithm for logarithmic scale transformation.
#'  If 0 or \code{FALSE} no transformation will be performed.
#'  \item \code{layerDomainX, layerDomainY} - default axes ranges for the given layer.
#'  \item \code{domainX, domainY} - default axes ranges for the entire chart. If not defined, 
#'  it is automatically set to include all layer domains.
#'  \item \code{contScaleX, contScaleY} - whether or not the axis should be continuous.
#'  \item \code{aspectRatio} - an aspect ratio for the chart.
#'  \item \code{axisTitleX, axisTitleY} - axis titles.
#'  \item \code{axisTitlePosX, axisTitlePosY} - positions of the axis titles. For each axis, one can specify a title position
#'  across or along the corresponding axis. Possible options are \code{"up"} (for title inside the plotting area)
#'  or \code{"down"} (outside the plotting area, under the axis), and
#'  \code{"start"}, \code{"middle"}, \code{"end"}. This property must be a string with one or two of the aforementioned options
#'  (e.g. \code{"middle down"}, \code{"start"}, etc.).
#'  \item \code{ticksRotateX, ticksRotateY} - angles by which to rotate ticks (in degrees). Must be between 
#'  0 (horizontal ticks, default) and 90 (vertical ticks).
#'  \item \code{ticksX, ticksY} - sets of ticks for the axes.}
#'  
#'
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - a function, to be called when one of the bars is clicked. Gets an
#'  index of the clicked bar as an argument.
#'  \item \code{on_clickPosition} - a function, to be called when any point of the chart is clicked. Unlike
#'  \code{on_click}, which is called only when an element of the chart (point, line, etc.) is clicked, this
#'  function reacts to any click on the chart. As an argument, it receives a vector of x and y coordinates of
#'  the click (based on the current axes scales). If one of the axes is categorical, the function will
#'  get the closest tick to the clicked position.
#'  \item \code{on_mouseover} - a function, to be called when the mouse hovers over one of the bars.
#'  Gets an index of the clicked bar as an argument.
#'  \item \code{on_mouseout} - a function, to be called when the mouse moves out of one of the bars.
#'  \item \code{on_marked} - a function, to be called when any of the bars are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked bars. To mark bars,
#'  select them with your mouse while holding the \emph{Shift} key.} 
#'  
#' Legend settings
#' \itemize{
#'  \item \code{legend_width} - width of the legend in pixels. The default value is 200.
#'  \item \code{legend_height} - height of the legend in pixels. By default, it is equal to the height of the chart.
#'  \item \code{legend_sampleHeight} - height of a single key of the legend in pixels. The default value is 20.
#'  \item \code{legend_ncol} - number of columns to order several legends. By default, this is defined from the number 
#'  of legends to reach close to a square shape.
#'  \item \code{legend_container} - a DOM element of the web page where to place the legend. By default, the legend is 
#'  positioned to the right from the chart in a table cell specifically made for it. This should be a valid CSS selector.
#'  If the specified element does not exist, the legend will be added to the web page's body.}
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - a title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend. 
#'  \item \code{showPanel} - whether of not to show the instrument panel (grey triangle in the upper-left corner of the chart).
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.} 
#'   
#' @examples 
#' \dontrun{data("esoph")
#' 
#' lc_bars(dat(value = tapply(esoph$ncases, esoph$agegp, sum), 
#'             title = "Number of cases per age group",
#'             axisTitleX = "Age group", 
#'             axisTitleY = "Number of esophageal cases",
#'             axisTitlePosX = "down"))
#' 
#' lc_bars(dat(value = c(tapply(esoph$ncases, esoph$agegp, sum), 
#'                       tapply(esoph$ncontrols, esoph$agegp, sum)),
#'             stackIds = c(rep("case", 6), rep("control", 6))))
#' 
#' #It is easy to put data in a convenient form for barplots using tidyverse
#' library(magrittr)
#' library(dplyr)
#' library(tidyr)
#' library(stringr)
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
lc_bars <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, with = NULL, addLayer = FALSE) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("bars", data, ..., place = place, chartId = chartId, layerId = layerId, with = with, addLayer = addLayer)
}

#' Histograms and density plots
#' 
#' These functions make either a histogram or a density plot of the given data 
#' and either add them as a new layer to an existing chart or create a new chart.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced unless \code{addLayer = TRUE}. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param layerId An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same
#' ID already exists, it will be replaced. If not defined, it will be set to \code{LayerN}, where \code{N - 1} 
#' is the current number of layers in this chart.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. It must be
#' a \code{data.frame} or a \code{list}.
#' @param addLayer If there is already a chart with the same ID, this argument defines whether to replace it or to add a
#' new layer to it. This argument is ignored if both \code{place} and \code{chartId} are \code{NULL} or if there is no
#' chart with the given ID. 
#' 
#' @section Available properties:
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{value} - vector of data values. 
#'  \item \code{nbins} - (only for \code{lc_hist}) number of bins.} 
#' 
#' These functions are extensions of \code{\link{lc_line}} (\code{lc_dens}) or \code{\link{lc_bars}} 
#' (\code{lc_hist}) and therefore also accept all their properties.
#' 
#' @describeIn lc_hist makes a histogram. It is an extension of \code{\link{lc_bars}}.
#' 
#' @examples
#' \dontrun{
#' lc_hist(dat(value = rnorm(1000), nbins = 30, height = 300))
#' lc_dens(dat(value = rnorm(1000), height = 300)) }
#' 
#' @export 
lc_hist <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, with = NULL, addLayer = FALSE) {
  # has a nbins property. Not implemented in JS
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("hist", data, ..., place = place, chartId = chartId, layerId = layerId, with = with, addLayer = addLayer)
}

#' @describeIn lc_hist makes a density plot. Is an extension of \code{\link{lc_line}}.
#' @export
#' @importFrom stats density.default
lc_dens <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, with = NULL, addLayer = FALSE) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("dens", data, ..., place = place, chartId = chartId, layerId = layerId, with = with, addLayer = addLayer)
}

#' Create a heatmap
#' 
#' \code{lc_heatmap} creates a new heatmap. Unlike charts with axes, heatmaps do not have
#' any layers.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' @param pacerStep Time in ms between two consecutive calls of an \code{onmouseover} event. Prevents over-queueing in case
#' of cumbersome computations. May be important when the chart works in canvas mode.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{value} - matrix of values that will be displayed as a heatmap.
#'  \item \code{rowLabel, colLabel} - vector of labels for all rows or columns.
#'  \item \code{showDendogramRow, showDendogramCol} - whether to show dendrograms when rows or columns are
#'  clustered. Even if these properties are set to \code{FALSE}, rows and columns can still be clustered. 
#'  \item \code{clusterRows, clusterCols} - whether rows or columns should be clustered. If these
#'  properties are set to \code{FALSE}, rows and columns can still be clustered later using the instrument
#'  panel.
#'  \item \code{mode} - one of \code{"default", "svg", "canvas"}. Defines, whether to display heatmap as
#'  an SVG or Canvas object. \code{"default"} mode switches between the two, turning heatmap into Canvas 
#'  image, when there are too many cell, and into SVG object otherwise.
#'  \item \code{rankRows, rankCols} - rank of rows and columns of the heatmap. This should be a vector with a
#'  numeric value for each row or column.
#'  \item \code{showValue} - if \code{TRUE}, values will be shown as text in each cell.
#'  \item \code{valueTextColour} - of the value text in each cell. By default, the colour is defined individually
#'  based on the cell colour.
#'  \item \code{informText} - text that appears when the mouse cursor moves over an element. Unlike \code{label},
#'  completely overwrites the tooltip content with a custom HTML code. Must be a matrix of characters (HTML code
#'  for each cell).}
#' 
#' Style settings
#' \itemize{
#'  \item \code{rowTitle, colTilte} - titles for rows and columns (similar to axes titles).
#'  \item \code{palette} - a vector of colours to construct a colour scale.
#'  \item \code{colourDomain} - domain of the colour scale. All values outside it will
#'  be clamped to its edges.} 
#'  
#' Interactivity settings
#' \itemize{
#'  \item \code{on_click} - a function, to be called when one of the cells is clicked. Gets a vector of row and column indices 
#'  of the clicked cell as its arguments.
#'  \item \code{on_mouseover} - a function, to be called when the mouse hovers over one of the cells.
#'  Gets a vector of row and column indices of the clicked cell as its arguments.
#'  \item \code{on_mouseout} - a function, to be called when the mouse moves away from one of the cells.
#'  \item \code{on_marked} - a function, to be called when any of the cells are selected (marked) 
#'  or deselected. Use \code{\link{getMarked}} function to get the IDs of the currently marked cells. To mark cells,
#'  select them with your mouse while holding the \emph{Shift} key.
#'  \item \code{on_labelClickRow, on_labelClickCol} - functions, to be called when a row or a column label is clicked.
#'  By default, a click on a, for instance, row label sorts all columns of the heatmap based on their value in the selected row.}
#'  
#' Legend settings
#' \itemize{
#'  \item \code{legend_width} - width of the legend in pixels. The default value is 200.
#'  \item \code{legend_height} - height of the legend in pixels. By default, it is equal to the height of the chart.
#'  \item \code{legend_sampleHeight} - height of a single key of the legend in pixels. The default value is 20.
#'  \item \code{legend_ncol} - number of columns to order several legends. By default, this is defined from the number 
#'  of legends to reach close to a square shape.
#'  \item \code{legend_container} - a DOM element of the web page where to place the legend. By default, the legend is 
#'  positioned to the right from the chart in a table cell specifically made for it. This should be a valid CSS selector.
#'  If the specified element does not exist, the legend will be added to the web page's body.}
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{plotWidth} - width of the plotting area in pixels.
#'  \item \code{plotHeight} - height of the plotting area in pixels.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - a title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.
#'  \item \code{showLegend} - whether or not to show the legend.
#'  \item \code{showPanel} - whether of not to show the instrument panel (grey triangle in the upper-left corner of the chart).
#'  \item \code{transitionDuration} - duration of the transitions between any two states of the chart. If 0,
#'  no animated transition is shown. It can be useful to turn the transition off, when lots of frequent 
#'  changes happen to the chart.} 
#' @examples 
#' \dontrun{
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
lc_heatmap <- function(data = list(), place = NULL, ..., chartId = NULL, with = NULL, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("heatmap", data, ..., place = place, chartId = chartId, layerId = "main", with = with, pacerStep = pacerStep)
}

#' Add a colour slider
#' 
#' Colour slider provides an easy way to change any continuous colour scale
#' interactively. If your chart uses a continuous colour scale, you can just 
#' link a colour slider and it will be automatically synchronized with your
#' chart's colour scale.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' 
#' @section Available properties: 
#' You can read more about different properties
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{chart} - ID of the chart to which the colour slider should be linked.
#'  \item \code{layer} - id of the layer to which the colour slider should be linked.
#'  If the chart has only one layer, this property is optional.}
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels.
#'  \item \code{heigth} - height of the chart in pixels.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.
#'  \item \code{title} - a title of the chart.
#'  \item \code{titleX, titleY} - coordinates of the chart title.
#'  \item \code{titleSize} - font-size of the chart title.} 
#' 
#' @examples 
#' \dontrun{data("iris")
#' lc_scatter(dat(x = Sepal.Length, 
#'                y = Petal.Length,
#'                colourValue = Petal.Width,
#'                symbolValue = Species),
#'            with = iris,
#'            title = "Iris dataset",
#'            axisTitleY = "Petal Length",
#'            axisTitleX = "Sepal Length",
#'            colourLegendTitle = "Petal Width",
#'            symbolLegendTitle = "Species",
#'            showLegend = FALSE,
#'            chartId = "scatter")
#' 
#' lc_colourSlider(chart = "scatter")}
#' 
#' @export
lc_colourSlider <- function(data = list(), place = NULL, ..., chartId = NULL, with = NULL) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }

  pkg.env$app$setChart(chartType = "colourSlider", data = data, ..., place = place, chartId = chartId, layerId = "main", with = with)
}

#' @describeIn lc_line creates straight lines by intercept and slope values
#' @export
#' @importFrom jsonlite toJSON
lc_abLine <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, 
                      with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("abLine", data, ..., place = place, chartId = chartId, 
                       layerId = layerId, with = with, addLayer = addLayer, pacerStep = pacerStep)
}

#' @describeIn lc_line creates horizontal lines by y-intercept values
#' @export
lc_hLine <- function(data = list(), place = NULL, ..., chartId = NULL, 
                     layerId = NULL, with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("hLine", data, ..., place = place, chartId = chartId, layerId = layerId, 
                       with = with, addLayer = addLayer, pacerStep = pacerStep)
}

#' @describeIn lc_line creates vertical lines by x-intercept values
#' @export
lc_vLine <- function(data = list(), place = NULL, ..., chartId = NULL, layerId = NULL, 
                     with = NULL, addLayer = FALSE, pacerStep = 50) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("vLine", data, ..., place = place, chartId = chartId, 
                       layerId = layerId, with = with, addLayer = addLayer, pacerStep = pacerStep)
}

#' Add HTML code to the page
#' 
#' \code{lc_html} adds a block of HTML code. It uses \code{\link[hwriter]{hwrite}} function
#' to transform some data structures (e.g. data frames) to HTML tables.
#'
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#'
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{content} - HTML code to display on the page. Can also be a vector, \code{data.frame} or
#'  any other structure, that can be transformed to HTML by \code{\link[hwriter]{hwrite}}. } 
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{width} - width of the chart in pixels. By default, width will be set to fit the content.
#'  If width is defined and it's smaller than content's width, scrolling will be possible.
#'  \item \code{heigth} - height of the chart in pixels. By default, height will be set to fit the content.
#'  If height is defined and it's smaller than content's height, scrolling will be possible.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'  
#' @examples
#' \dontrun{lc_html(content = "Some <b>HTML</b> <br> <i>code</i>.")
#' lc_html(dat(content = matrix(1:12, nrow = 4)))
#' data(iris)
#' lc_html(content = iris, height = 200)}
#' 
#' @export
#' @importFrom hwriter hwrite
lc_html <- function(data = list(), place = NULL, ..., chartId = NULL, with = NULL) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("html", data, ..., place = place, chartId = chartId, layerId = "main", with = with)
}

#' Add input forms to the page
#'
#' \code{lc_input} adds an input form. This function is an rlc wrapper for an
#' HTML \code{<input>} tag. Five types of input are supported: \code{"text"}, \code{"range"},
#' \code{"checkbox"}, \code{"radio"} and \code{"button"}.
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' \itemize{
#'  \item \code{type} - type of input. Must be one of \code{"text"}, \code{"range"}, \code{"checkbox"}, 
#'  \code{"radio"} or \code{"button"}.
#'  \item \code{value} - current state of the input block. For radio buttons it is an index of the checked
#'  button. For checkboxes - a vector of \code{TRUE} (for each checked box) and \code{FALSE} (for each unchecked ones),
#'  for ranges and text boxes - a vector of values for each text field or slider.
#'  \item \code{step} (only for \code{type = "range"}) - stepping interval for values that can be selected with the slider.
#'  Must be a numeric vector with one value for each slider in the input block.
#'  \item \code{min, max} (only for \code{type = "range"}) - minimal and maximal values that can be selected with the slider.
#'  Must be a numeric vector with one value for each slider in the input block.
#'  \item \code{fontSize} - changes font size of the labels. The default size is 17.
#'  \item \code{nrows} - number of rows in the table of input elements. By default is defined by the number of elements.
#'  \item \code{ncols} - number of columns of input elements. The default value is 1.
#'  }
#'  
#' Interactivity settings
#' \itemize{
#'   \item \code{on_click, on_change} - a function, to be called when user clicks on a button, enters text in a text field
#'   or moves a slider. The two properties are complete synonyms and can replace one another.
#' }
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{title} - title of the input block.
#'  \item \code{width} - width of the chart in pixels. By default, width will be set to fit the content.
#'  If width is defined and it's smaller than content's width, scrolling will be possible.
#'  \item \code{heigth} - height of the chart in pixels. By default, height will be set to fit the content.
#'  If height is defined and it's smaller than content's height, scrolling will be possible.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'
#'@examples
#' \dontrun{lc_input(type = "checkbox", labels = paste0("el", 1:5), on_click = function(value) print(value),
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
lc_input <- function(data = list(), place = NULL, ..., chartId = NULL, with = NULL) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("input", data, ..., place = place, chartId = chartId, layerId = "main", with = with)
}

#' Add static plot or custom image to the page
#'
#' \code{lc_image} adds a graphical object to the page. It can be any graphical R object (for example,
#' objects of class \code{ggplot}) or image that is stored locally. Note: currently works only on Linux and iOS. 
#' 
#' @param data Name-value pairs of properties passed through the \code{\link{dat}} function. These
#' properties will be re-evaluated on each \code{\link{updateCharts}} call. 
#' @param place An ID of the container, where to place new chart. It will be ignored if the chart already
#' exists. If not defined, the chart will be appended to the web page's body.
#' @param ... Name-value pairs of properties that will be evaluated only once and then will remain 
#' constant. These properties can still be changed later using the \code{\link{setProperties}} function.
#' @param chartId An ID for the chart. All charts must have unique IDs. If a chart with the same ID already
#' exists, it will be replaced. If ID is not defined, it will be the same as the
#' value of the \code{place} argument. And if both are not defined, the ID will be set to \code{ChartN}, 
#' where \code{N - 1} is the number of existing charts.
#' @param with A dataset or a list from which other properties should be taken. If the dataset doesn't have a 
#' column with the requested name, the variable will be searched for outside of the dataset. Must be
#' a \code{data.frame} or a \code{list}.
#' 
#' @section Available properties: 
#' You can read more about different properties 
#' \href{https://anders-biostat.github.io/linked-charts/rlc/tutorials/props.html}{here}.
#' 
#' One of \code{img} and \code{src} properties is required.
#' \itemize{
#'  \item \code{img} - static plot to display. Anything that can be saved as png can be used here. .png image fill be saved to
#'  a temporary directory (see \code{\link[base]{tempdir}}).
#'  \item \code{src} - path to an already saved image. Can be an absolute path or a path relative to the current working directory. 
#'  If \code{img} is defined, this property will be ignored.
#'  }
#'  
#' Global chart settings
#' \itemize{
#'  \item \code{title} - title of the input block.
#'  \item \code{width} - width of the chart in pixels. By default, width will be set to fit the content.
#'  If width is defined and it's smaller than content's width, scrolling will be possible.
#'  \item \code{heigth} - height of the chart in pixels. By default, height will be set to fit the content.
#'  If height is defined and it's smaller than content's height, scrolling will be possible.
#'  \item \code{paddings} - padding sizes in pixels. Must be a list with all the following fields: 
#'  \code{"top", "bottom", "left", "right"}.}
#'
#'@examples
#' \dontrun{
#' library(ggplot2)
#' pl <- ggplot() + geom_point(aes(1:10, 1:10))
#' 
#' lc_image(dat(img = pl, 
#'    title = "Some plot", 
#'    paddings = list(top = 100, bottom = 100, left = 10, right = 10)))
#' }
#'
#' @export
lc_image <- function(data = list(), place = NULL, ..., chartId = NULL, with = NULL) {
  if(is.null(pkg.env$app)){
    openPage()
    pkg.env$app$setEnvironment(parent.frame())
  }
  
  pkg.env$app$setChart("image", data, ..., place = place, chartId = chartId, layerId = "main", with = with)
}

