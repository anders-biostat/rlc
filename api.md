---
usePrism: true
title: 'R/LinkedCharts API'
hideTOC: 'true'
api: 'rlc'
---
# `chartEvent`

Trigger an event


## Description

This function is called whenever any interactive element of a chart is activated by clicking, marking,
 hovering, etc. In turn,
 it calls a corresponding callback function, if any has been specified. This function
 is meant to be used internally. However, an experienced user can still use it to simulate mouse events,
 even those triggered by non-existing elements. This function is a wrapper around method
 `chartEvent` of class [`LCApp`](#lcapp) .


## Usage

```r
chartEvent(d, chartId, layerId = "main", event, sessionId = .id, app = .app)
```


## Arguments

Argument      |Description
------------- |----------------
`d`     |     Value that is used to identify interactive element or its state. A single numeric index for a point or a line, vector or row and column indices of a cell for a heatmap, value for an input block (please, check [`lc_input`](#lcinput)  for more details about input blocks and their values). Should be `NULL` for `mouseout` or `marked` events. N.B. This function is called from the web page and therefore all element indices start from zero as it is happens in JavaScript.
`chartId`     |     ID of the chart.
`layerId`     |     ID of the layer. You can print IDs of all charts and their layers with [`listCharts`](#listcharts) .
`event`     |     Type of event. Must be one of `"click"` , `"mouseover"` , `"mouseout"` , `"marked"` , `"labelClickRow"` , `"labelClickCol"` .
`sessionId`     |     ID of the session (opened client page) that triggered the event. The default value uses a local session variable. This must be a single session ID. You can get a list of IDs of all currently active with the method `getSessionIds` inherited from class [`App`](#app) by [`LCApp`](#lcapp) . Possible errors in evaluation of this argument are ignored.
`app`     |     Object of class [`LCApp`](#lcapp) for which the event was triggered. Note that this argument is here for internal use and its default value is a variable, stored in each session locally. If you are not using wrapper functions, it is preferred to call method `chartEvent` of an object of class [`LCApp`](#lcapp) .


## Examples

```r
list("x <- rnorm(50)\n", "lc_scatter(x = x, y = 2*x + rnorm(50, 0.1), on_click = function(d) print(d))\n", "chartEvent(51, \"Chart1\", \"Layer1\", \"click\")")
```


# `closePage`

Stop server


## Description

Stops the server and closes all currently opened pages (if any). This function is a
 wrapper of `stopServer` method inherited by the [`LCApp`](#lcapp) class from the [`App`](#app) class.


## Usage

```r
closePage()
```


## Examples

```r
list("openPage(useViewer = FALSE)\n", "closePage()")
```


# `dat`

Link data to the chart


## Description

`dat` allows to link variables from the current environment to chart's properties.
 On every [`updateCharts`](#updatecharts) call all the data provided via the `dat` function
 will be automatically reevaluated and the chart will be changed accordingly. One can also
 put properties outside of the `dat` function to prevent their reevaluation.


## Usage

```r
dat(...)
```


## Arguments

Argument      |Description
------------- |----------------
`...`     |     List of name-value pairs to define the properties.


## Examples

```r
list("lc_scatter(dat(x = rnorm(30)), y = rnorm(30))\n", "#note that the Y values remain the same after each updateCharts call\n", "updateCharts()")
```


# `getMarked`

Get currently marked elements


## Description

`getMarked` returns indices of the chart's elements that are currently
 marked. To mark elements select them with your mouse while holding the Shift key.
 Double click on the chart with the Shift key pressed will deselect all the
 elements. This function is a wrapper of method `getMarked` of class [`LCApp`](#lcapp) .


## Usage

```r
getMarked(chartId = NULL, layerId = NULL, sessionId = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`chartId`     |     An ID of the chart. This argument is optional, if there is only one chart.
`layerId`     |     An ID of the layer. This argument is optional, if there is only one chart. than one layer.
`sessionId`     |     An ID of the session from which to get the marked elements. Can be `NULL`  if there is only one active session. Otherwise must be a valid session ID. Check [`Session`](#session)  for more information on client sessions. If a call to this function was triggered from an opened web page, ID of the corresponding session will be used automatically.


## Value

a vector of indices or, in case of heatmaps, an n x 2 matrix were first and
 second columns contain row and column indices of the marked cells, respectively.


## Examples

```r
list("data(iris)\n", "\n", "lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length))\n", "\n", "#now mark some points by selecting them with your mouse with Shift pressed\n", "\n", "getMarked(\"Chart1\")")
```


# `getPage`

Get the currently running app


## Description

`rlc` offers two ways to control an interactive app. One is by using methods of class
 [`LCApp`](#lcapp) . This allows one to have any number of apps within one
 R session, but requires some understanding of object oriented programming. Another way is to use
 provided wrapper functions that are exported by the package. These functions internally work with
 the [`LCApp`](#lcapp) object, which is stored in the package namespace upon initialization with
 [`openPage`](#openpage) function. `getPage` returns this object if any.


## Usage

```r
getPage()
```


## Details

Note that `rlc` package is based on `jrc` library. Both packages are organized in similar manner.
 Both have a central class that represents the entire app and can be fully managed with their methods ( [`LCApp`](#lcapp) 
 and [`App`](#app) , respectively). And both also provide a set of wrapper functions, that can be used instead of
 the methods. However, wrapper functions of the `jrc` package can't be use for `rlc` apps, while all the
 methods of class [`App`](#app) are inherited by [`LCApp`](#lcapp) . Therefore, if you want to get more low level
 control over your app, such as managing client sessions, local variables and memory usage, you should methods of
 [`App`](#app) class.


## Value

Object of class [`LCApp`](#lcapp) or `NULL` if there is no active app.


# `lc_bars`

Create a barplot


## Description

`lc_bars` creates a new barplot and adds it to the app and to the all currently opened pages
 as a new chart or as a new layer of an existing chart.


## Usage

```r
lc_bars(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced unless `addLayer = TRUE` . If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`layerId`     |     An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same ID already exists, it will be replaced. If not defined, will be set to `LayerN` , where `N - 1`  is the number of currently existing layers in this chart.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.
`addLayer`     |     if there is already a chart with the same ID, this argument defines whether to replace it or to add a new layer to it. This argument is ignored if both `place` and `chartId` are `NULL` or if there is no chart with the given ID.


## Examples

```r
list("data(\"esoph\")\n", "\n", "lc_bars(dat(value = tapply(esoph$ncases, esoph$agegp, sum), \n", "            title = \"Number of cases per age group\",\n", "            axisTitleX = \"Age group\", \n", "            axisTitleY = \"Number of esophageal cases\",\n", "            axisTitlePosX = \"down\"))\n", "\n", "lc_bars(dat(value = c(tapply(esoph$ncases, esoph$agegp, sum), \n", "                      tapply(esoph$ncontrols, esoph$agegp, sum)),\n", "            stackIds = c(rep(\"case\", 6), rep(\"control\", 6))))\n", 
    "\n", "#It is easy to put data in a convenient form for barplots using tidyverse\n", "library(magrittr)\n", "library(dplyr)\n", "library(tidyr)\n", "library(stringr)\n", "\n", "esoph %>%\n", "  gather(type, cases, (ncases:ncontrols)) %>%\n", "  mutate(type = str_sub(type, 2, -2)) %>%\n", "  group_by(agegp, alcgp, type) %>%\n", "  summarise(ncases = sum(cases)) -> newData\n", "\n", "lc_bars(dat(value = newData$ncases,\n", "            stackIds = newData$type,\n", "            barIds = newData$alcgp,\n", 
    "            groupIds = newData$agegp))")
```


# `lc_colourSlider`

Add a colour slider


## Description

Colour slider provides an easy way to change any continuous colour scale
 interactively. If your chart uses a continuous colour scale, you can just
 link a colour slider and it will be automatically synchronized with your
 chart's colour scale.


## Usage

```r
lc_colourSlider(data = list(), place = NULL, ..., chartId = NULL, with = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced. If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.


## Examples

```r
list("data(\"iris\")\n", "lc_scatter(dat(x = Sepal.Length, \n", "               y = Petal.Length,\n", "               colourValue = Petal.Width,\n", "               symbolValue = Species),\n", "           with = iris,\n", "           title = \"Iris dataset\",\n", "           axisTitleY = \"Petal Length\",\n", "           axisTitleX = \"Sepal Length\",\n", "           colourLegendTitle = \"Petal Width\",\n", "           symbolLegendTitle = \"Species\",\n", "           showLegend = FALSE,\n", "           chartId = \"scatter\")\n", 
    "\n", "lc_colourSlider(chart = \"scatter\")")
```


# `lc_heatmap`

Create a heatmap


## Description

`lc_heatmap` creates a new heatmap. Unlike charts with axes, heatmaps do not have
 any layers.


## Usage

```r
lc_heatmap(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  with = NULL,
  pacerStep = 50
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced. If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.
`pacerStep`     |     Time in ms between two consecutive calls of an `onmouseover` event. Prevents overqueuing in case of cumbersome computations. May be important when the chart works in canvas mode.


## Examples

```r
list("\n", "library(RColorBrewer)\n", "#create a test matrix\n", "test <- cbind(sapply(1:10, function(i) c(rnorm(10, mean = 1, sd = 3), \n", "                                         rnorm(6, mean = 5, sd = 2), \n", "                                         runif(14, 0, 8))),\n", "              sapply(1:10, function(i) c(rnorm(10, mean = 3, sd = 2), \n", "                                         rnorm(6, mean = 1, sd = 2), \n", "                                         runif(14, 0, 8))))\n", "test[test < 0] <- 0\n", 
    "rownames(test) <- paste0(\"Gene\", 1:30)\n", "colnames(test) <- paste0(\"Sample\", 1:20)\n", "\n", "lc_heatmap(dat(value = test))\n", "\n", "# when you want to cluster rows or columns, it can be\n", "# a good idea to make bottom and right paddings larger to\n", "# fit labels\n", "lc_heatmap(dat(value = test),\n", "           clusterRows = TRUE,\n", "           clusterCols = TRUE,\n", "           paddings = list(top = 50, left = 30, bottom = 75, right = 75))\n", "\n", "lc_heatmap(dat(value = cor(test), \n", 
    "               colourDomain = c(-1, 1),\n", "               palette = brewer.pal(11, \"RdYlBu\")))")
```


# `lc_hist`

Histograms and density plots


## Description

These functions make either a histogram or a density plot of the given data
 and either add them as a new layer to an existing chart or create a new chart.


## Usage

```r
lc_hist(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE
)
lc_dens(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced unless `addLayer = TRUE` . If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`layerId`     |     An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same ID already exists, it will be replaced. If not defined, will be set to `LayerN` , where `N - 1`  is the number of currently existing layers in this chart.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.
`addLayer`     |     if there is already a chart with the same ID, this argument defines whether to replace it or to add a new layer to it. This argument is ignored if both `place` and `chartId` are `NULL` or if there is no chart with the given ID.


## Examples

```r
list("\n", "lc_hist(dat(value = rnorm(1000), nbins = 30, height = 300))\n", "lc_dens(dat(value = rnorm(1000), height = 300)) ")
```


# `lc_html`

Add HTML code to the page


## Description

`lc_html` adds a block of HTML code. It uses [`hwrite`](#hwrite) function
 to transform some data structures (e.g. data frames) to HTML tables.


## Usage

```r
lc_html(data = list(), place = NULL, ..., chartId = NULL, with = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced. If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.


## Examples

```r
list("lc_html(content = \"Some <b>HTML</b> <br> <i>code</i>.\")\n", "lc_html(dat(content = matrix(1:12, nrow = 4)))\n", "data(iris)\n", "lc_html(content = iris, height = 200)")
```


# `lc_image`

Add static plot or custom image to the page


## Description

`lc_image` adds a graphical object to the page. It can be any graphical R object (for example,
 objects of class 'ggplot') or image that is stored locally. Note: currently works only on Linux and iOS.


## Usage

```r
lc_image(data = list(), place = NULL, ..., chartId = NULL, with = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced. If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.


## Examples

```r
list("\n", "library(ggplot2)\n", "pl <- ggplot() + geom_point(aes(1:10, 1:10))\n", "\n", "lc_image(dat(img = pl, \n", "   title = \"Some plot\", \n", "   paddings = list(top = 100, bottom = 100, left = 10, right = 10)))\n")
```


# `lc_input`

Add input forms to the page


## Description

`lc_input` adds an input form. This function is an rlc wrapper for an
 HTML `<input>` tag. Five types of input are supported: `"text"` , `"range"` ,
 `"checkbox"` , `"radio"` and `"button"` .


## Usage

```r
lc_input(data = list(), place = NULL, ..., chartId = NULL, with = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced. If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.


## Examples

```r
list("lc_input(type = \"checkbox\", labels = paste0(\"el\", 1:5), on_click = function(value) print(value),\n", "value = TRUE)\n", "lc_input(type = \"radio\", labels = paste0(\"el\", 1:5), on_click = function(value) print(value),\n", "         value = 1)\n", "lc_input(type = \"text\", labels = paste0(\"el\", 1:5), on_click = function(value) print(value),\n", "         value = c(\"a\", \"b\", \"c\", \"e\", \"d\"))\n", "lc_input(type = \"range\", labels = paste0(\"el\", 1:5), on_click = function(value) print(value),\n", 
    "         value = 10, max = c(10, 20, 30, 40, 50), step = c(0.5, 0.1, 1, 5, 25))\n", "lc_input(type = \"button\", labels = paste0(\"el\", 1:5), on_click = function(value) print(value))")
```


# `lc_line`

Lines and ribbons


## Description

These functions create various kinds of lines. They connect observations or
 create filled areas with customized border. Each layer may have one or several lines.


## Usage

```r
lc_line(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
lc_path(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
lc_ribbon(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE
)
lc_abLine(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
lc_hLine(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
lc_vLine(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced unless `addLayer = TRUE` . If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`layerId`     |     An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same ID already exists, it will be replaced. If not defined, will be set to `LayerN` , where `N - 1`  is the number of currently existing layers in this chart.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.
`addLayer`     |     if there is already a chart with the same ID, this argument defines whether to replace it or to add a new layer to it. This argument is ignored if both `place` and `chartId` are `NULL` or if there is no chart with the given ID.
`pacerStep`     |     Time in ms between two consecutive calls of an `on_mouseover` event. Prevents overqueuing in case of cumbersome computations. May be important when the chart works in canvas mode.


## Examples

```r
list("x <- seq(0, 8, 0.2)\n", "lc_line(dat(x = x, y = cbind(cos(x), sin(x)),\n", "            aspectRatio = 1,\n", "            colour = c(\"blue\", \"red\"),\n", "            dasharray = c(\"5\", \"1 5 5\")))\n", "            \n", "points <- seq(0, 6.5, 0.1)\n", "x <- cos(points)\n", "y <- sin(points)\n", "lc_path(dat(x = sapply(0:2, function(i) x + i), \n", "            y = sapply(0:2, function(i) y + i),\n", "            fill = c(\"blue\", \"red\", \"black\"),\n", "            opacity = c(0.3, 0.5, 0.7)))\n", 
    "            \n", "x <- seq(0, 5, 0.1)\n", "y <- x*3 + rnorm(length(x), sd = 2)\n", "fit <- lm(y ~ x)\n", "pred <- predict(fit, data.frame(x = x), se.fit = TRUE)\n", "lc_ribbon(dat(ymin = pred$fit - 1.96 * pred$se.fit,\n", "              ymax = pred$fit + 1.96 * pred$se.fit,\n", "              x = x,\n", "              colour = \"#555555\"), chartId = \"ribbonTest\")\n", "lc_scatter(dat(x = x, y = y), size = 2, chartId = \"ribbonTest\", addLayer = TRUE)\n", "lc_abLine(dat(a = fit$coefficients[2], b = fit$coefficients[1]), \n", 
    "          chartId = \"ribbonTest\", addLayer = TRUE)\n", "\n", "lc_hLine(dat(h = seq(1, 9, 1), domainX = c(0, 10), domainY = c(0, 10)), chartId = \"grid\")\n", "lc_vLine(dat(v = seq(1, 9, 1)), chartId = \"grid\", addLayer = TRUE)")
```


# `lc_scatter`

Visualize a set of points


## Description

These functions plot a set of points with known coordinates that can be either categorical,
 or continuous.


## Usage

```r
lc_scatter(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
lc_beeswarm(
  data = list(),
  place = NULL,
  ...,
  chartId = NULL,
  layerId = NULL,
  with = NULL,
  addLayer = FALSE,
  pacerStep = 50
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     Name value pairs of properties, passed through the [`dat`](#dat) function. These properties will be reevaluated on each [`updateCharts`](#updatecharts) call.
`place`     |     ID of a container, where to place new chart. Will be ignored if the chart already exists. If not defined, the chart will be appended to the body of the web pages.
`...`     |     Name-value pairs of properties that will be evaluated only once and then will remain constant. These properties can still be changed later using the [`setProperties`](#setproperties) function.
`chartId`     |     ID for the chart. All charts must have unique IDs. If a chart with the same ID already exists, it will be replaced unless `addLayer = TRUE` . If ID is not defined, it will be the same as value of the `place` argument. And if both are not defined, the ID will be set to `ChartN` , where `N - 1` is the number of existing charts.
`layerId`     |     An ID for the new layer. All layers within one chart must have different IDs. If a layer with the same ID already exists, it will be replaced. If not defined, will be set to `LayerN` , where `N - 1`  is the number of currently existing layers in this chart.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.
`addLayer`     |     if there is already a chart with the same ID, this argument defines whether to replace it or to add a new layer to it. This argument is ignored if both `place` and `chartId` are `NULL` or if there is no chart with the given ID.
`pacerStep`     |     Time in ms between two consecutive calls of an `onmouseover` event. Prevents overqueuing in case of cumbersome computations. May be important when the chart works in canvas mode.


## Examples

```r
list("data(\"iris\")\n", "lc_scatter(dat(x = Sepal.Length, \n", "               y = Petal.Length,\n", "               colourValue = Petal.Width,\n", "               symbolValue = Species),\n", "           with = iris,\n", "           title = \"Iris dataset\",\n", "           axisTitleY = \"Petal Length\",\n", "           axisTitleX = \"Sepal Length\",\n", "           colourLegendTitle = \"Petal Width\",\n", "           symbolLegendTitle = \"Species\")\n", "\n", "lc_beeswarm(dat(x = iris$Species,\n", 
    "                y = iris$Sepal.Length,\n", "                colourValue = iris$Sepal.Width),\n", "            title = \"Iris dataset\",\n", "            axisTitleY = \"Sepal Length\",\n", "            axisTitleX = \"Species\",\n", "            colourLegendTitle = \"Sepal Width\")")
```


# `LCApp`

LCApp class


## Description

Object of this class represents the entire linked-charts app. It stores all charts, client sessions and
 local variables. You can create and manage interactive apps solely by creating new instances of this class and utilizing
 their methods. There are no limitations on the number of apps simultaneously running in one R session.
 However, it is also possible to create and manage app via the wrapper functions provided in this package. In this case an
 instance of [`LCApp`](#lcapp) class is initialized and stored in the package's namespace. Therefore, only one app can be active simultaneously.
 You can always retrieve the active app with the [`getPage`](#getpage) function. The `LCApp` class inherits from
 the [`App`](#app) class of the `jrc` package.


# `listCharts`

List all existing charts and layers


## Description

`listCharts` prints a list of IDs of all existing charts and layers.
 This function is wrapper around method `listCharts` of class [`LCApp`](#lcapp) .


## Usage

```r
listCharts()
```


## Examples

```r
list("noise <- rnorm(30)\n", "x <- seq(-4, 4, length.out = 30)\n", "\n", "lc_scatter(dat(x = x,\n", "               y = sin(x) + noise,\n", "               colourValue = noise), \n", "           chartId = \"plot\", layerId = \"points\")\n", "lc_line(dat(x = x, y = sin(x)), chartId = \"plot\", addLayer = TRUE)\n", "lc_colourSlider(chart = \"plot\", layer = \"points\")\n", "\n", "listCharts()")
```


# `mark`

Mark elements of a chart


## Description

`mark` selects a set of elements in a given chart. It is equivalent to
 selecting elements interactively by drawing a rectangle with the mouse
 while holding the `Shift` key. This function is a wrapper of method `mark` of
 class [`LCApp`](#lcapp) .


## Usage

```r
mark(
  elements,
  chartId = NULL,
  layerId = NULL,
  preventEvent = TRUE,
  clear = FALSE,
  sessionId = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`elements`     |     numeric vector of indices of the elements to select.
`chartId`     |     ID of the chart where to select elements (can be omitted if there is only one chart).
`layerId`     |     ID of the layer where to select elements (can be omitted if the chart has only one layer).
`preventEvent`     |     if `TRUE` , `on_marked` callback function will not be called. Can be used to prevent endless stacks of calls.
`clear`     |     if `TRUE` , all previously marked elements will be unmarked, otherwise new elements will be added to a set of currently marked ones.
`sessionId`     |     An ID of the session for which to mark elements. Can be `NULL`  if there is only one active session. Otherwise must be a valid session ID. Check [`Session`](#session)  for more information on client sessions. If a call to this function was triggered from an opened web page, ID of the corresponding session will be used automatically.


## Examples

```r
list("data(\"iris\")\n", "openPage(FALSE, layout = \"table1x2\")\n", "\n", "#brushing example\n", "#Hold Shift pressed and select a group of point on one of the charts\n", "\n", "lc_scatter(dat(\n", "  x = iris$Sepal.Length,\n", "  y = iris$Petal.Length,\n", "  colourValue = iris$Species,\n", "  on_marked = function() {\n", "    mark(getMarked(\"A1\"), \"A2\")\n", "  }\n", "), \"A1\")\n", "\n", "lc_scatter(dat(\n", "  x = iris$Sepal.Width,\n", "  y = iris$Petal.Width,\n", "  colourValue = iris$Species,\n", 
    "  on_marked = function() {\n", "    mark(getMarked(\"A2\"), \"A1\")\n", "  }\n", "), \"A2\")")
```


# `openPage`

Open a new empty page


## Description

`openPage` starts a server, establishes a web socket connection between it and the current
 R session and loads linked-charts JS library with all the dependencies. This function initializes
 an instance of class [`LCApp`](#lcapp) and stores it in the namespace of the package. If another
 instance has already been stored (i.e. another app has been started with this function), the existing
 app will be closed.


## Usage

```r
openPage(
  useViewer = TRUE,
  rootDirectory = NULL,
  startPage = NULL,
  layout = NULL,
  port = NULL,
  browser = NULL,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`useViewer`     |     If `TRUE` , a page will be opened in the RStudio Viewer. If `FALSE` , a default web browser will be used.
`rootDirectory`     |     A path to the root directory for the server. Any file, requested by the server will be searched for in this directory. If `rootDirectory` is not defined, the `http_root` in the package directory will be used as a root directory.
`startPage`     |     A path to an HTML file that should be used as a starting page of the app. It can be an absolute path to a local file, or it can be relative to the `rootDirectory`  or to the current R working directory. If `startPage` is not defined, an empty page will be used. The file must have .html extension.
`layout`     |     Adds one of the defaults layouts to each new page. Currently, only tables of arbitrary size are supported. To add a table, this parameter must be equal to `"tableNxM"` , where `N` is the number of rows and `M` is the number of columns. Each cell will get an ID that consists of a letter (indicating the row) and a number (indicating the column) (e.g. `B3` is an ID of the second row and third column).
`port`     |     Defines which TCP port the server will listen to. If not defined, random available port will be used (see [`randomPort`](#randomport) ).
`browser`     |     A browser in which to open a new web page. If not defined, default browser will be used. For more information check [`browseURL`](#browseurl) . If this argument is specified, `useViewer` will be ignored.
`...`     |     Further arguments passed to [`openPage`](#openpage) . Check details for more information.


## Details

Argument `onStart` of `jrc`  [`openPage`](#openpage) function is replaced in `rlc` 
 with `beforeLoad` and `afterLoad` . The reason for that is when the page opens, `rlc` 
 has to put there all the existing charts. Different situations may require some code be loaded before or after
 that happens. `beforeLoad` and `afterLoad` provide a way to define two callback functions, each
 receiving a [`Session`](#session) object as an argument and is called once for each new page.
 `beforeLoad` runs before anything else has happened, while `afterLoad` is called after all the
 existing charts have been added to the page.
 
 This function initializes a new instance of class [`LCApp`](#lcapp) and wraps around methods
 `startServer` and `openPage` of its parent class [`App`](#app) .


## Value

A new instance of class [`LCApp`](#lcapp) .


## Examples

```r
list("openPage()\n", "\n", "openPage(useViewer = FALSE, layout = \"table2x3\")")
```


# `removeChart`

Remove chart from the page


## Description

Removes an existing chart. Changes will be applied to all currently opened and future pages.
 This function is a wrapper around method `removeChart` of
 class [`LCApp`](#lcapp) .


## Usage

```r
removeChart(chartId)
```


## Arguments

Argument      |Description
------------- |----------------
`chartId`     |     A vector of IDs of the charts to be removed.


## Examples

```r
list("lc_scatter(dat(x = 1:10, y = 1:10 * 2), chartId = \"scatter\")\n", "removeChart(\"scatter\")")
```


# `removeLayer`

Remove a layer from a chart


## Description

Removes a layer from an existing chart. Changes will be applied to all currently opened and future pages.
 This function is a wrapper around method `removeLayer` of
 class [`LCApp`](#lcapp) .


## Usage

```r
removeLayer(chartId, layerId)
```


## Arguments

Argument      |Description
------------- |----------------
`chartId`     |     ID of the chart from which to remove a layer.
`layerId`     |     ID of the layer to remove.


## Examples

```r
list("lc_scatter(dat(x = 1:10, y = 1:10 * 2), chartId = \"scatter\")\n", "lc_abLine(a = 2, b = 0, chartId = \"scatter\", addLayer = TRUE)\n", "removeLayer(\"scatter\", \"Layer1\")")
```


# `setProperties`

Set properties of the chart


## Description

Sets or resets properties for an
 existing chart. Changes will be applied to all currently opened and future pages.
 This function is a wrapper around method `setProperties` of class [`LCApp`](#lcapp) .


## Usage

```r
setProperties(data, chartId, layerId = NULL, with = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     List of properties to be redefined for this layer or chart. Created by the [`dat`](#dat)  function.
`chartId`     |     ID of the chart, for which to redefine properties.
`layerId`     |     ID of the layer, for which to redefine properties. If the chart has a single layer or doesn't have layers, default value (which is NULL) can be used.
`with`     |     A data set from which other properties should be taken. If the data set doesn't have a column with the requested name, the variable will be searched for outside of the data set. Must be a data.frame or a list.


## Examples

```r
list("data(\"iris\")\n", "lc_scatter(dat(x = iris$Sepal.Length, y = iris$Sepal.Width), chartId = \"irisScatter\")\n", "setProperties(dat(symbolValue = iris$Species, y = iris$Petal.Length), chartId = \"irisScatter\")\n", "updateCharts(\"irisScatter\")\n", "\n", "lc_line(dat(x = iris$Sepal.Length, y = iris$Petal.Length), chartId = \"irisScatter\", \n", "        layerId = \"line\")\n", "setProperties(dat(colour = \"red\"), chartId = \"irisScatter\", layerId = \"line\")\n", "updateCharts(\"irisScatter\")")
```


# `updateCharts`

Update a chart


## Description

`updateCharts` redraws a chart or a single layer of a chart to make it up
 to date with the current state of the environment variables.


## Usage

```r
updateCharts(chartId = NULL, layerId = NULL, updateOnly = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`chartId`     |     ID of the chart to be updated (or vector of IDs). If `NULL` , all the existing charts will be updated.
`layerId`     |     ID of the layer to be updated (or vector of IDs). If `NULL` , all the layers of the selected charts will be updated. To update only some layers of multiple charts the lengths of `chartId` and `layerId` must be the same.
`updateOnly`     |     To improve performance it may be useful to change only certain aspects of a chart (e.g. positions of points, colour of heatmap cells, etc.). This argument can specify which part of chart to update. Possible options are `Elements` , `ElementPosition` , `ElementStyle` , `Axes` , `Labels` , `Cells` , `Texts` , `LabelPosition` , `CellPosition` , `TextPosition` , `LabelText` , `CellColour` , `TextValues` , `Canvas` , `Size` . See details for more information.


## Details

Linked charts of the rlc package are based on the idea that the variables that are
 used to define a chart are not constant, but can change as a result of user's
 actions. Each time the `updateCharts` function is called, all the properties that were set inside
 the [`dat`](#dat) function are reevaluated and the chart is redrawn in accordance with the
 new state.
 
 If this function is called from the R session, changes will be applied
 to all currently opened pages. If it is used as a part of any `rlc` callback, only the page
 that triggered the call will be affected.
 
 This function is a wrapper around method `updateCharts` of class [`LCApp`](#lcapp) .


## Examples

```r
list("data(iris)\n", "\n", "#store some properties in global variables\n", "width <- 300\n", "height <- 300\n", "colour <- iris$Sepal.Width\n", "#create a chart\n", "lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length, colourValue = colour,\n", "               width = width, height = height), chartId = \"iris\")\n", "\n", "#change the variables\n", "height <- 400\n", "colour <- iris$Petal.Width\n", "\n", "#this will change colour of points and chart height\n", "updateCharts(\"iris\")\n", 
    "#this will change only height\n", "updateCharts(\"iris\", updateOnly = \"Size\")\n", "\n", "#add another property\n", "setProperties(dat(symbolValue = iris$Species), \"iris\")\n", "#this will change only colour and symbols\n", "updateCharts(\"iris\", updateOnly = \"ElementStyle\")")
```


