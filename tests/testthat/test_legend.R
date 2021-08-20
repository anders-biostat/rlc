test_that("Legend size can be manipulated", {
  data("iris")
  
  lc_scatter(x = iris$Sepal.Length, y = iris$Petal.Length,
             colourValue = iris$Species, legend_width = 50,
             legend_sampleHeight = 10)
  
  app <- getPage()
  ses <- app$getSession()
  app$allowVariables(c("width", "height"))
  width <- -1
  height <- -1
  ses$sendCommand("jrc.sendData('width', charts.Chart1.legend.legendTable.selectAll('svg').attr('width'));", wait = 3)
  ses$sendCommand("jrc.sendData('height', charts.Chart1.legend.legendTable.selectAll('rect').attr('height'));", wait = 3)
  closePage()
  
  expect_equal(width, "50")
  expect_equal(height, "10")
})

test_that("Legends location can be changed", {
  data("iris")
  app <- openPage(layout = "table2x1")
  
  lc_scatter(x = iris$Sepal.Length, y = iris$Petal.Length,
             colourValue = iris$Species, legend_container = "#B1",
             place = "A1")
  
  ses <- app$getSession()
  app$allowVariables("nrect")
  nrect <- -1

  ses$sendCommand("jrc.sendData('nrect', d3.select('#B1').selectAll('rect').size());", wait = 3)
  closePage()
  
  expect_equal(nrect, 3)
})

test_that("Number of legend columns can be changed", {
  data("iris")
  
  lc_scatter(x = iris$Sepal.Length, y = iris$Petal.Length,
             symbolValue = iris$Species, colourValue = iris$Sepal.Width,
             legend_ncol = 2)
  
  app <- getPage()
  ses <- app$getSession()
  app$allowVariables("ncol")
  ncol <- -1
  ses$sendCommand("jrc.sendData('ncol', charts.Chart1.legend.legendTable.select('tr').selectAll('td').size());", wait = 3)
  closePage()
  
  expect_equal(ncol, 2)
})