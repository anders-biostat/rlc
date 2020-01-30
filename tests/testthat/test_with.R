test_that("Arguments can be taken from a data frame", {
  data("iris")
  openPage(allowedVariables = "n")
  
  lc_scatter(dat(x = Sepal.Length, y = Petal.Length,
                 size = Sepal.Width * 2, colourValue = Petal.Width,
                 symbolValue = Species), with = iris)
  
  s <- getPage()$getSession()
  n <- -1
  s$sendCommand("jrc.sendData('n', d3.select('#Chart1').selectAll('.data_element').size())", wait = 3)
  
  expect_equal(n, nrow(iris))
  
  closePage()
  
  app <- LCApp$new(allowedVariables = "n")
  app$startPage()
  app$setChart("scatter", dat(x = Sepal.Length, y = Petal.Length,
                              size = Sepal.Width * 2, colourValue = Petal.Width,
                              symbolValue = Species), with = iris)
  s <- app$openPage(FALSE)

  n <- -1
  s$sendCommand("jrc.sendData('n', d3.select('#Chart1').selectAll('.data_element').size())", wait = 3)
  
  expect_equal(n, nrow(iris))
  app$stopServer()
})

test_that("Changes in the data set also influence the chart", {
  data("iris")
  
  openPage(allowedVariables = "n")
  lc_scatter(dat(x = Sepal.Length, y = Petal.Length), with = iris)
  
  iris <- iris[1:10, ]
  updateCharts()
  
  s <- getPage()$getSession()
  n <- -1
  s$sendCommand("jrc.sendData('n', d3.select('#Chart1').selectAll('.data_element').size())", wait = 3)
  
  expect_equal(n, 10)
  
  closePage()
})
