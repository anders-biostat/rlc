test_that("Points on a scatter plot can be marked and indices of marked points can be retreived", {
  lc_scatter(x = rnorm(100), y = rnorm(100), transitionDuration = 0)
  
  mark(1:10)
  
  getPage()$allowVariables(c("n", "ms"))
  ses <- getPage()$getSession()
  
  ses$sendCommand("jrc.sendData('ms', charts.Chart1.get_marked().Layer1)", wait = 3)
  ses$sendCommand(paste0("jrc.sendData('n', d3.selectAll('.data_element').filter(function() {",
                         "return d3.select(this).attr('fill') == '#aaa';",
                         "}).size())"), wait = 3)
  
  expect_equivalent(ms, 0:9)
  expect_equal(n, 90)
  
  ms <- getMarked()
  expect_equivalent(ms, 1:10)
  
  closePage()
})

test_that("'clear' argument works correctly", {
  lc_scatter(x = rnorm(100), y = rnorm(100), transitionDuration = 0)
  
  mark(1:10)
  
  mark(11:20, clear = TRUE)
  
  getPage()$allowVariables(c("n", "ms"))
  ses <- getPage()$getSession()
  
  ses$sendCommand("jrc.sendData('ms', charts.Chart1.get_marked().Layer1)", wait = 3)
  ses$sendCommand(paste0("jrc.sendData('n', d3.selectAll('.data_element').filter(function() {",
                         "return d3.select(this).attr('fill') == '#aaa';",
                         "}).size())"), wait = 3)
  
  expect_equivalent(ms, 10:19)
  expect_equal(n, 90)
  
  closePage()
})

test_that("Marking works with multilayer charts", {
  lc_abLine(a = 1:4, b = rep(0, 4), chartId = "test")
  lc_scatter(x = rnorm(1:10), y = rnorm(1:10), chartId = "test", addLayer = TRUE)
  
  mark(1:2, layerId = "Layer1")
  
  expect_equivalent(getMarked(layerId = "Layer1"), 1:2)
  expect_null(getMarked(layerId = "Layer2"))
  expect_error(getMarked())
  
  closePage()
})

test_that("on_marked event is working", {
  lc_scatter(x = rnorm(100), y = rnorm(100), chartId = "ch1", on_marked = function() {
    mark(getMarked("ch1"), "ch2")
  })
  lc_scatter(x = rnorm(100), y = rnorm(100), chartId = "ch2", on_marked = function() {
    mark(getMarked("ch2"), "ch1")
  })
  
  mark(15, "ch1", preventEvent = FALSE)
  expect_equal(getMarked("ch1"), 15)
  expect_equal(getMarked("ch2"), 15)

  closePage()
})