test_that("Table layout can be used", {
  app <- openPage(layout = "table3x2", allowedVariables = c("cells", "rows"))
  ses <- app$getSession()
  
  ses$sendCommand("jrc.sendData('cells', d3.selectAll('td').size())", wait = 3)
  ses$sendCommand("jrc.sendData('rows', d3.selectAll('tr').size())", wait = 3)
  
  expect_equal(cells, 6)
  expect_equal(rows, 3)
  
  closePage()
})

test_that("beforeLoad and afterLoad can be used", {
  ncharts_before <- -1
  ncharts_after <- -1
  
  app <- LCApp$new(beforeLoad = function(session) {
    session$sendCommand("jrc.sendData('ncharts_before', d3.select('.linked-charts').size())", wait = 3)
  }, afterLoad = function(session) {
    session$sendCommand("jrc.sendData('ncharts_after', d3.select('.linked-charts').size())", wait = 3)
  }, allowedVariables = c("ncharts_before", "ncharts_after"))
  
  app$setChart("scatter", x = 1:10, y = 1:10)
  
  app$openPage(FALSE)
  
  expect_equal(ncharts_before, 0)
  expect_equal(ncharts_after, 1)
  
  app$stopServer()
})

#all other arguments shoul be tested in jrc