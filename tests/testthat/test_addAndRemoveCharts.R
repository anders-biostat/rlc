test_that("Charts can be added and removed", {
  openPage(allowedVariables = "n")
  n <- -1
  lc_scatter(x = 1:10, y = 1:10)
  lc_scatter(x = 1:10, y = 1:10)
  lc_scatter(x = 1:10, y = 1:10)
  app <- getPage()
  session <- app$getSession()
  session$sendCommand("jrc.sendData('n', d3.selectAll('div.linked-charts').size())", wait = 3)
  expect_equal(n, 3)
  
  removeChart("Chart2")
  session$sendCommand("jrc.sendData('n', d3.selectAll('div.linked-charts').size())", wait = 3)
  expect_equal(n, 2)
  
  session$sendCommand("jrc.sendData('n', d3.select('#Chart3').selectAll('.data_element').size())", wait = 3)
  expect_equal(n, 10)
  
  closePage()
})

test_that("Layers can be added and removed", {
  lc_scatter(x = 1:10, y = 1:10)
  lc_line(x = 1:10, y = 1:10, id = "Chart1", addLayer = TRUE)
  lc_hist(value = sample(1:10, 50, replace = TRUE), nbins = 15, id = "Chart1", addLayer = TRUE)

  app <- getPage()
  app$allowVariables("n")
  
  session <- app$getSession()
  session$sendCommand("jrc.sendData('n', d3.select('#Chart1').selectAll('.chart_g').size())", wait = 3)
  expect_equal(n, 3)
  
  session$sendCommand("jrc.sendData('n', d3.select('#Layer1').selectAll('.data_element').size())", wait = 3)
  expect_equal(n, 10)

  session$sendCommand("jrc.sendData('n', d3.select('#Layer2').selectAll('.data_element').size())", wait = 3)
  expect_equal(n, 1)

  session$sendCommand("jrc.sendData('n', d3.select('#Layer3').selectAll('.data_element').size())", wait = 3)
  expect_equal(n, 15)
  
  removeLayer("Chart1", "Layer2")
  
  session$sendCommand("jrc.sendData('n', d3.select('#Chart1').selectAll('.chart_g').size())", wait = 3)
  expect_equal(n, 2)
  
  closePage()
})

test_that("Only one session is updated on click", {
  openPage(allowedVariables = c("red1", "red2"))
  clicked <- 1
  lc_scatter(dat(colour = ifelse(1:10 == clicked, "red", "black")), x = 1:10, y = 1:10,
             transitionDuration = 0,
             on_click = function(d) {
               clicked <<- d
               updateCharts()
             })
  
  app <- getPage()
  s1 <- app$getSession()
  s2 <- app$openPage(FALSE)
  
  s1$sendCommand('f = d3.select("#pLayer1_4").on("click")')
  s1$callFunction('f', list(4), wait = 3)
  expect_equal(clicked, 5)
  
  red1 <- -1
  red2 <- -1
  s1$sendCommand(paste0("jrc.sendData('red1', d3.selectAll('.data_element').filter(function() {",
                          "return d3.select(this).attr('fill') == 'red'",
                        "}).data())"), wait = 3)
  s2$sendCommand(paste0("jrc.sendData('red2', d3.selectAll('.data_element').filter(function() {",
                          "return d3.select(this).attr('fill') == 'red'",
                        "}).data())"), wait = 3)
  expect_equal(red1, 4)
  expect_equal(red2, 0)
  
  closePage()
})

test_that("Session states are stored independently", {
  openPage(sessionVars = list(clicked = 1))
  
  lc_scatter(dat(colour = ifelse(1:10 == clicked, "red", "black")), x = 1:10, y = 1:10, 
             on_click = function(d) {
               clicked <<- d
               updateCharts()
             })
  
  app <- getPage()
  s1 <- app$getSession()
  s2 <- app$openPage(FALSE)
  
  s1$sendCommand('f = d3.select("#pLayer1_4").on("click")')
  s1$callFunction('f', list(4), wait = 3)
  
  expect_false(exists("clicked", inherits = FALSE))
  expect_equal(s1$sessionVariables(varName = "clicked"), 5)
  expect_equal(s2$sessionVariables(varName = "clicked"), 1)

  closePage()
})