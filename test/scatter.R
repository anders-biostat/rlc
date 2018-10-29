#test scatter functionality

data(iris)

#make a simple scater plot
lc_scatter(dat(x = iris$Sepal.Length, y = iris$Sepal.Width), id = "irisScatter")

#try to change properties on the fly, use symbolValue
setProperties(dat(symbolValue = as.character(iris$Species), y = iris$Petal.Length), id = "irisScatter")
updateCharts("irisScatter")

