data(iris)

#store some properties in global variables
width <- 300
height <- 300
colour <- iris$Sepal.Width
#create a chart
lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length, colourValue = colour,
               width = width, height = height), id = "iris")

#change the variables
height <- 400
colour <- iris$Petal.Width

#this will change colour of points and chart height
updateChart("iris")
#this will change only height
updateChart("iris", updateType = "Size")

#add another property
setProperties(dat(symbolValue = iris$Species), "iris")
#this will change only colour and symbols
updateChart("iris", updateType = "ElementStyle")

a <- 1
b <- 0
lc_abLine(dat(a = a, b = b), id = "iris")

b <- -2
updateChart("iris", "Layer2")


lc_scatter(dat(x = 1:10, y = 1:10, colourValue = 1:10 * 10^-20))
lc_html("abcde\", ''' ljlkj")


lc_scatter(dat(x = iris$Sepal.Length), y = iris$Petal.Length, colourValue = colour,
               width = width, height = height, id = "iris")
