library(rlc)

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

lc_scatter(dat(x = rnorm(30)), y = rnorm(30))
#note that the Y values remain the same after each updateChart call
updateChart()

data("iris")
lc_scatter(dat(x = iris$Sepal.Length, 
               y = iris$Petal.Length,
               colourValue = iris$Petal.Width,
               symbolValue = iris$Species),
           title = "Iris dataset",
           axisTitleY = "Petal Length",
           axisTitleX = "Sepal Length",
           colourLegendTitle = "Petal Width",
           symbolLegendTitle = "Species")

lc_beeswarm(dat(x = iris$Species,
                y = iris$Sepal.Length,
                colourValue = iris$Sepal.Width),
            title = "Iris dataset",
            axisTitleY = "Sepal Length",
            axisTitleX = "Species",
            colourLegendTitle = "Sepal Width")



x <- seq(0, 8, 0.2)
lc_line(dat(x = x, y = cbind(cos(x), sin(x)),
            aspectRatio = 1,
            colour = c("blue", "red"),
            dasharray = c("5", "1 5 5")))



points <- seq(0, 6.5, 0.1)
x <- cos(points)
y <- sin(points)
lc_path(dat(x = sapply(0:2, function(i) x + i), 
            y = sapply(0:2, function(i) y + i),
            fill = c("blue", "red", "black"),
            opacity = c(0.3, 0.5, 0.7)))


x <- seq(0, 5, 0.1, sd = 2)
y <- x*3 + rnorm(length(x))
plot(x, y)
fit <- lm(x ~ y)
pred <- predict(fit, x, se.fit = T)

lc_ribbon(dat(ymin = y - 1.96 * pred$se.fit,
              ymax = y + 1.96 * pred$se.fit,
              x = x,
              colour = "#555555"), id = "ribbonTest")
lc_scatter(dat(x = x, y = y), id = "ribbonTest")


data("esoph")

lc_bars(dat(value = tapply(esoph$ncases, esoph$agegp, sum)))
