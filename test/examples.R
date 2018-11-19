library(rlc)

data(iris)

#store some properties in global variables
width <- 300
height <- 300
colour <- iris$Sepal.Width
#create a chart
lc_scatter(dat(x = iris$Sepal.Length, y = iris$Petal.Length, colorValues = colour,
               width = width, height = height), id = "iris")


x <- as.factor(iris$Species)
x <- relevel(x,"virginica") 

lc_scatter(dat(x = x, y = iris$Petal.Length, colorValues = colour,
               width = width, height = height), id = "iris")

#change the variables
height <- 400
colour <- iris$Petal.Width

#this will change colour of points and chart height
updateCharts("iris")
#this will change only height
updateCharts("iris", updateOnly = "Size")

#add another property
setProperties(dat(symbolValue = iris$Species), "iris")
#this will change only colour and symbols
updateCharts("iris", updateOnly = "ElementStyle")

a <- 1
b <- 0
lc_abLine(dat(a = a, b = b), id = "iris")

b <- -2
updateCharts("iris", "Layer2")


lc_scatter(dat(x = 1:10, y = 1:10, colourValue = 1:10 * 10^-20))
lc_html("abcde\", ''' ljlkj")

lc_scatter(dat(x = iris$Sepal.Length), y = iris$Petal.Length, colourValue = colour,
               width = width, height = height, id = "iris")

lc_scatter(dat(x = rnorm(30)), y = rnorm(30))
#note that the Y values remain the same after each updateCharts call
updateCharts()

data("iris")
lc_scatter(dat(x = iris$Sepal.Length, 
               y = iris$Petal.Length,
               colourValue = iris$Petal.Width,
               symbolValue = iris$Species),
           title = "Iris dataset",
           axisTitleY = "Petal Length",
           axisTitleX = "Sepal Length",
           colourLegendTitle = "Petal Width",
           symbolLegendTitle = "Species",
           showLegend = F,
           id = "scatter")

lc_colourSlider(chart = "scatter")

lc_beeswarm(dat(x = iris$Species,
                y = iris$Sepal.Length,
                colourValue = iris$Sepal.Width,
                on_click = function(i) {print(i)}),
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


x <- seq(0, 5, 0.1)
y <- x*3 + rnorm(length(x), sd = 2)
plot(x, y)
fit <- lm(x ~ y)
pred <- predict(fit, data.frame(x = x), se.fit = T)

lc_ribbon(dat(ymin = y - 1.96 * pred$se.fit,
              ymax = y + 1.96 * pred$se.fit,
              x = x,
              colour = "#555555"), id = "ribbonTest")
lc_scatter(dat(x = x, y = y), id = "ribbonTest")


data("esoph")

lc_bars(dat(value = tapply(esoph$ncases, esoph$agegp, sum), 
            title = "Number of cases per age group",
            axisTitleX = "Age group", 
            axisTitleY = "Number of esophageal cases"))

lc_bars(dat(value = c(tapply(esoph$ncases, esoph$agegp, sum), tapply(esoph$ncontrols, esoph$agegp, sum)),
            stackIds = c(rep("case", 6), rep("control", 6))))

#It is ease to put data in a convenient form for barplots using tidyverse
library(tidyverse)

esoph %>%
  gather(type, cases, (ncases:ncontrols)) %>%
  mutate(type = str_sub(type, 2, -2)) %>%
  group_by(agegp, alcgp, type) %>%
  summarise(ncases = sum(cases)) -> newData

lc_bars(dat(value = newData$ncases,
            stackIds = newData$type,
            barIds = newData$alcgp,
            groupIds = newData$agegp))


#create a test matrix
test <- cbind(sapply(1:10, function(i) c(rnorm(10, mean = 1, sd = 3), 
                                 rnorm(6, mean = 5, sd = 2), 
                                 runif(14, 0, 8))),
              sapply(1:10, function(i) c(rnorm(10, mean = 3, sd = 2), 
                                         rnorm(6, mean = 1, sd = 2), 
                                         runif(14, 0, 8))))
test[test < 0] <- 0
rownames(test) <- paste0("Gene", 1:30)
colnames(test) <- paste0("Sample", 1:20)

lc_heatmap(dat(value = test))

# when you want to cluster rows or columns, it can be
# a good idea to make bottom and right margins larger to
# fit labels
lc_heatmap(dat(value = test),
           clusterRows = T,
           clusterCols = T,
           margins = list(top = 50, left = 30, bottom = 75, right = 75))

lc_heatmap(dat(value = cor(test), 
               colourDomain = c(-1, 1),
               palette = RColorBrewer::brewer.pal(11, "RdYlBu")))


lc_hLine(dat(h = seq(1, 9, 1), domainX = c(0, 10), domainY = c(0, 10)), id = "grid")
lc_vLine(dat(v = seq(1, 9, 1)), id = "grid")


noise <- rnorm(30)
x <- seq(-4, 4, length.out = 30)

lc_scatter(dat(x = x,
               y = sin(x) + noise,
               colourValue = noise), id = "plot", layerId = "points")
lc_line(dat(x = x, y = sin(x)), id = "plot")
lc_colourSlider(chart = "plot", layer = "points")

listCharts()
