data("iris")

nas <- sample(nrow(iris), 3)
iris$Species[nas] <- NA
iris$Sepal.Length[nas] <- NA

openPage(layout = "table1x2", useViewer = F)
lc_scatter(dat(
  x = iris$Sepal.Length,
  y = iris$Petal.Length,
  colourValue = iris$Species,
  on_marked = function() {
    mark(getMarked("A1"), "A2")
  }
), "A1", width = 300, height = 300)

lc_scatter(dat(
  x = iris$Sepal.Width,
  y = iris$Petal.Width,
  colourValue = iris$Species,
  on_marked = function() {
    mark(getMarked("A2"), "A1")
  }
), "A2", width = 300, height = 300)

