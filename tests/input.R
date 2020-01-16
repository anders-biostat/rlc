library(rlc)

openPage(F)

lc_input(type = "checkbox", labels = paste0("el", 1:5), on_click = function(value) print(value),
         value = T)
lc_input(type = "radio", labels = paste0("el", 1:5), on_click = function(value) print(value),
         value = 1)
lc_input(type = "text", labels = paste0("el", 1:5), on_click = function(value) print(value),
         value = c("a", "b", "c", "e", "d"))
lc_input(type = "range", labels = paste0("el", 1:5), on_click = function(value) print(value),
         value = 10, max = c(10, 20, 30, 40, 50), step = c(0.5, 0.1, 1, 5, 25))
lc_input(type = "button", labels = paste0("el", 1:5), on_click = function(value) print(value))
