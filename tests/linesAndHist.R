library(rlc)

openPage(useViewer = F)

lc_hist(dat(value = rnorm(1000), nbins = 30, height = 300))

lc_dens(dat(value = rnorm(1000), height = 300))

lc_scatter(dat(x = 1:10, y = 1:10, height = 300), chartId = "pointsAndLines")
lc_abLine(dat(a = 1:2, b = c(0, 0)), chartId = "pointsAndLines", addLayer = T)
lc_hLine(dat(h = 4), chartId = "pointsAndLines", addLayer = T)
lc_vLine(dat(v = 4), chartId = "pointsAndLines", addLayer = T)

closePage()
