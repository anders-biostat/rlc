library(rlc)

load("barplots.RData")

fitPvals <- apply(fitPvals, 2, p.adjust, method = "BH")

lc_bars(dat(value = colSums(resPvals < 0.01, na.rm = T), height = 250))

lc_bars(dat(value = c(colSums(resPvals < 0.1, na.rm = T), colSums(fitPvals < 0.1, na.rm = T)), 
            barIds = rep(c("res", "fit"), each = 5),
            groupIds = c(colnames(resPvals), colnames(fitPvals)), height = 250))

values <- c(colSums(resPvals < 0.1, na.rm = T), colSums(fitPvals < 0.1, na.rm = T))
inCommon <- sapply(1:5, function(i) {
  sum(resPvals[, i] < 0.1 & fitPvals[, i] < 0.1, na.rm = T)
})
dif <- values - rep(inCommon, times = 2)


lc_bars(dat(value = c(rep(inCommon, times = 2), values), 
            barIds = rep(rep(c("res", "fit"), each = 5), times = 2),
            groupIds = rep(c(colnames(resPvals), colnames(fitPvals)), times = 2), 
            stackIds = rep(c("in common", "not in common"), each = 10),
            height = 250))

closePage()
