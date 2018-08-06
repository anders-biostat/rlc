library(rlc)
library(orthogonalsplinebasis)
library(splines2)

load("test3.RData")

selGene <- rownames(exprs)[1]

s <- seq(min(st$timePoint), max(st$timePoint), length.out = 50)
predTable <- data.frame(timePoint = c(s, s), sex = rep(c("Male", "Female"), each = 50))

tp <- st$timePoint
tr <- OBasis(expand.knots(seq(min(tp), max(tp), length.out = 4)))@transformation
tr <- tr[nrow(tr):1, ]
spl <- bSpline(tp, df = 6, intercept = T) %*% tr
splc <- bSpline(predTable$timePoint, df = 6, intercept = T) %*% tr
XPred <- model.matrix(~splc + splc:sex + 0, predTable)[, 1:12]

fitCurve <- function(gene, ret = "curve") {
  dataTable <- data.frame(timePoint = st$timePoint, sex = st$sex, expr = exprs[gene, ], 
                          stringsAsFactors = F)  
  X <- model.matrix(~spl + spl:sex + 0, dataTable)[, 1:12]
  fit <- lm(expr ~ spl + spl:sex + 0, dataTable)
  sigma <- sqrt(sum(fit$residuals^2)/(length(fit$residuals) - 12))
  
  if(ret == "curve"){
    matrix(XPred %*% fit$coefficients[1:12], ncol = 2)
  } else {
    sqrt(1/length(fit$residuals) + diag(XPred %*% solve(t(X) %*% X) %*% t(XPred))) * sigma
  }
}

openPage(layout = "table1x2", useViewer = F)

lc_scatter(dat(x = -log10(pvals$alt), y = -log10(pvals$res), nelements = nrow(exprs), size = 4, opacity  = 0.8,
               on_click = function(i) {
                 selGene <<- rownames(exprs)[i + 1]
                 updateChart("expr")
               }), place = "A1")
lc_ribbon(dat(x = cbind(s, s), ymax = fitCurve(selGene) + 1.96 * fitCurve(selGene, ret = "se"),
              ymin = fitCurve(selGene) - 1.96 * fitCurve(selGene, ret = "se"), colourValue = c("Male", "Female"),
              addColourScaleToLegend = F), 
          id = "expr", place = "A2")
lc_scatter(dat(x = st$timePoint, y = exprs[selGene, ], colourValue = st$sex, height = 300), id = "expr")
lc_line(dat(x = cbind(s, s), y = fitCurve(selGene), colourValue = c("Male", "Female"), 
            addColourScaleToLegend = F), id = "expr")

closePage()
