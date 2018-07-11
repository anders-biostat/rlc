library(rlc)

load("test1.RData")

pvals <- apply(counts, 1, function(row) {
  wilcox.test(row ~ cellType)$p.value
})
padjs <- p.adjust(pvals, method = "BH")
log2Fold <- apply(counts, 1, function(row) {
  log2(mean(row[cellType == "T cell"])/mean(row[cellType != "T cell"]))
})

selGene <- rownames(counts)[1]
openPage(layout = "table1x2")

lc_scatter(dat(x = log2Fold, y = -log10(pvals), colourValue = (padjs < 0.1), 
               size = 4, showLegend = F, on_click = function(i) {
                 selGene <<- rownames(counts)[i]
                 updateChart("beeswarm")
              }), place = "A1")
lc_beeswarm(dat(x = cellType, y = counts[selGene, ], size = 3), id = "beeswarm", place = "A2")

closePage()
