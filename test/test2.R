library(Seurat)
library(rlc)

load("droplet_Kidney_seurat_tiss.Robj")

distsFrom <- function(point) {
  sqrt(colSums((t(tiss@dr$pca@cell.embeddings) - tiss@dr$pca@cell.embeddings[point, ])^2))
}

clusts <- T
activePoint <- 1
openPage(useViewer = F)
lc_scatter(dat(x = tiss@dr$tsne@cell.embeddings[, 1], y = tiss@dr$tsne@cell.embeddings[, 2],
               size = 4, width = 800, height = 800,
               colourValue = (if(clusts) as.character(tiss@ident) else distsFrom(activePoint)),
               transitionDuration = 0,
               on_click = function(k){
                 clusts <<- !clusts
                 updateChart("tsne", updateType = "ElementStyle")
               },
               elementMouseOver = function(k) {
                activePoint <<- k + 1
                if(!clusts){
                  updateChart("tsne", updateType = "ElementStyle")
                }
               }), id = "tsne")

closePage()
