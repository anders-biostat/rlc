library(Seurat)
library(rlc)

load("droplet_Kidney_seurat_tiss.Robj")

plot( tiss@dr$tsne@cell.embeddings, asp=1, col=as.integer(tiss@ident) )

distsFrom <- function(point) {
  sqrt(colSums((t(tiss@dr$pca@cell.embeddings) - tiss@dr$pca@cell.embeddings[point, ])^2))
}

clusts <- F
activePoint <- 1

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