# Raw data on GEO:
# ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE100nnn/GSE100866/suppl/GSE100866_CBMC_8K_13AB_10X-ADT_umi.csv.gz
# ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE100nnn/GSE100866/suppl/GSE100866_CBMC_8K_13AB_10X-RNA_umi.csv.gz
library(rlc)
library(RColorBrewer)

rawCounts_RNA <- as.matrix( read.csv( "~/Downloads/GSE100866_CBMC_8K_13AB_10X-RNA_umi.csv.gz", header=TRUE, row.names=1 ) )
rawCounts_ADT <- as.matrix( read.csv( "~/Downloads/GSE100866_CBMC_8K_13AB_10X-ADT_umi.csv.gz", header=TRUE, row.names=1 ) )

cellOrder <- sample(colnames(rawCounts_RNA))

rawCounts_RNA <- rawCounts_RNA[, cellOrder]
rawCounts_ADT <- rawCounts_ADT[, cellOrder]

# They have spiked in ~ 4% mouse fibroblast cells 
# Calculate for each cell the ratio of human to mouse genes:
gene_species <- sapply( strsplit( rownames(rawCounts_RNA), "_" ), `[`, 1 ) 
table( gene_species )
cell_counts_by_species <- sapply( unique( gene_species ), function(x) 
   colSums( rawCounts_RNA[ gene_species == x, , drop=FALSE ] ) )
head(cell_counts_by_species)
hist( log2( cell_counts_by_species[,"HUMAN"] / cell_counts_by_species[,"MOUSE"] ))

# subset raw counts matrix to only those cells that have >90% human UMIs, and to only human genes
rawCounts_RNA <- rawCounts_RNA[ 
  gene_species == "HUMAN",
  cell_counts_by_species[,"HUMAN"] / cell_counts_by_species[,"MOUSE"] > 10 ]
rownames(rawCounts_RNA) <- sub( "HUMAN_", "", rownames(rawCounts_RNA) )
rawCounts_ADT <- rawCounts_ADT[ , colnames(rawCounts_RNA)]

# sum of UMIs per cell to nomalize
countsums <- colSums( rawCounts_RNA )

means <- colMeans( t(rawCounts_RNA) / countsums )
vars <- genefilter::rowVars(t( t(rawCounts_RNA) / countsums ))

# let's use this as first plot
plot( means, vars, pch=".", log="xy" )
xg <- 10^seq( -9, -1, length.out=1000 )
lines( xg, xg * mean(1/countsums), col="red" )


nCounts <- sqrt( t( rawCounts_RNA ) / countsums )
varGenes <- vars / means / mean(1/countsums) > 1.5 & means > 1e-6
# Calculate a t-SNE
tsne_res <- Rtsne::Rtsne( nCounts[, varGenes] )

selGene <- rownames(rawCounts_RNA)[1]

openPage(layout = "table2x2", useViewer = F)

#means vs. vars
lc_scatter(dat(x = means, y = vars / means / mean(1/countsums), logScaleX = 10, logScaleY = 10, size = 2,
               on_click = function(i) {
                 selGene <<- rownames(rawCounts_RNA)[i]
                 updateChart("expression")
                 updateChart("tsne")
                }), place = "A1")
lc_hLine(dat(h = 1, colour = "red"), id = "A1")

#expression, coloured by one of the ADTs
lc_scatter(dat(y = rawCounts_RNA[selGene,]/countsums, 
               colourValue = rawCounts_ADT["CD3", ], size = 3, title = selGene,
               palette = brewer.pal(9, "YlOrRd")), place = "A2", id = "expression")

#tsne
lc_scatter(dat(x = tsne_res$Y[, 1], y = tsne_res$Y[, 2], 
               colourValue = rawCounts_RNA[selGene,]/countsums * 100000, size = 2,
               palette = brewer.pal(9, "YlOrRd"), markedUpdated = function() print(getMarked("tsne"))), id = "tsne", place = "B1")


####compare UMAP to TSNE

umap_res <- umap(nCounts[, varGenes])
distsFrom <- function(point) {
  sqrt(colSums((t(nCounts[, varGenes]) - nCounts[point, varGenes])^2))
}
activeCell <- 1

openPage(useViewer = F, layout = "table1x2")

lc_scatter(dat(x = tsne_res$Y[, 1], y = tsne_res$Y[, 2], title = "TSNE", 
               colourValue = distsFrom(activeCell), opacity = 0.2, size = 3,
               colourDomain = c(0, 0.5),
               elementMouseOver = function(k) {
                 activeCell <<- k
                 updateChart(c("tsne", "umap"))
               }), id = "tsne", place = "A1", parcerStep = 200)

lc_scatter(dat(x = umap_res$layout[, 1], y = umap_res$layout[, 2], title = "UMAP", 
               colourValue = distsFrom(activeCell), opacity = 0.2, size = 3,
               colourDomain = c(0, 0.5),
               elementMouseOver = function(k) {
                 activeCell <<- k
                 updateChart(c("tsne", "umap"))
               }), id = "umap", place = "A2", parcerStep = 300)
