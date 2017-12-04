library( ALL )
library( genefilter )
library( vsn )
library( hgu95av2.db )
library( stringr)
library( hwriter )

data(ALL)
a <- ALL
exprs(a) <- 2^exprs(a)
e <- justvsn(a)
meanSdPlot(e)

es <- e[ , e$mol.biol %in% c("BCR/ABL","NEG") & grepl( "B", e$BT ) ]
ttres <- rowttests( exprs(es), droplevels(es$mol.biol) )
ttres$padj <- p.adjust( ttres$p.value, "BH" )
ttres$gene <- mapIds( hgu95av2.db, rownames(ttres), "SYMBOL", "PROBEID" )

library( rlc )

dfrow2html <- function( a ){
   str_c(
      sapply( names(a), function(n) str_interp( "<b>${n}:</b> ${a[n]}<br>" ) ),
      collapse = "\n" )
}


lc_newpage(FALSE)
selprobe <- 1
selpat <- 1

lc_scatterchart( place="A1", dat( 
      x = rowMeans(exprs(es)),
      y = ttres$dm,
      col = ifelse( ttres$padj < .1, "red", "black" ) ),
   on_click = function(k) {
      selprobe <<- k
      lc_update("A2") })

lc_scatterchart( place="A2", dat(
     x = 1:ncol(es),
     y = exprs(es)[selprobe,],
     col = c( `NEG`="blue", `BCR/ABL`="orange" )[ es$mol.biol ] ),
   on_click = function(k) {
     selpat <<- k
     lc_update("B2") })

lc_rawhtml( place="B1", dat(
   hwrite(t( ttres[selprobe,] ) ) ) )

lc_rawhtml( place="B2", dat(
  hwrite(t( pData(e)[selpat,] ) )) )
