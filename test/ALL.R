library( ALL )
library( genefilter )
library( vsn )
library( hgu95av2.db )

data(ALL)
a <- ALL
exprs(a) <- 2^exprs(a)
e <- justvsn(a)
meanSdPlot(e)

es <- e[ , e$mol.biol %in% c("BCR/ABL","NEG")]
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
lc_scatterchart( place="A1", dat( 
      x = rowMeans(exprs(es)),
      y = ttres$dm,
      col = ifelse( ttres$padj < .1, "red", "black" ) ),
   on_click = function(k) {
      selprobe <<- k
      lc_update() })
lc_scatterchart( place="A2", dat(
   x = 1:ncol(e),
   y = exprs(e)[selprobe,],
   col = substr( rainbow(6,v=.8)[ as.integer(e$mol.biol) ], 1, 7 ) ) )
lc_rawhtml( place="B1", dat(
   dfrow2html( ttres[selprobe,] ) ) )

