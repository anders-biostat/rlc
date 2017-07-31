library( rlc )


lc_newpage( FALSE );

which_red <- 13

lc_scatterchart( place="A1",
   {  x <- seq( 0, 2*pi, length.out=100 )
      y <- sin( x )
      col <- ifelse( seq_along(x) %in% which_red, "red", "black" )
   },
   on_click = function(k) { 
      which_red <<- k;      
      lc_update() } )

lc_scatterchart( place="A2",
   {  x <- seq( 0, 2*pi, length.out=100 )
      y <- cos( x )
      col <- ifelse( seq_along(x) %in% which_red, "blue", "black" )
   },
   on_click = function(k) { 
      which_red <<- k;      
      lc_update() } )






for( which_red in 1:100 ) {
   lc_update()
   Sys.sleep(.01) 
}

