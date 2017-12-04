pageobj <- new.env()

handle_http_request <- function( req ) {
   
   filename <- req$PATH_INFO
   
   filename = system.file( str_replace( req$PATH_INFO, "^/", "http_root/" ), package="rlc" )
   
   if( !file.exists(filename) ) {
      return( list( 
         status = 404L,
         headers = list( "Content-Type" = "text/html" ),
         body = "404: Resource not found" ) )
   }
   
   file_extension = str_extract( filename, "(?<=\\.)[^\\.]*$" )
   
   if( file_extension == "html" )
      content_type <- "text/html"
   else if( file_extension == "js" )
      content_type <- "text/javascript"
   else if( file_extension == "css" )
      content_type <- "text/css"
   else {
      content_type <- "text";
      print( filename )
      warning( "Serving file of unknown content type (neither .html nor .js nor .css)." )
   }
   
   list(
         status = 200L,
         headers = list( 'Content-Type' = content_type ),
         body = str_c( readLines( filename, warn=FALSE ), collapse="\n" )
      )
}


handle_websocket_open <- function( ws ) {

   #if( ws$request$HTTP_SEC_WEBSOCKET_PROTOCOL != "RLC-0" )
   #   stop( str_c( "Unknown WebSocket protocol", 
   #      ws$request$HTTP_SEC_WEBSOCKET_PROTOCOL ) )
   ws$onMessage( function( isBinary, msg ) {
      if( isBinary )
         stop( "Unexpected binary message received via WebSocket" )
      msg <- fromJSON( msg, simplifyVector=FALSE )
      if( !is.character( msg[[1]] ) || length(msg[[1]]) != 1 ) {
         stop( "Malformed message received via WebSocket" )
      }
      if( msg[[1]] == "EVENT" ) {
         # msg[[2]]: chart ID
         # msg[[3]]: event type (e.g., "click")
         # msg[[4]]: event data
         if( !exists( msg[[2]], envir=pageobj$charts, inherits=FALSE ) )
            stop( str_interp( "Event message received for chart with unknown id '${msg[[2]]}'." ) )
         pageobj$charts[[msg[[2]]]]$callback( msg[[3]], msg[[4]] )
      } else if( msg[[1]] == "ERROR" ) {
         # ...
      } else
         stop( "Unknown message code received via WebSocket" )
   } );
   pageobj$websocket <- ws
}

lc_stopserver <- function() {

   if( !is.null(pageobj$httpuv_handle) ) {
      if( !is.null(pageobj$websocket) ) {
         pageobj$websocket$close()
      }
      stopDaemonizedServer(pageobj$httpuv_handle )
   }
   
   rm( list=ls(pageobj), envir=pageobj )
}

.onUnload <- function( libpath ) {
   lc_stopserver()
}

lc_newpage <- function( use_viewer=TRUE ) {

   lc_stopserver()
   
   pageobj$app <- list( 
      call = handle_http_request,
      onWSOpen = handle_websocket_open )

   pageobj$httpuv_handle <- startDaemonizedServer( "0.0.0.0", 1237, pageobj$app )
   if( use_viewer & !is.null( getOption("viewer") ) )
      getOption("viewer")( "http://localhost:1237/init.html" )
   else
      browseURL( "http://localhost:1237/init.html" )
   
   # Wait up to 5 seconds for the a websocket connection
   # incoming from the client
   for( i in 1:(5/0.05) ) {
      if( !is.null(pageobj$websocket) ) 
         break
      Sys.sleep( .05 )
   }
   if( is.null(pageobj$websocket) ) {
      lc_stopserver()
      stop( "Timeout waiting for websocket." )
   }
   pageobj$charts <- new.env()   # WRONG: Should be further up!
   
   invisible(NULL)
}

scatterchart_update <- function( id ) {

   e <- new.env()
   tryCatch(
      { d <- pageobj$charts[[id]]$data_fun() },
      error = function(e) 
         stop( str_interp( "in data expression for chart '${id}': ${e$message}." ), call.=FALSE ) ) 
   
   if( ! "x" %in% names(d) )
      stop( str_interp( "Data expression for chart '${id}': 'x' is missing." ) )
   if( ! "y" %in% names(d) )
      stop( str_interp( "Data expression for chart '${id}': 'y' is missing." ) )
   if( !is.vector( d$x ) || !is.numeric( d$x ) )
      stop( str_interp( "Data expression for chart '${id}': 'x' is not a numeric vector." ) )
   if( !is.vector( d$y ) || !is.numeric( d$y ) )
      stop( str_interp( "Data expression for chart '${id}': 'y' is not a numeric vector." ) )
   if( length(d$x) != length(d$y) )
      stop( str_interp( "Data expression for chart '${id}': 'x' and 'y' differ in length." ) )

   if( ! "col" %in% names(d) ) 
      d$col <- rep( "black", length(e$x) )

   if( !is.vector( d$col ) || !is.character( d$col ) )
      stop( str_interp( "Data expression for chart '${id}': 'col' is not a character vector." ) )
   if( length(e$col) != length(e$x) )
      stop( str_interp( "Data expression for chart '${id}': 'col' and 'x' differ in length." ) )
   
   pageobj$websocket$send( toJSON( 
      list( unbox("NEWDATA"), unbox(id), list( x=d$x, y=d$y, col=d$col ) ) ) )   

}

rawhtml_update <- function( id ) {
   
   e <- new.env()
   tryCatch(
      d <- pageobj$charts[[id]]$data_fun(),
      error = function(e) 
         stop( str_interp( "in data expression for chart '${id}': ${e$message}." ), call.=FALSE ) ) 

   if( !is.character(d[[1]]) | length(d[[1]]) != 1 )
      stop( str_interp( "Data expression for chart '${id}': did not return a single character string" ) ) 

   pageobj$websocket$send( toJSON( 
      list( unbox("NEWDATA"), unbox(id), unbox(d[[1]]) ) ) )   
   
}   

lc_update <- function( id=NULL ) {
   
   if( is.null( id ) ) {
      for( id in ls(pageobj$charts) )
         lc_update( id )
   }
   
   if( !exists( id, envir=pageobj$charts, inherits=FALSE ) )
      stop( str_interp( "There is no chart with id '${id}'." ) )
   if( pageobj$charts[[id]]$type == "scatterChart" ) {
      scatterchart_update( id )
   } else if( pageobj$charts[[id]]$type == "rawHtml" ) {
      rawhtml_update( id )
   } else
      stop( str_interp( "Encountered unknown chart type '${pageobj$charts[[id]]$type}'." ) )
}

place_chart <- function( chart_type, data_fun, callback, place, id ) {
   
   if( exists( id, envir=pageobj$charts, inherits=FALSE ) )
      stop( str_interp( "There already is a chart with id '${id}'." ) )
   
   pageobj$charts[[id]] <- 
      list( type = chart_type, data_fun = data_fun, callback = callback )
   
   pageobj$websocket$send( toJSON( c( 
      "NEWCHART", chart_type, id, str_c( "#", place ) ) ) )

   lc_update( id )   
   
}


lc_scatterchart <- function( data_fun, place, id=place, on_click=function(k){} ) {
   
   callback <- function( event_type, event_data ) {
      if( event_type == "click" )
         on_click( as.integer(event_data) + 1 )
   }
   
   place_chart( "scatterChart", data_fun, callback, place, id )
   
}

lc_rawhtml <- function( data_fun, place, id=place ) {
   
   callback <- function( event_type, event_data ) {
   }
   
   place_chart( "rawHtml", data_fun, callback, place, id )
   
}

dat <- function( ... ) {
   e <- parent.frame()
   l <- match.call()[-1]
   function() lapply( l, eval, e )
}

