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
         stop( "Binary message received via WebSocket" )
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


handle_event <- function( chart_id, event_type, event_data ) {
}

lc_newpage <- function( use_viewer=TRUE ) {

   if( !is.null(pageobj$httpuv_handle) ) {
      stopDaemonizedServer(pageobj$httpuv_handle )
   }
   
   rm( list=ls(pageobj),  envir=pageobj )
   
   pageobj$app <- list( 
      call = handle_http_request,
      onWSOpen = handle_websocket_open )

   pageobj$httpuv_handle <- startDaemonizedServer( "0.0.0.0", 1237, pageobj$app )
   if( use_viewer )
      getOption("viewer")( "http://localhost:1237/init.html" )
   else
      browseURL( "http://localhost:1237/init.html" )
      
   for( i in 1:(5/0.05) ) {
      if( !is.null(pageobj$websocket) ) 
         break
      Sys.sleep( .05 )
   }
   if( is.null(pageobj$websocket) ) 
      stop( "Timeout waiting for websocket." )
   
   pageobj$charts <- new.env()
   
   invisible(TRUE)
}

scatterchart_update <- function( id ) {

   e <- new.env()
   tryCatch(
      eval( pageobj$charts[[id]]$data_expr, envir=e ),
      error = function(e) 
         stop( str_interp( "in data expression for chart '${id}': ${e$message}." ), call.=FALSE ) ) 
   
   if( !exists( "x", envir=e, inherits=FALSE ) )
      stop( str_interp( "Data expression for chart '${id}': 'x' is missing." ) )
   if( !exists( "y", envir=e, inherits=FALSE ) )
      stop( str_interp( "Data expression for chart '${id}': 'y' is missing." ) )
   if( !is.vector( e$x ) || !is.numeric( e$x ) )
      stop( str_interp( "Data expression for chart '${id}': 'x' is not a numeric vector." ) )
   if( !is.vector( e$y ) || !is.numeric( e$y ) )
      stop( str_interp( "Data expression for chart '${id}': 'y' is not a numeric vector." ) )
   if( length(e$x) != length(e$y) )
      stop( str_interp( "Data expression for chart '${id}': 'x' and 'y' differ in length." ) )

   if( !exists( "col", envir=e, inherits=FALSE ) ) 
      e$col <- rep( "black", length(e$x) )

   if( !is.vector( e$col ) || !is.character( e$col ) )
      stop( str_interp( "Data expression for chart '${id}': 'col' is not a character vector." ) )
   if( length(e$col) != length(e$x) )
      stop( str_interp( "Data expression for chart '${id}': 'col' and 'x' differ in length." ) )
   
   pageobj$websocket$send( toJSON( 
      list( unbox("NEWDATA"), unbox(id), list( x=e$x, y=e$y, col=e$col ) ) ) )   

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
   } else {
      stop( str_interp( "Encountered unknown chart type '${pageobj$charts[[id]]$type}'." ) )
   }
}

place_chart <- function( chart_type, data_expr, callback, place, id ) {
   
   if( exists( id, envir=pageobj$charts, inherits=FALSE ) )
      stop( str_interp( "There already is a chart with id '${id}'." ) )
   
   pageobj$charts[[id]] <- 
      list( type = chart_type, data_expr = data_expr, callback = callback )
   
   pageobj$websocket$send( toJSON( c( 
      "NEWCHART", chart_type, id, str_c( "#", place ) ) ) )

   lc_update( id )   
   
}


lc_scatterchart <- function( data_expr, place, id=place, on_click ) {
   
   callback <- function( event_type, event_data ) {
      if( event_type == "click" )
         on_click( as.integer(event_data) + 1 )
   }
   
   place_chart( "scatterChart", substitute( data_expr ), callback, place, id )
   
}

