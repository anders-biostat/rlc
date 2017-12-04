charts = {}

new_scatterChart = function( id, place ) {
   charts[id]  = lc.scatterChart();
   charts[id].Data = { x: [0,0,1,1], y: [0,1,0,1], col: ["black","black","black","black"] };
   charts[id]
    .npoints( 4 )
    .transitionDuration( 0 )
    .size( 3.5 )
    .x( function( k ) { return charts[id].Data.x[k] } )
    .y( function( k ) { return charts[id].Data.y[k] } )
    .colour( function( k ) { return charts[id].Data.col[k] } )
    .on_click( function( k ) { ws.send( JSON.stringify(
        [ "EVENT", id, "click", k ] ) ) } )
    .place( place );
}

new_rawHtml = function( id, place ) {
   charts[id] = { 
      Data: "",
      update: function(){
         d3.select( place ).html( charts[id].Data )
      }
   } 
}

// establish WebSocket link and handlers 
ws = new WebSocket( "ws://localhost:1237/" ); //, "RLC-0" );
ws.addEventListener( "open", function(event) { 
   // ...
} ); 
ws.addEventListener( "message", function(event) {
   msg = JSON.parse( event.data );
   if( msg[0] == "NEWCHART" ) {
      // msg[1]: type, msg[2]: id, msg[3]: selector
      if( msg[1] == "scatterChart" ) {
         new_scatterChart( msg[2], msg[3] );
      } else if ( msg[1] == "rawHtml" ) {
         new_rawHtml( msg[2], msg[3] );
      }
   } else if ( msg[0] == "NEWDATA" ) {
      if( !( msg[1] in charts ) ) {
         throw "Unknown chart ID"
      } 
      charts[ msg[1] ].Data = msg[2]
      try{ //if( charts[ msg[1] ].type == "scatterChart" ) {  // FIXME
         charts[ msg[1] ].npoints( msg[2].x.length )
      } catch(e) {};
      charts[ msg[1] ].update()
   }
   // ...
} );
ws.addEventListener( "close", function(event) { 
   window.close()
} ); 
