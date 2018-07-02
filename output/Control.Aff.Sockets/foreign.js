function createConnectionEmitterImpl(left, right, options, emit)
{
  const server = require('net').createServer(options);

  // Make the server accept connections.
  server.listen(options.port, options.host);
  // Return a new connection.
  server.on('connection',
    function(connection)
    {
      emit( left(connection) )();
    });
  server.on('error',
    function( error )
    {
      // Show or log the error.
      server.close(function()
        {
          // log or show that all connections are terminated and the server has fully ended.
        });
      // Finish the Producer.
      emit(right({}))();
    });
}
exports.createConnectionEmitterImpl = createConnectionEmitterImpl;


function createMessageEmitterImpl(connection, emit)
{
  connection.on("data",
    function(a)
    {
      emit(a + "")();
    });

  // https://nodejs.org/docs/latest-v6.x/api/net.html#net_event_error
  // Emitted when an error occurs. The 'close' event will be called
  // directly following this event.
  connection.on('error',
    function(error)
    {
      console.log( "Error on the connection: " + error );
      // Half-closes the socket. i.e., it sends a FIN packet.
      // It is possible the server will still send some data.
      //
      // Therefore we do not close the producer, but wait till the other
      // side sends a FIN packet, too.
      connection.end();
    });

  // https://nodejs.org/docs/latest-v6.x/api/net.html#net_event_close
  // Emitted once the socket is fully closed. The argument had_error is a boolean
  // which says if the socket was closed due to a transmission error.
  connection.on('close',
    function(had_error)
    {
      if ( had_error )
      {
        console.log("The Perspectives Core has hung up because of an error.")
      }
      else
      {
        console.log("The Perspectives Core has hung up.")
      }
      // No data will come anymore. Finish the producer.
      emit("shutdown")();
    });

  // https://nodejs.org/docs/latest-v6.x/api/net.html#net_event_end
  // Emitted when the other end of the socket sends a FIN packet.
  // By default (allowHalfOpen == false) the socket will destroy its file
  // descriptor once it has written out its pending write queue.
  connection.on('end',
    function()
    {
      // This means the other side will no longer send data, hence we
      // finish the producer.
      emit("shutdown")();
    });
}
exports.createMessageEmitterImpl = createMessageEmitterImpl;

function writeMessageImpl(s,d) {
  return function() { return s.write(d); };
}
exports.writeMessageImpl = writeMessageImpl;

function connectToServerImpl(o) {
  return require('net').createConnection(o);
}
exports.connectToServerImpl = connectToServerImpl;
