const app = require('express')();
const server = require('http').createServer(app);
const io = require('socket.io')(server);
const Chess = require('chess.js').Chess;
const chess = new Chess();

chess.move('e4');

console.log(chess.ascii());


io.on('connection', (socket) => {
  console.log(`new connection: ${socket.id}`);

  io.emit('new connection', {
    id: socket.id,
  });

  socket.on('move', (data) => {
    console.log('socket move', data);
  });
});

server.listen(3000, () => {
  console.log('Server listening on port 3000');
});
