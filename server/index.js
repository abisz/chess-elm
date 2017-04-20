const app = require('express')();
const server = require('http').createServer(app);
const io = require('socket.io')(server);
const Chess = require('chess.js').Chess;
const chess = new Chess();

console.log(chess.ascii());


io.on('connection', (socket) => {
  console.log(`new connection: ${socket.id}`);

  io.emit('new connection', {
    id: socket.id,
  });

  socket.on('move', (move) => {
    const from = move.slice(0, 2);
    const to = move.slice(2, 4);

    console.log('Attempting Move:');
    console.log('from:', from);
    console.log('to:', to);

    console.log({ from, to });

    if (chess.move({ from, to })) {
      console.log(chess.ascii());

      io.emit('update', {
        fen: chess.fen()
      });
    } else {
      console.log('move is not legal');
    }
  });

  socket.on('getBoard', () => {
    socket.emit('update', {
      fen: chess.fen()
    });
  });
});

server.listen(3000, () => {
  console.log('Server listening on port 3000');
});
