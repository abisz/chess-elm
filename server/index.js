const app = require('express')();
const server = require('http').createServer(app);
const io = require('socket.io')(server);
const Chess = require('chess.js').Chess;

const clientSockets = [];
const clientRoom = new Map();
const games = new Map();

io.on('connection', (socket) => {
  console.log(`new connection: ${socket.id}`);

  clientSockets.push(socket.id);

  // io.emit('new connection', {
  //   id: socket.id,
  // });
  
  socket.on('connection', (data) => {
    console.log(`connection request: ${data}`);
    
  });

  socket.on('disconnect', () => {
    console.log(`Socket disconnected: ${socket.id}`);
    const socketIndex = clientSockets.indexOf(socket.id);
    clientSockets.splice(socketIndex, 1);
  });
  
  socket.on('connectToRoom', (room) => {
    console.log(`${socket.id} is trying to connect to room: ${room}`);
    socket.join(room);
    clientRoom.set(socket.id, room);

    if (!games.has(room)) {
      games.set(room, new Chess());
    }

    io.to(room).emit('update', {
      fen: games.get(room).fen()
    });
  });

  socket.on('move', (moveString) => {
    const from = moveString.slice(0, 2);
    const to = moveString.slice(2, 4);
    const room = clientRoom.get(socket.id);
    const chess = games.get(room);

    if (chess.move({ from, to })) {
      io.to(room).emit('update', {
        fen: chess.fen()
      });
    } else {
      console.log('move is not legal');
    }
  });

  socket.on('getBoard', () => {
    const chess = games.get(socket.id);

    if (chess) {
      socket.emit('update', {
        fen: chess.fen()
      });
    }
  });
});

server.listen(3000, () => {
  console.log('Server listening on port 3000');
});

