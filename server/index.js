const app = require('express')();
const server = require('http').createServer(app);
const io = require('socket.io')(server);

io.on('connection', (socket) => {
  console.log(`new connection: ${socket.id}`);

  io.emit('new connection', {
    id: socket.id,
  });
});

server.listen(3000, () => {
  console.log('Server listening on port 3000');
});
