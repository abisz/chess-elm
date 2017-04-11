const app = require('express')();
const server = require('http').createServer(app);
const io = require('socket.io')(server);

io.on('connection', (socket) => {
  socket.broadcast.emit('new connection', {
    foo: "bar",
  });
});

server.listen(3000, () => {
  console.log('Server listening on port 3000');
});
