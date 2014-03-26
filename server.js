var app = require('http').createServer(handler)
  , io = require('socket.io').listen(app)
  , fs = require('fs')
  , five = require("johnny-five")
  , board = new five.Board()

app.listen(8000);

//webserver
function handler (req, res) {
  if (req.url == "/"){
      req.url = "/index.html"
  }
  fs.readFile(__dirname + req.url,
  function (err, data) {
    if (err) {
      //console.log(err);
      res.writeHead(500);
      return res.end('Error loading '+req.url);
    }
    res.writeHead(200);
    res.end(data);
  });
}


board.on("ready", function() {
    var led = new five.Led(13),
        buttonU = new five.Button(1),
        buttonD = new five.Button(2),
        buttonL = new five.Button(3),
        buttonR = new five.Button(4);

    function led_off(){
        led.stop().off();
    }

    console.log("BOARD ready!");

    io.sockets.on('connection', function (socket) {
        console.log("SOCKETS ready!");
        //The connection event fires every time the webpage is reloaded
        //So that fixes any problem of the ready callbacks  being out of order

        //sending button info to browser/Elm
        function update(){
            socket.emit("game_move", [ buttonU.downValue, buttonD.downValue,
                buttonL.downValue, buttonR.downValue]); 
        }
        buttonU.on("press", update);
        buttonD.on("press", update);
        buttonL.on("press", update);
        buttonR.on("press", update);

        //sending received game info to LED
        socket.on('sound', function (data) {
            if (data == "Goal"){
                led.strobe(100)
                setTimeout(led_off, 1000);
            }else if (data.indexOf("Mv") != -1){
                led.on()
                setTimeout(led_off, 500);
            }
        });
    });
});


//send random moves
/*
var interval = 100;
setTimeout(send_move, interval);
function send_move() {
    var x = Math.random() * 4;
    if (x < 1){
      socket.emit('move', "Up");
    }else if (x < 2){
      socket.emit('move', "Right");
    }else if (x < 3){
      socket.emit('move', "Down");
    }else{
      socket.emit('move', "Left");
    }
    setTimeout(send_move, interval);
}
*/
