# Tic-tac-toe
Single Page [TicTacToe!](https://en.wikipedia.org/wiki/Tic-tac-toe) Game App. 

## Overview
This project uses functional reactive programing (Elm-lang) and websocket (Scala with Akka) to create a two-player TicTacToe game. The game was developed to try Elm-lang in light weight gaming app.

####Features:
* Multiplayer TicTacToe
 * Local play
 * Remote play

## Tech stack:
* Elm-lang 

    There are many elm packages that has been used to make this project.
      * core
      * html
      * svg
      * elm-style-animation
      * http
      * websocket
      
    There is js local storage api involved to save the game settings.
      * elm port

* Scala

    This module serve APIs to create game and exchange the game play information.
     *  Akka framework 
    
## Api details

* Create game 
  Url - ```http://{host}:{port}/game/code/request/<player-name> ```

  Type : Http Api
  
  Method - Get
  
  Parameters - [ player-name : String ]
  
  
  Response - ```{ gameCode : <game-code> }```

* Remote Play 
  Url - ```ws://" {host}:{port}/game/join?name=<player-name>&id=<game-code>```

  Type - Websocket Api
  
  Paramter - [player-name : String , game-code : String]
  
  Response - 
    Response would push event from the sever. Socket client has to listen to get message from server.
    
    * Messages are simply two types
        * Even Message - Join / Left 
        * Pay Message - Play event Possition
        
## Build Instructions
This project is consist of service and ui modules.

**Service** project is a scala module with SBT build.
  
Run the following command from the root of service module:
         
```sbt run```     
         
**UI** project is Elm module
  
Run the following command from the root of ui module:
        
```elm-make Elm/Tictactoe.elm --output Web/tictactoe.js ```
        
Open the index.html form the ui/Web folder

Please find more details on this section from the respective modules.
  
## Other reference links
  Elm - [Installation guidence](http://elm-lang.org/install)
