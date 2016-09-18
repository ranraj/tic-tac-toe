# Tic-tac-toe
Single Page [TicTacToe!](https://en.wikipedia.org/wiki/Tic-tac-toe) Game App. 

## Overview
This project uses functional reactive programing (Elm-lang) and websocket (Scala with Akka) to create a two-player TicTacToe game. The game was developed to try Elm-lang in light weight gaming app.

####Features:
* Multiplayer TicTacToe
 * Local 
 * Remote play option

## Tech stack:
* Elm-lang 
    There are many elm packages that has been used to make this project.
      * core
      * html
      * svg
      * elm-style-animation
      * http
      * websocket
    There is local storage involved to save the game settings.
      * elm port

* Scala

## Build Instructions
This project is consist of service and ui modules.
  Serice project is scala module with SBT build.
         use 
         ```
         sbt run
         ```
  UI project is Elm module
        use
        ```
        elm-make Todo.elm --output elm.js
        ```
        run  
        ```
        index.html
        ```
  
## Other reference links
  Elm - [Installation guidence](http://elm-lang.org/install)
