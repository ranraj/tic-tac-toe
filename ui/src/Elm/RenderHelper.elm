module RenderHelper exposing (..)

{-| 
  RenderHelper module supports TicTacToe application to manage Responsive UI based on the screen resolution.    
  All these helper methods are linked with Orientation to make Responsive UI.
-}

import Window exposing (Size)

{- Window size identified by Orientation type
  1. Portrait
  2. Landscape  
-}

type Orientation = Portrait | Landscape

{- 
 screenOrientation is an expression which takes Window.Size as input parameter and results the Orientation type
 It computes the result based on the screen width and height to decide Orientation
-}

screenOrientation : Size -> Orientation
screenOrientation size = if size.width < size.height  then Portrait else Landscape

px v = (toString v) ++ "px" 

{-
boardSize function takes Window size as parameter and return TicTacToe board size based on the current Orientation
This values is used to draw square board. Board size is deducted by 15 as board margin value.
-}

boardSize : Size -> Int
boardSize size = case (screenOrientation size) of        
        Portrait -> (size.width) - 15
        Landscape -> (size.height) - 15

{-
Board has 3 X 3 tiles.    
This function gets Window size as parameter and it divides board by 3 and return as result.
-}

tileSize : Size -> Int
tileSize size = (boardSize size) // 3

{-
    boxAlignStyle retun css style value with respect to screenOrientation
-}
boxAlignStyle size = case (screenOrientation size) of 
    Portrait -> "none"
    Landscape -> "left"

