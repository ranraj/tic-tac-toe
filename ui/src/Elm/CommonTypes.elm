module CommonTypes exposing (..)

{-|
  Common Application Types and static data grouped in one place.
  It is across accross all the modules.
-}

import Style
import Http
import Window exposing (Size)
import Time exposing (Time, second)
import AnimationFrame
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import Color exposing (rgb, rgba,white)
import Time exposing (Time, second)
import Ease

{-| Msg is Action type 
  All the game event handled as Msg
-}

type Board = EmptyBoard Cells String | PlayBoard Cells | WinBoard Cells String (List Position) | TieBoard Cells String | ErrorBoard Cells String

type PlayerMode = SinglePlayer | MultiPlayer

type Player = PlayerO | PlayerX | NoPlayer

type alias Cells = List Cell

type alias Cell = {
    position : Position,    
    player : Player,
    animation : Style.Animation     
}

type alias Position = (Int,Int)

-- Static Data --           
winCellSequence = [
  [(0,0),(0,1),(0,2)]
  ,[(0,0),(1,0),(2,0)]
  ,[(0,0),(1,1),(2,2)]
  ,[(0,1),(1,1),(2,1)]
  ,[(1,0),(1,1),(1,2)]
  ,[(2,0),(2,1),(2,2)]
  ,[(0,2),(1,2),(2,2)]
  ,[(0,2),(1,1),(2,0)]
  ] 

defaultCells = [
   Cell (0,0) NoPlayer initialWidgetStyle
  ,Cell (0,1) NoPlayer initialWidgetStyle
  ,Cell (0,2) NoPlayer initialWidgetStyle
  ,Cell (1,0) NoPlayer initialWidgetStyle
  ,Cell (1,1) NoPlayer initialWidgetStyle
  ,Cell (1,2) NoPlayer initialWidgetStyle
  ,Cell (2,0) NoPlayer initialWidgetStyle
  ,Cell (2,1) NoPlayer initialWidgetStyle
  ,Cell (2,2) NoPlayer initialWidgetStyle 
  ]

{- Board tiles WidgetStyle used in inital board construction -}

initialWidgetStyle =
    Style.init
        [ Display InlineBlock
        , Rotate 0.0 Turn
        , RotateX 0.0 Turn
        , RotateY 0.0 Turn
        , TranslateY 0.0 Px
        , TranslateX 0.0 Px
        , Rotate 0.0 Turn
        , Opacity 1
        , BackgroundColor (rgba 58 40 69 1.0)
        , Color (rgba 255 255 255 1.0)
        , Scale 1.0                
        ]  