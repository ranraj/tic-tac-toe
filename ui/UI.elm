module UI exposing (..)
import Api exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Window exposing (Size)
import AnimationFrame
import Style
import Time exposing (Time, second)
import AppStyle exposing (..)

px v = (toString v) ++ "px" 

type Orientation = Portrait | Landscape
screenOrientation : Size -> Orientation
screenOrientation size = if size.width < size.height  then Portrait else Landscape

windowSize : Size -> Int
windowSize size = case (screenOrientation size) of        
        Portrait -> (size.width) - 33
        Landscape -> (size.height) - 33

tileSize : Size -> Int
tileSize size = case (screenOrientation size) of        
        Portrait -> (size.width // 3) - 11
        Landscape -> (size.height // 3) - 11

boxAlign size= case (screenOrientation size) of 
    Portrait -> ("float","none")
    Landscape -> ("float","left")

menuAnimation model flag = if(flag) then
           ( { model
                | style =
                    Style.animate
                        |> Style.duration (second / 5)
                        |> Style.easing (\x -> x)
                        |> Style.to menuStyles.closed
                        |> Style.on model.style
                  ,menu = not flag    
              }
            , Cmd.none
            )                           
          else
            ( { model
                | style =
                    Style.animate
                        |> Style.duration (second / 5)
                        |> Style.easing (\x -> x)
                        |> Style.to menuStyles.open
                        |> Style.on model.style
                  ,menu = not flag
              }
            , Cmd.none)

cellAnimation : Time -> List Cell -> List Cell
cellAnimation time cells = List.map (cellMap time) cells
cellMap time cell = {cell | animation = Style.tick time cell.animation}

