module UI exposing (boxAlign,windowSize,screenOrientation,tileSize,px)
import Api exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Window exposing (Size)

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
