module CommonTypes exposing (..)

{-|
  Common Application Types and static data grouped in one place.
  It is across accross all the modules.
-}
import Array
import Animation
import Http
import Window exposing (Size)
import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (px, turn, percent)
import Color exposing (rgb, rgba,white)
import Time exposing (Time, second)
import Ease

{-| Msg is type of Action which identified as the game events
-}

type Msg =  Play Position  | Reset 
  | Resize Size  | Animate Animation.Msg 
  | FetchSucceed String | FetchFail Http.Error | SendMessage  | ReceiveMessage String   
  | InputMessage String  | InputPlayerName String | InputGameCode String    
  | MenuAction Bool | ConnectOrDisconnect | PlayerModeToggle | CreateGameEvent | JoinApply

{-| Model is application status.
  Which holds reference to other types like Board, Player And PlayerMode as properties  
-}
type alias Model = {        
   board : Board
  ,nextPlayer : Player
  ,playerMode : PlayerMode  
  ,playerName : String
  ,lastMove : Position
  ,status : Status               
  ,connectionStatus : Bool    
  ,isConnected : Bool
  ,gameCode : String  
  ,players : Array.Array String  
  ,currentPlayer : Player
  ,inputMessage : String  
  ,messages : List String    
  ,menuStyle : Animation.State
  ,menuFlag : Bool      
  ,screenSize : Size
  }

type Status = LastMove String | Default String | NotValidMove String | GameResult String | Error String

type Board = EmptyBoard Cells String | PlayBoard Cells | WinBoard Cells String (List Position) | TieBoard Cells String | ErrorBoard Cells String

type PlayerMode = SinglePlayer | MultiPlayer

type Player = PlayerO | PlayerX | NoPlayer

type alias Cells = List Cell

type alias Cell = {
    position : Position,    
    player : Player,
    animation : Animation.State     
}

type alias Position = (Int,Int)

-- Static Data --   
winCellSequence : List (List Position)        
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

defaultCells : List Cell
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
    Animation.styleWith
        (Animation.spring
            { stiffness = 400
            , damping = 23 }
        )
        [ Animation.display Animation.inlineBlock
        , Animation.opacity 2
        , Animation.backgroundColor (rgba 58 40 69 1.0)
        , Animation.color (rgba 255 255 255 1.0)
        , Animation.scale 1.0
        , Animation.translate3d (percent 0) (percent 0) (px 0)
        , Animation.shadow
                { offsetX = 50
                , offsetY = 55
                , blur = 6
                , size = 4                
                , color = rgba 0 0 0 0.1
                }
        ]