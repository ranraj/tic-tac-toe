module CommonTypes exposing (..)

{-|
  Common Application Types and static data grouped in one place.
  It is across accross all the modules.
-}
import Array
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

{-| Msg is type of Action which identified as the game events
-}

type Msg =  Play Position  | Reset 
  | Resize Size  | Animate Time 
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
  ,menuStyle : Style.Animation
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
    animation : Style.Animation     
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