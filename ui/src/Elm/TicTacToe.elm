port module TicTacToe exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class,style,checked,readonly,placeholder)
import Svg exposing (Svg,g,rect,circle,line)
import Svg.Attributes exposing (..)
import Window exposing (Size)
import Task
import Json.Decode exposing (Decoder,decodeString, int, string,array,at)
import Http
import Time exposing (Time, second)
import Array
import String
import Json.Encode
import Animation

-- Custom Module
import Api exposing (..)
import RenderHelper exposing (..)
import CommonTypes exposing (..)
import AnimationHelper exposing (..)
import View

{-| Initial Model definition with game default values.
  Find Model type definition from the CommmonTypes.elm
-}

model : Model
model = 
  {
  board = (EmptyBoard CommonTypes.defaultCells "New Game"),
  nextPlayer = PlayerO,
  playerMode = SinglePlayer,
  playerName = "",
  lastMove = (0,0),
  status = (Default "New Game"),
  connectionStatus = False,
  isConnected = False,    
  gameCode = "",
  players = Array.empty,
  currentPlayer = NoPlayer,  
  inputMessage = "",  
  messages = [],  
  menuStyle = (Animation.style menuStyles.closed),
  menuFlag = False,  
  screenSize = (Size 0 0)  
  } 

{-| 
  ProgramWithFlags useful when application state intracting with local storage
-}

main = Html.programWithFlags{ init=init,view = view ,update = updateWithStorage,subscriptions= subscriptions }

-- INIT

init : Json.Encode.Value -> ( Model, Cmd Msg )
init savedModel =  
  Api.decodeJsonToModel savedModel model  ! [sizeTask]

-- UPDATE

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =  
  case msg of
    Play position ->  
      case model.playerMode of
        SinglePlayer -> 
          Api.playGameLocal position model
        MultiPlayer -> 
          Api.playGameRemote position model

    Resize size -> { model | screenSize = size } ! []

    Reset -> if model.isConnected 
              then model ! [ Api.messageSender model.playerName model.gameCode "Game.Reset" ]
              else { model | board = EmptyBoard CommonTypes.defaultCells "New Game" } ! [ sizeTask ]

    Animate animationMsg ->                 
      case (model.board) of 
        PlayBoard cells -> ({ model | 
          board = PlayBoard ( AnimationHelper.executeTileAnimation animationMsg cells)
          ,menuStyle = Animation.update animationMsg model.menuStyle
          },
        Cmd.none)            
        EmptyBoard cells msg -> 
          ({model | 
            menuStyle = Animation.update animationMsg model.menuStyle
            },
          Cmd.none)

        ErrorBoard cells msg -> 
          ({model | 
            board = ErrorBoard ( AnimationHelper.executeTileAnimation animationMsg cells) msg
            ,menuStyle = Animation.update animationMsg model.menuStyle
            },
          Cmd.none)

        TieBoard cells msg ->  
          ({ model  | 
            board = TieBoard ( AnimationHelper.executeTileAnimation animationMsg cells ) msg ,
            status = NotValidMove ("Game Over as match result :" ++ msg)
            ,menuStyle = Animation.update animationMsg model.menuStyle
            },
          Cmd.none)         

        WinBoard cells msg sequence ->  
          ({ model  | 
            board = WinBoard ( AnimationHelper.executeTileAnimation animationMsg cells ) msg sequence,
            status = NotValidMove ("Game Over as match result :" ++ msg)
            ,menuStyle = Animation.update animationMsg model.menuStyle
            },
          Cmd.none)
       
    PlayerModeToggle -> 
      { model |
        playerMode = 
          if model.playerMode == SinglePlayer 
            then MultiPlayer 
            else SinglePlayer
      } ! [] 

    FetchSucceed newCode ->
      { model | gameCode = newCode } ! []

    FetchFail _ -> 
      { model | board = ErrorBoard CommonTypes.defaultCells "Something went wrong.!!" } ! []

    CreateGameEvent -> 
      Api.createGameCode model FetchFail FetchSucceed

    ConnectOrDisconnect -> 
      { model | isConnected = (not model.isConnected) } ! []

    InputMessage newMessage ->
      { model | inputMessage = newMessage } ! []

    InputPlayerName newName ->     
      { model | playerName = newName } ! []

    InputGameCode code ->     
      { model | gameCode = code} ! []

    SendMessage -> 
      { model | inputMessage = "" }
      ! if model.isConnected 
         then [ Api.messageSender model.playerName model.gameCode model.inputMessage ] 
         else []

    ReceiveMessage str -> 
      Api.messageReceiver model str

    JoinApply -> 
      Api.joinGame model model.gameCode (EmptyBoard CommonTypes.defaultCells "New Game") ! []

    MenuAction flag -> 
      AnimationHelper.menuAnimation model flag

-- VIEW
view : Model -> Html Msg
view model = View.baseView model

port setStorage : Json.Encode.Value -> Cmd msg

port focus : String -> Cmd msg

{-| It stores the application status on every update.
-}

updateWithStorage: Msg -> Model -> (Model,Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    (newModel,Cmd.none
    {-, Cmd.batch [ Api.encodeJsonMessage newModel |> setStorage , cmds ]-}
    )
 
-- TASK
sizeTask = Task.perform Resize Window.size

-- SUBSCRIPTION
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch (Api.getSubscribtions model ReceiveMessage Resize)
   


