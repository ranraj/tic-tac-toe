port module TicTacToe exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (class,style,checked,readonly,placeholder)
import Svg exposing (Svg,g,rect,circle,line)
import Svg.Attributes exposing (..)
import Window exposing (Size)
import Task
import Json.Decode exposing (Decoder,tuple2,decodeString, int, string, object2, (:=),array,at)
import Http
import Time exposing (Time, second)
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import Array
import String
import Json.Encode
import Json.Decode exposing ((:=))

-- Custom Module
import Api exposing (..)
import RenderHelper exposing (..)
import CommonTypes exposing (..)
import AnimationHelper exposing (..)

type Msg = Played Position  | Reset | PlayerModeToggle | CreateGameEvent | JoinApply
  | Resize Size  | Animate Time 
  | FetchSucceed String | FetchFail Http.Error | SendMessage  | ReceiveMessage String  | ConnectOrDisconnect   
  | InputMessage String  | InputPlayerName String | InputGameCode String    
  | HideComponent (String,Bool) |  ShowOrHide Bool

type alias Model = {        
   status : Status 
  ,nextPlayer : Player
  ,board : Board  
  ,lastMove : Position    
  ,screenSize : Size  
  ,playerMode : PlayerMode
  ,connectionStatus : Bool
  ,startRequest : Bool
  ,isCreateMode : Bool
  ,gameCode : String  
  ,inputMessage : String
  ,playerName : (String, Bool)
  ,messages : List String
  ,displayMessages : List String
  ,isConnected : Bool
  ,menuStyle : Style.Animation
  ,menuFlag : Bool
  ,players : Array.Array String
  ,playLock : Bool
  ,currentPlayer : Player
  }

main = App.programWithFlags{ init=init,view = view ,update = updateWithStorage,subscriptions= subscriptions }

-- Update
update : Msg -> Model -> (Model,Cmd Msg)
update msg model =  case msg of
  Played position->  case model.playerMode of
    SinglePlayer -> playGame position model
    MultiPlayer -> playMultiPlayerGame position model

  Resize size -> ({model | screenSize = size},Cmd.none)

  Reset -> if model.isConnected then (model,sendMessage model.playerName model.gameCode "Game.Reset") else ({model | board = EmptyBoard defaultCells "New Game"},sizeTask)

  Animate time ->                 
      case (model.board) of 
        PlayBoard cells-> ({model | board = PlayBoard (executeTileAnimation time cells), menuStyle = Style.tick time model.menuStyle},Cmd.none)
        EmptyBoard cells msg -> ({model | menuStyle = Style.tick time model.menuStyle},Cmd.none)
        ErrorBoard cells msg -> ({model | board = ErrorBoard (executeTileAnimation time cells) msg,menuStyle = Style.tick time model.menuStyle},Cmd.none)
        TieBoard cells msg ->  ({ model  | board = TieBoard (executeTileAnimation time cells ) msg ,status = NotValidMove ("Game Over as match result :" ++ msg),menuStyle = Style.tick time model.menuStyle},Cmd.none)         
        WinBoard cells msg sequence ->  ({ model  | board = WinBoard (executeTileAnimation time cells ) msg sequence,status = NotValidMove ("Game Over as match result :" ++ msg),menuStyle = Style.tick time model.menuStyle},Cmd.none)

  PlayerModeToggle -> ({model | playerMode = if model.playerMode == SinglePlayer then MultiPlayer else SinglePlayer},Cmd.none)        

  FetchSucceed newCode ->
      ({model | gameCode = newCode}, Cmd.none)

  FetchFail _ -> ({model | board = ErrorBoard defaultCells "Something went wrong.!"}, Cmd.none)

  CreateGameEvent -> getRandomGameCode model FetchFail FetchSucceed

  ConnectOrDisconnect -> ({model | isConnected = (not model.isConnected)}, Cmd.none)

  InputMessage newMessage ->
      ({ model | inputMessage = newMessage}, Cmd.none)

  InputPlayerName newName ->     
      ({ model | playerName = (newName, snd model.playerName)}, Cmd.none)  

  InputGameCode code ->     
      ({ model | gameCode = code}, Cmd.none)  

  SendMessage -> 
      ( {model | inputMessage = ""},if model.isConnected then sendMessage model.playerName model.gameCode model.inputMessage else Cmd.none)

  ReceiveMessage str -> socketMessageHandler model str

  HideComponent a -> ({ model | playerName = (fst model.playerName, not (snd model.playerName))}, Cmd.none)  

  JoinApply -> (joinGame model model.gameCode (EmptyBoard defaultCells "New Game"),Cmd.none)

  ShowOrHide flag -> menuAnimation model flag

-- View
view model =    
    div [Html.Attributes.style []] [
     boardBox model     
     ,div
            [Html.Attributes.class "menu-box",  Html.Attributes.style (Style.render model.menuStyle)               
            ]
            [ button [Html.Attributes.class "btn-red",onClick (ShowOrHide model.menuFlag)] [text "Close"]
              ,h1 [] [ text "Tic-Tac-Toe" ]
              ,gameSettingsBox model
            ]
        
     , div [] [div [] [
       button [Html.Attributes.class "btn-menu",onClick (ShowOrHide model.menuFlag)] [text "Menu"]],
       toolBox model
     ]     
    ] 

-- Init
init : Json.Encode.Value -> ( Model, Cmd Msg )
init savedModel =  mapStorageInput savedModel model  ! [sizeTask]

port setStorage : Json.Encode.Value -> Cmd msg

port focus : String -> Cmd msg

model = Model (Default "New Game") PlayerO (EmptyBoard defaultCells "New Game") (0,0) (Size 0 0) SinglePlayer False False True "" "" ("",False) [] [] False (Style.init menuStyles.closed) False Array.empty False NoPlayer

updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ encodeJsonMessage newModel |> setStorage , cmds ]
    )
 
-- Task
sizeTask = Task.perform Resize Resize Window.size

-- Sub
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch (getSubscribtions model ReceiveMessage Resize Animate)
   

boardBox {board,screenSize} =       
  div [Html.Attributes.style [("float",boxAlignStyle screenSize)]] [          
      Svg.svg [Svg.Attributes.class "board-box", version "1.1", x "0", y "0", viewBox "0 0" 
        ,width (px (boardSize screenSize)),
        height (px (boardSize screenSize))
      ] (drawBoard board screenSize)            
  ]

toolBox {board,lastMove,playerMode,connectionStatus,startRequest,gameCode} = 
  let               
    gameStatus = statusToMessage (fetchStatus board lastMove)        
    
  in
    div [Html.Attributes.class "tool-box"][
         button [Html.Attributes.class "btn-green",onClick Reset] [text (playMode playerMode connectionStatus startRequest)]  
         ,div [Html.Attributes.style (fst gameStatus) ] [         
           span [Html.Attributes.class "notification"] [text (snd gameStatus)]
         ]                  
    ]   

gameSettingsBox {board,lastMove,playerMode,playerName,connectionStatus,startRequest,gameCode,isConnected,messages,players,currentPlayer,menuFlag} = 
  let     
    multiPlayerToolBoxVisible = if (playerMode == MultiPlayer) then "block" else "none"
  in 
  div [Html.Attributes.class "setting-box"] [
    div [Html.Attributes.style [("padding","5px")]] [  
          span [] [text " 2-Player "]
          ,button [Html.Attributes.class "btn-blue",onClick PlayerModeToggle] [text (if playerMode == SinglePlayer then "Standalone" else "Remote")]          
         ]         
         ,div [Html.Attributes.style [("display",multiPlayerToolBoxVisible)]] [
          div[][
            --span [] [text "Player Name "]
           br [] []
           ,input [ placeholder "Player Name",Html.Attributes.class "text-normal",Html.Attributes.style [("background", if (snd playerName) then "#dddddd" else "#ffffff")],onInput InputPlayerName,Html.Attributes.value (fst playerName)] []           
           ,button [Html.Attributes.class "btn-blue",onClick (HideComponent playerName) ] [text (if (snd playerName) then "Edit " else "Save")]
          ]
          ,div [] [
            br [] []
            ,div[] [
              button [Html.Attributes.class "btn-blue",onClick CreateGameEvent] [text "Create"]                                    
            ]
            ,div[][                        
              div [][
                input [placeholder "Code",Html.Attributes.class "text-normal",onInput InputGameCode,Html.Attributes.value gameCode] []
                ,button [Html.Attributes.class "btn-blue",onClick JoinApply] [text "Join"]
              ] 
            ]
          ] 
          ,div [Html.Attributes.style [("display",if (Array.length players > 0) then "block" else "none")]] [         
         span [] [text "Players : " ]         
         ,span [] [text (playerJoiningStatus players playerName) ]
         ,br[][]
         ,span [][text ("Symbol : " ++ case currentPlayer of
                          NoPlayer -> "_"
                          PlayerX -> "X"
                          PlayerO -> "O") ]
         ]
          ,div [][
            label []
              [ br [] []              
              , button [Html.Attributes.class ("connection-status " ++ (if isConnected then "connection-online" else "connection-offline")),onClick ConnectOrDisconnect] [text (if isConnected then "Online" else "Offline")]
              ]
            ]
         ]
         --UI Game Message logger
         ,div []
          [                     
          input [Html.Attributes.class "text-normal",onInput InputMessage] []
          , button [Html.Attributes.class "btn-blue",onClick SendMessage] [text "Send"]
         -- ,div [] (List.map (\msg -> div [] [ text msg ]) messages)-}         
          ]
  ]

socketMessageHandler model str = 
  let       
    newModel = case (parseGameMessage str)  of
      Result.Err a ->
        -- Joined / Left - Message Request
        case (parseEventMessage str) of
          Result.Err e -> ({ model | messages = toString e :: model.messages},Cmd.none)
          Result.Ok v -> 
            if (Array.length v.allMembers <= 1) then 
              ({ model | messages = toString v :: model.messages 
                , currentPlayer = playerParser v model.playerName model.currentPlayer 
                , board = EmptyBoard defaultCells (playerJoiningStatus v.allMembers model.playerName) 
                , connectionStatus = isOnline v
                , players = updatePlayers v
                }
              ,Cmd.none) 
            else 
              ({ model | messages = toString v :: model.messages ,currentPlayer = playerParser v model.playerName model.currentPlayer, board = EmptyBoard defaultCells ((playerJoiningStatus v.allMembers model.playerName) ++"! Ready to play"), connectionStatus = isOnline v,players = updatePlayers v},Cmd.none)
      Result.Ok r -> 
        -- CHAT MESSAGE REQUEST
        let 
          positionMessage = r.message
          remotePosition = 
            if (isPositionShared positionMessage) 
            then  
              -- CASE FOR POSITION SHARE
              case Array.get 1 (Array.fromList (String.split "=" positionMessage)) of
                Just position ->  case (decodeString (tuple2 (,) Json.Decode.int Json.Decode.int) position) of
                    Result.Err e -> ({model | messages =  toString e :: model.messages },Cmd.none)
                    Result.Ok v -> 
                      let
                        b = {model | messages = (toString str) :: model.messages } 
                      in
                        playGame v b
                Nothing -> ({ model | messages = toString "Nothing received" :: model.messages},Cmd.none)
            else if (String.contains "Game.Reset" positionMessage) then
              ({ model | board = EmptyBoard defaultCells ("Game has been rest by " ++ (isYou r.sender) (fst model.playerName)),messages = toString r:: model.messages},Cmd.none)
            else 
              ({ model | messages = toString r:: model.messages},Cmd.none)
        in
          remotePosition
  in 
    newModel
    
    
drawBoard : Board -> Window.Size -> List (Svg Msg)
drawBoard board screenSize = case board of  
  EmptyBoard b msg-> List.map (mapCellToTile board screenSize) b 
  PlayBoard b -> List.map (mapCellToTile board screenSize) b 
  WinBoard b msg winSequence -> List.map (mapCellToTile board screenSize) b 
  TieBoard b msg -> List.map (mapCellToTile board screenSize) b  
  ErrorBoard b msg ->  List.map (mapCellToTile board screenSize) b 

mapCellToTile board screenSize {position,animation,player} = 
  let
    h = screenSize.height
    w = screenSize.width             
    size = tileSize screenSize
    times = size // 100
    xVal = ((fst position) * size)
    yVal = ((snd position) * size)   
    circleSize  = (size // 2)
    crossSize = (size // 4)
    symbolWidth = (size // 6)
    boardBgColor =  case (List.member position (getWinnerBoardSequence board)) of
      True -> "#2bbbad"
      False -> "#9F9F9F"
    symbolColor = "#ffffff"  
  in
  case player of
    PlayerO ->        
      g [] [
      rect [Svg.Attributes.class "cell",Html.Attributes.style (Style.render animation),onClick (Played (position)), fill boardBgColor, stroke symbolColor, x (toString xVal), y (toString yVal), width (toString size), height (toString size)][]                      
      ,circle [Html.Attributes.style (Style.render animation),onClick (Played (position)), fill symbolColor, cx (toString ((xVal + circleSize) + times)), cy (toString ((yVal + circleSize) + times)), r (toString ((symbolWidth * 2) + times)) ] []
      ,circle [Html.Attributes.style (Style.render animation),onClick (Played (position)), fill boardBgColor, cx (toString ((xVal + circleSize) + times)), cy (toString ((yVal + circleSize) + times)), r (toString (symbolWidth + times)) ] []
      ]
    PlayerX -> 
       g [] [
      rect [Svg.Attributes.class "cell",Html.Attributes.style (Style.render animation),onClick (Played (position)), fill boardBgColor, stroke symbolColor, x (toString ((fst position) * size)), y (toString ((snd position) * size)), width (toString size), height (toString size)][]                
      ,line [Html.Attributes.style (Style.render animation),onClick (Played (position)), x1 (toString (xVal + crossSize)), y1 (toString (yVal + crossSize)), x2 (toString (xVal + (size - crossSize))), y2 (toString (yVal + (size - crossSize))), stroke symbolColor,strokeWidth (toString symbolWidth)] []
      ,line [Html.Attributes.style (Style.render animation),onClick (Played (position)), x1 (toString (xVal + crossSize)), y1 (toString (yVal + (size - crossSize ))), x2 (toString (xVal + (size - crossSize))), y2 (toString (yVal + crossSize)), stroke symbolColor,strokeWidth (toString symbolWidth)] []
      ]
    NoPlayer ->
      rect [Svg.Attributes.class "cell",Html.Attributes.style (Style.render animation),onClick (Played (position)), fill symbolColor, stroke boardBgColor, x (toString ((fst position) * size)), y (toString ((snd position) * size)), width (toString size), height (toString size)][]   

statusToMessage : Status -> (List (String,String),String)
statusToMessage status = case status of
    Default msg-> ([("color","#f57f17")],msg)
    LastMove msg -> ([("color","#2bbbad")],"LastMove - " ++ msg)
    NotValidMove msg -> ([("color","#e57373")],"Exception - " ++ msg)
    GameResult msg -> ([("color","#2bbbad")],"Result - " ++ msg)
    Error msg -> ([("color","#e57373")],"Error - " ++ msg)

