module Api exposing (..)

import AnimationFrame
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import String
import Array
import WebSocket
import Http
import Task
import Json.Encode
import Json.Decode exposing (Decoder,tuple2,decodeString, int, string, object1,object2,object4, (:=),array,at)
import Window exposing (Size)

-- Custom modules

import AppConfig
import CommonTypes exposing (..)
import JsonMapper exposing (..)
import RenderHelper exposing (..)
import AnimationHelper exposing (..)

type BoardUpdateResponse = Success Cells | Failure String
type Status = LastMove String | Default String | NotValidMove String | GameResult String | Error String

{-
  playGameLocal function is the core function in TicTacToe Api.
  This function being called in the local and remote play.
  It validates the Invalid move and fetching game result in every play.  
-}

playGameLocal position model = 
  let
    plotPlayerCellOnBoard player position cells =  
      case (getPlayerAtPosition position cells) of    
        NoPlayer ->         
          Success (List.map 
                     (\ cell -> 
                        case (position == cell.position) of 
                          True -> 
                            { cell |
                             player = player 
                             ,animation = AnimationHelper.applyTileAnimationProperties cell.animation 
                            }
                          False -> cell) 
                     cells)    
        _ -> Failure "Invalid Move"

    updateBoardModel cells = 
      case (plotPlayerCellOnBoard model.nextPlayer position cells) of
          Success b -> 
            {
                model |
                lastMove = position
                ,nextPlayer = 
                  if model.nextPlayer == PlayerO 
                    then PlayerX 
                    else PlayerO
                ,board = fetchGameResult b
            }  
          Failure msg -> { model  | board = ErrorBoard cells msg}          
  in
    case (model.board) of 
        PlayBoard cells->  updateBoardModel cells ! []
        EmptyBoard cells msg-> updateBoardModel cells ! []
        ErrorBoard cells msg -> updateBoardModel cells ! []
        TieBoard cells msg ->  
          { model  |
           status = NotValidMove ("Game Over as match result :" ++ msg) } ! []
        WinBoard cells msg sequence ->  
          { model  |
           status = NotValidMove ("Game Over as match result :" ++ msg) } ! []

{-|
  playGameRemote plays the online game.
    It validates and update the Board in model
-}

playGameRemote  position model= 
  if model.isConnected then 
    if ( Array.length model.players <= 1 ) 
      then
      { model | board = ErrorBoard defaultCells "Waiting for other to join" } ! []
    else 
      if ( model.currentPlayer == model.nextPlayer ) 
        then
          model ! [ sendMessage model.playerName model.gameCode (lastMoveToMessage position) ]
        else   
          { model | board = EmptyBoard (getCells model) "Waiting for other player to play"} ! []
  else   
    { model | board = ErrorBoard defaultCells "You have not joined the game"} ! []

{-| getPlayerAtPosition returns Player type.
  It find the cell with position and returns the Player from the cell.
-}

getPlayerAtPosition : Position -> Cells -> Player
getPlayerAtPosition position cells = 
  let 
    cell = List.head (List.filter (\currentCell -> currentCell.position == position) cells)
  in     
    case cell of
      Just val -> val.player
      Nothing -> NoPlayer

{-| fetchGameResult takes cells as input paramater and returns Board type as Game Result.
  Boards has added with message about the result, that can be used to render status in UI.
-}

fetchGameResult : Cells -> Board
fetchGameResult cells = 
  let
    isFinishedBoard cells = List.isEmpty (List.filter (\x -> x.player == NoPlayer) cells)
    whoWin : Player -> Cells -> List (List Position) -> Maybe (List Position)
    whoWin player cells winnerCells = 
      let    
        isSubsetOf mainl subl = 
          case subl of
            [] -> True
            (x::xs) -> case (List.member x mainl) of
               True -> isSubsetOf mainl xs
               False -> False 

        filterCellsByPlayer : Player -> Cells -> List Position
        filterCellsByPlayer player cells = 
          List.filter (\x -> x.player == player) cells |> List.map (\x -> x.position)
      in
        case winnerCells of
          [] -> Nothing
          (x::xs) -> case (isSubsetOf (filterCellsByPlayer player cells) x) of
            True -> Just x
            False -> whoWin player cells xs  
  in
    case (whoWin PlayerO cells winCellSequence) of
      Just sequence -> WinBoard cells ("Player O Wins the Game") sequence
      Nothing -> case (whoWin PlayerX cells winCellSequence) of
        Just sequence -> WinBoard cells ("Player X Wins the Game") sequence
        Nothing -> case (isFinishedBoard cells) of
          True -> TieBoard cells "Game Tie"
          False -> PlayBoard cells      
{-|
  fetchStatusFromBoard parse Game board to relevent Status type.
    This Stauts can be used to render status as message
  toHtmlMessage parse Status to Html Message
  renderStatus compose and send the HtmlMessage as return value.  
-}

renderStatus : Board -> Position -> (List (String,String),String)
renderStatus  board position =
  let
    fetchStatusFromBoard : Status
    fetchStatusFromBoard =   
      case board of
        TieBoard cells msg -> GameResult msg
        WinBoard cells msg sequence-> GameResult msg
        PlayBoard cells -> LastMove (toString position)
        EmptyBoard cells msg -> Default msg
        ErrorBoard cells msg -> NotValidMove (toString msg)

    toHtmlMessage : Status -> (List (String,String),String)
    toHtmlMessage status = case status of
        Default msg-> ([("color","#f57f17")],msg)
        LastMove msg -> ([("color","#2bbbad")],"LastMove - " ++ msg)
        NotValidMove msg -> ([("color","#e57373")],"Exception - " ++ msg)
        GameResult msg -> ([("color","#2bbbad")],"Result - " ++ msg)
        Error msg -> ([("color","#e57373")],"Error - " ++ msg)
  in
    fetchStatusFromBoard |> toHtmlMessage

{-|
  getWinnerBoardSequence return List of Position
  This funtion used to highlight winner sequence tiles
-}

getWinnerBoardSequence : Board -> List Position
getWinnerBoardSequence board =  
  case board of
      WinBoard b msg winSeq -> winSeq
      _ -> [] 

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

-- Subscriptions

getSubscribtions model receiveMessageAction reSizeAction animateAction = 
  let
    defaultSubscriptions = [ Window.resizes reSizeAction 
      , AnimationFrame.times animateAction 
    ]
    socketSubscriptions = 
      WebSocket.listen (AppConfig.wSocketApiUrl model.playerName model.gameCode) receiveMessageAction :: defaultSubscriptions
  in
    if (model.isConnected) then (socketSubscriptions) else (defaultSubscriptions)  

-- Http and WebSocket Service utils 

{-|
joinGame set the gameCode to the model and make conntcted status true.
Once the connection status is set then immediately Socket Subscriptions will start listen.
It validates the name, gameCode before set the player to join
-}

joinGame model code board =     
   if (String.isEmpty model.playerName) 
     then 
      { model | 
        board = ErrorBoard defaultCells "Name Must not be Empty" 
      } 
     else 
      if (String.isEmpty model.gameCode) 
        then 
          { model |
            board = ErrorBoard defaultCells "Code is mandatory to join" 
          }
        else
          { model | 
            isConnected = True, board = board , gameCode = code 
          }
{-|
  createGameCode call http api and get the game code
  API uri : http://{host}:{port}/game/code/request/{player-name}  
-}

createGameCode model a b =
  let  
    url = AppConfig.gameCodeApiUrl model.playerName
  in 
    if (String.isEmpty model.playerName) then 
      { model | board = ErrorBoard defaultCells "Name Must not be Empty"} ![]
    else 
      model ! [Task.perform a b (Http.get decodeGameCode url)]

{-| sendMessage api involves in the Remote play model
  It publish the current play position to other player.
-}

sendMessage playerName gameCode msg = WebSocket.send (AppConfig.wSocketApiUrl playerName gameCode) msg   

{-| playModeToMessage takes GameMode and connection status and it convert to render as String
-}

playModeToMessage gameMode connectionStatus= 
  case gameMode of 
    SinglePlayer -> "New"  
    MultiPlayer -> case connectionStatus of 
      True -> "New"
      False -> "Join"

{-| lastMoveToMessage takes lastMovePostion as paramter and it convert to render as String
-}

lastMoveToMessage lastMovePostion= ("Position = [" ++ toString (fst lastMovePostion) ++ "," ++ toString (snd lastMovePostion) ++"]")

{-|
  senderAndReceiverEquals check sender and reciver
-}

senderAndReceiverEquals sender receiver = 
  case (sender == receiver) of
    True -> "You"
    False -> sender

{-|
  playerJoiningStatus check sender and reciver and render it for status
-}

playerJoiningStatus players playerName = 
  let    
    renderPlayerName index default players player = 
      let        
        arrayGetOrElse index default players = 
        case (Array.get index players) of
          Just v -> v
          Nothing-> default
        v = (arrayGetOrElse index default players)
  in  
    senderAndReceiverEquals v player
  in
    (renderPlayerName 0 "_" players playerName) 
    ++ " & " 
    ++ (renderPlayerName 1 " (waiting for other to join)" players playerName)

{-|
  getCells takes board as parameter from the model and exctract the cells from it.
-}

getCells { board } =
  case board of 
    PlayBoard cells-> cells
    EmptyBoard cells msg -> cells 
    ErrorBoard cells msg -> cells
    TieBoard cells msg ->  cells
    WinBoard cells msg sequence ->  cells

{-| encodeJson takes model and convert it as JSON
-}

encodeJsonMessage model = encodeJson model <| getCells model

{-| decodeJsonToModel Convert json to Model  
This being used to decode the stored JSON Model 
-}

decodeJsonToModel modelJson defaultModel =  
  let    
    parseJsonModelToModel defaultCells jsonModel =  
      {
      defaultCells | playerName = jsonModel.playerName
      ,gameCode = jsonModel.gameCode
      ,lastMove = jsonModel.lastMove
      ,isConnected = jsonModel.isConnected
      ,playerMode = jsonModel.playerMode  
      ,board = PlayBoard (mapJsonCellToModelCell jsonModel.cells)
      }
    mapJsonCellToModelCell cells = List.map (\a -> Cell a.position a.player initialWidgetStyle) cells  
  in
    case (decodeJson modelJson) of
      Ok jsonModel -> parseJsonModelToModel defaultModel jsonModel
      Err e -> { defaultModel | messages = toString e :: defaultModel.messages}

{-| 
  socketMessageHandler help to manage socket incoming messages
  If message received it undergoes for further process.
    That should be catched in the any of the defined message category.
    1 ) Game Message - Which is only used to share the play position
    2 ) Event Message - It has only Join and Left message. 
-}

socketMessageHandler model message = 
  let      
    playerMessageToTypeParser memberMessage playerName currentPlayer= 
      if (String.contains playerName memberMessage.member.name) 
        then if (String.contains "PlayerO" memberMessage.member.playerSymbol.messageType) 
          then PlayerO 
          else PlayerX      
      else
        currentPlayer

    isOnline v =
     if (String.contains "Joined" v.messageType) then 
      True 
      else
       if (String.contains "Left" v.messageType) 
         then False 
         else False  

    parseGamePlayMessage = decodeString jsonPlayMessageDecoder message      

    handleGameEventMessage = 
      case (decodeString jsonEventMessageDecoder message) of
        Result.Err eventMessageError -> { model | messages = toString eventMessageError :: model.messages} ! []
        Result.Ok eventMessageValue -> 
          if (Array.length eventMessageValue.allMembers <= 1) 
            then 
              { model |
                messages = toString eventMessageValue :: model.messages 
                ,currentPlayer = playerMessageToTypeParser eventMessageValue model.playerName model.currentPlayer 
                ,board = EmptyBoard defaultCells (playerJoiningStatus eventMessageValue.allMembers model.playerName) 
                ,connectionStatus = isOnline eventMessageValue
                ,players = eventMessageValue.allMembers
              } ! []                
            else 
              { model |
               messages = toString eventMessageValue :: model.messages 
               ,currentPlayer = playerMessageToTypeParser eventMessageValue model.playerName model.currentPlayer
               ,board = EmptyBoard defaultCells ((playerJoiningStatus eventMessageValue.allMembers model.playerName) ++"! Ready to play")
               ,connectionStatus = isOnline eventMessageValue
               ,players = eventMessageValue.allMembers
              } ! []    

    decodePostionFromSting position = decodeString (tuple2 (,) Json.Decode.int Json.Decode.int) position

    {- If possition received in message then parse position string to Position type -}          

    handlePositionMessage playMessageValue =         
              case Array.get 1 (Array.fromList (String.split "=" playMessageValue.message)) of
                Just positionString ->  
                  case (decodePostionFromSting positionString) of
                    Result.Err e -> 
                      { model | messages =  toString e :: model.messages } ! []
                    Result.Ok positionValue -> 
                      let
                        newModel = { model | messages = (toString message) :: model.messages } 
                      in
                        playGameLocal positionValue newModel
                Nothing -> { model | messages = toString "Nothing in message" :: model.messages} ! []            
  in 
    case parseGamePlayMessage of
      Result.Err playMessageError -> 
        {- If incoming fails to parse Play message then try to parse 
        -}
        handleGameEventMessage

      Result.Ok playMessageValue ->           
        if ( String.contains "Position" playMessageValue.message ) 
          then  
            handlePositionMessage playMessageValue
          else
            {- If board reset reqested from sender , this block will reset 
            Todo : Reset request should be accepted by the receiver
            -} 
            if (String.contains "Game.Reset" playMessageValue.message) 
            then 
              { model |
               board = 
                EmptyBoard defaultCells ("Game has been rest by " 
                  ++ (senderAndReceiverEquals playMessageValue.sender)  model.playerName)
               ,messages = toString playMessageValue :: model.messages
               } ! []
            else 
              {- If the received message does not catched in any of the category then, just log it in the messages
              -}
              { model | messages = toString playMessageValue :: model.messages} ! []
                     

