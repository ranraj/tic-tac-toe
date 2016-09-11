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
import AppConfig exposing (serverAddress)
import CommonTypes exposing (..)
import JsonMapper exposing (..)
import RenderHelper exposing (..)
import AnimationHelper exposing (..)

type BoardUpdateResponse = Success (List Cell)| Failure String
type Status = LastMove String | Default String | NotValidMove String | GameResult String | Error String

{-
  playGame function is the core function in TicTacToe Api.
  This function being called in the local and remote play.
  It validates the Invalid move and fetching game result in every play.  
-}

playGame position model = 
  let
    plotPlayerCellOnBoard player position cells =  
      case (hasOccupied position cells) of    
        NoPlayer ->         
            Success (List.map 
                     (\ cell -> case (position == cell.position) of 
                        True -> {cell | player = player , animation = applyTileAnimationProperties cell.animation}
                        False -> cell) 
                     cells)    
        _ -> Failure "Invalid Move"

    updateBoardModel cells model position = 
      case (plotPlayerCellOnBoard model.nextPlayer position cells) of
          Success b -> 
            {
                model |
                lastMove = position
                ,nextPlayer = if model.nextPlayer == PlayerO then PlayerX else PlayerO
                ,board = fetchGameResult b
            }  
          Failure msg -> { model  | board = ErrorBoard cells msg}
  in
    case (model.board) of 
        PlayBoard cells->  (updateBoardModel cells model position,Cmd.none)                  
        EmptyBoard cells msg-> (updateBoardModel cells model position,Cmd.none)          
        ErrorBoard cells msg -> (updateBoardModel cells model position,Cmd.none)         
        TieBoard cells msg ->  ({ model  | status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)         
        WinBoard cells msg sequence->  ({ model  | status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)                   

hasOccupied : Position -> List Cell -> Player
hasOccupied position cells = 
  let 
    cell = List.head (List.filter (\currentCell -> currentCell.position == position) cells)
  in     
    case cell of
      Just val -> val.player
      Nothing -> NoPlayer

fetchGameResult : List Cell -> Board
fetchGameResult cells = 
  let
    isFinishedBoard cells = List.isEmpty (List.filter (\x -> x.player == NoPlayer) cells)
  in
    case (whoWin PlayerO cells winCellSequence) of
      Just sequence -> WinBoard cells ("Player O Wins the Game") sequence
      Nothing -> case (whoWin PlayerX cells winCellSequence) of
        Just sequence -> WinBoard cells ("Player X Wins the Game") sequence
        Nothing -> case (isFinishedBoard cells) of
          True -> TieBoard cells "Game Tie"
          False -> PlayBoard cells      


whoWin : Player -> List Cell -> List (List Position) -> Maybe (List Position)
whoWin player cells l = 
  let    
    isSubsetOf mainl subl = 
      case subl of
        [] -> True
        (x::xs) -> case (List.member x mainl) of
           True -> isSubsetOf mainl xs
           False -> False 

    filterCellsByPlayer player cells = 
      List.filter (\x -> x.player == player) cells |> List.map (\x -> x.position)
  in
  case l of
    [] -> Nothing
    (x::xs) -> case (isSubsetOf (filterCellsByPlayer player cells) x) of
      True -> Just x
      False -> whoWin player cells xs        


fetchStatus : Board -> Position -> Status
fetchStatus board position =   
  case board of
    TieBoard b msg -> GameResult msg
    WinBoard b msg sequence-> GameResult msg
    PlayBoard b -> LastMove (toString position)
    EmptyBoard b msg -> Default msg
    ErrorBoard b msg -> NotValidMove (toString msg)

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
    socketSubscriptions= WebSocket.listen (wSocketLink (fst model.playerName) model.gameCode) receiveMessageAction :: defaultSubscriptions
  in
    if (model.isConnected) then (socketSubscriptions) else (defaultSubscriptions)  

-- Json Message parsers

parseEventMessage str = decodeString jsonEventMessageDecoder str
parseGameMessage str = decodeString jsonPlayMessageDecoder str

-- Http and WebSocket Service utils 

joinGame model code board = 
   if (String.isEmpty (fst model.playerName)) then
     {model | board = ErrorBoard defaultCells "Name Must not be Empty"} 
   else if (String.isEmpty model.gameCode) then 
          {model | board = ErrorBoard defaultCells "Code is mandatory to join"}
        else
          { model |isConnected = True, board = board , gameCode = code }

getRandomGameCode model a b =
  let
    name = fst model.playerName
    url = "http://" ++ serverAddress ++ "/game/code/request?name=" ++ name
  in 
    if (String.isEmpty name) then 
      ({model | board = ErrorBoard defaultCells "Name Must not be Empty"},Cmd.none) 
    else 
      (model,Task.perform a b (Http.get decodeGameCode url))

playMultiPlayerGame  position model= 
  if model.isConnected then 
    if (Array.length model.players <= 1) then
      ({model | board = ErrorBoard defaultCells "Waiting for other to join"},Cmd.none)
    else  if (model.currentPlayer == model.nextPlayer) then
      (model,sendMessage model.playerName model.gameCode (lastMoveToMessage position))
    else   
      ({model | board = EmptyBoard (getCells model) "Waiting for other player to play"},Cmd.none)
  else   
    ({model | board = ErrorBoard defaultCells "You have not joined the game"},Cmd.none)

sendMessage playerName gameCode msg = WebSocket.send (wSocketLink (fst playerName) gameCode) msg
wSocketLink name code = ("ws://" ++ serverAddress ++ "/game/join?name=" ++ name ++ "&id=" ++ code) 

isOnline v = if (String.contains "Joined" v.messageType) then 
  True 
  else if (String.contains "Left" v.messageType) then False 
  else False

updatePlayers v = v.allMembers

isPositionShared v = (String.contains "Position" v)   

playMode playerMode connectionStatus startRequest= 
  case playerMode of 
    SinglePlayer -> "New"  
    MultiPlayer -> case connectionStatus of 
      True -> if startRequest then "Ok" else "New"
      False -> "Join"

lastMoveToMessage lastMove= ("Position = [" ++ toString (fst lastMove) ++ "," ++ toString (snd lastMove) ++"]")

arrayGetOrElse : Int -> String -> Array.Array String -> String
arrayGetOrElse index default players= 
  case (Array.get index players) of
    Just v -> v
    Nothing-> default

renderPlayerName index default players player= 
  let
    v = (arrayGetOrElse index default players)
  in  
    isYou v player

playerJoiningStatus players playerName = (renderPlayerName 0 "_" players (fst playerName)) ++ " & " ++ (renderPlayerName 1 " (waiting for other to join)" players (fst playerName))

isYou sender receiver = 
  case (sender == receiver) of
    True -> "You"
    False -> sender

getCells model =
  case (model.board) of 
    PlayBoard cells-> cells
    EmptyBoard cells msg -> cells 
    ErrorBoard cells msg -> cells
    TieBoard cells msg ->  cells
    WinBoard cells msg sequence ->  cells

playerParser memberMessage playerName currentPlayer= 
  if (String.contains (fst playerName) memberMessage.member.name) 
    then if (String.contains "PlayerO" memberMessage.member.playerSymbol.messageType) 
      then PlayerO 
      else PlayerX      
  else
    currentPlayer


encodeJsonMessage model = encodeJson model getCells

mapStorageInput modelJson defaultModel =  
  case (decodeJson modelJson) of
    Ok savedModel -> parseStoredToDefaultModel defaultModel savedModel
    Err e -> {defaultModel | messages = toString e :: defaultModel.messages}



parseStoredToDefaultModel defaultCells storedModel =  
  {
  defaultCells | playerName = storedModel.playerName
  ,gameCode = storedModel.gameCode
  ,lastMove = storedModel.lastMove
  ,isConnected = storedModel.isConnected
  ,playerMode = storedModel.playerMode  
  ,board = PlayBoard (mapStoredCellToModel storedModel.cells)
  }

mapStoredCellToModel cells = List.map (\a -> Cell a.position a.player initialWidgetStyle) cells
