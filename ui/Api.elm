module Api exposing (..)
import AnimationFrame
import Style
import AppStyle exposing (..)
import Style.Properties exposing (..)
import Style.Spring.Presets
import String
import Task
import Http
import Json.Decode exposing (Decoder,tuple2,decodeString, int, string, object1,object2,object4, (:=),array,at)
import Array
import WebSocket
import AppConfig exposing (..)
import AnimationFrame
import Window exposing (Size)

type alias Position = (Int,Int)
type Board = EmptyBoard Cells String | PlayBoard Cells | WinBoard Cells String (List Position) | TieBoard Cells String | ErrorBoard Cells String
type Player = PlayerO | PlayerX | NoPlayer
type BoardUpdateResponse = Success (List Cell)| Failure String
type Status = LastMove String | Default String | NotValidMove String | GameResult String | Error String
type PlayerMode = SinglePlayer | MultiPlayer

type alias Cells = List Cell
type alias Cell = {
    position : Position,    
    player : Player,
    animation : Style.Animation     
}

playGame position model = case (model.board) of 
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
fetchGameResult cells = case (whoWin PlayerO cells winCellSequence) of
    Just sequence -> WinBoard cells ("Player O Wins the Game") sequence
    Nothing -> case (whoWin PlayerX cells winCellSequence) of
      Just sequence -> WinBoard cells ("Player X Wins the Game") sequence
      Nothing -> case (isFinishedBoard cells) of
        True -> TieBoard cells "Game Tie"
        False -> PlayBoard cells      

filterCellsByPlayer : Player -> List Cell -> List (Int,Int)
filterCellsByPlayer player cells = List.filter (\x -> x.player == player) cells |> List.map (\x -> x.position)

whoWin : Player -> List Cell -> List (List Position) -> Maybe (List Position)
whoWin player cells l = case l of
  [] -> Nothing
  (x::xs) -> case (isSubsetOf (filterCellsByPlayer player cells) x) of
    True -> Just x
    False -> whoWin player cells xs        

isFinishedBoard : List Cell -> Bool
isFinishedBoard cells = List.isEmpty (List.filter (\x -> x.player == NoPlayer) cells)


updateBoardModel cells model position = 
  let          
    updatedModel = case (plotPlayerCellOnBoard model.nextPlayer position cells) of
      Success b -> 
        {
            model |
            lastMove = position
            ,nextPlayer = if model.nextPlayer == PlayerO then PlayerX else PlayerO
            ,board = fetchGameResult b
        }  
      Failure msg -> { model  | board = ErrorBoard cells msg}
  in
    updatedModel

plotPlayerCellOnBoard : Player -> Position -> List Cell -> BoardUpdateResponse
plotPlayerCellOnBoard player position cells =  
  case (hasOccupied position cells) of    
    NoPlayer ->         
        Success (List.map 
                 (\ cell -> case (position == cell.position) of 
                    True -> {cell | player = player , animation = loadAnimation Spring cell.animation}
                    False -> cell) 
                 cells)    
    _ -> Failure "Invalid Move"



buildStatusFromResult : Board -> Position -> Status
buildStatusFromResult board position =   
  case board of
    TieBoard b msg -> GameResult msg
    WinBoard b msg sequence-> GameResult msg
    PlayBoard b -> LastMove (toString position)
    EmptyBoard b msg -> Default msg
    ErrorBoard b msg -> NotValidMove (toString msg)

getWinnerBoardSequence board =  
  case board of
      WinBoard b msg winSeq -> winSeq
      _ -> [] 


-- Util --
isSubsetOf : List a -> List a -> Bool
isSubsetOf mainl subl = 
  case subl of
    [] -> True
    (x::xs) -> case (List.member x mainl) of
       True -> isSubsetOf mainl xs
       False -> False 

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
socketSubscriptions model a b c= WebSocket.listen (wSocketLink (fst model.playerName) model.gameCode) a :: defaultSubscriptions model b c  

defaultSubscriptions model a b = [ Window.resizes a  
  , AnimationFrame.times b  
   ] 

-- Json Message parsers
type Message = ChatMessage JsonMessageContent | EventMessage JsonMessageContent
type alias Member = {
  name : String
  ,playerSymbol : PlaySymbol
}
type alias MessageMember =
  {
  messageType : String 
  ,member : Member
  ,gameId : String
  , allMembers : Array.Array String
  }
type alias JsonMessageContent = {
  messageType : String
  ,sender : String
  ,gameId : String
  ,message : String
}

type alias PlaySymbol = {
  messageType : String  
}

playSymbolDecoder : Decoder PlaySymbol
playSymbolDecoder =
    object1 PlaySymbol ("$type" := Json.Decode.string)


jsonMessageMemberDecoder1 : Decoder Member
jsonMessageMemberDecoder1 =
    object2 Member ("name" := Json.Decode.string) ("playerSymbol" := playSymbolDecoder)

jsonMessageMemberDecoder : Decoder MessageMember
jsonMessageMemberDecoder =
    object4 MessageMember ("$type" := Json.Decode.string) ("member" := jsonMessageMemberDecoder1) ("gameId" := Json.Decode.string) ("allMembers" := array Json.Decode.string)

jsonMessageContentDecorder : Decoder JsonMessageContent
jsonMessageContentDecorder = object4 JsonMessageContent ("$type" := Json.Decode.string) ("sender" := Json.Decode.string) ("gameId" := Json.Decode.string) ("message" := Json.Decode.string)   

-- Http and WebSocket Service utils 
joinGame model code b = 
   if (String.isEmpty (fst model.playerName)) then {model | board = ErrorBoard defaultCells "Name Must not be Empty"} 
  else if (String.isEmpty model.gameCode) then {model | board = ErrorBoard defaultCells "Code is mandatory to join"}
  else
  { model |isConnected = True, board = b , gameCode = code }

getRandomGameCode model a b =
  let
    name = fst model.playerName
    url = "http://" ++ serverAddress ++ "/game/code/request?name=" ++ name
  in 
  if (String.isEmpty name) then ({model | board = ErrorBoard defaultCells "Name Must not be Empty"},Cmd.none) 
  else 
    (model,Task.perform a b (Http.get decodeGameCode url))

decodeGameCode =
   at ["code"] Json.Decode.string

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