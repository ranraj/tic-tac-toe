module Api exposing (..)
import AnimationFrame
import Style
import MyStyle exposing (..)
import Style.Properties exposing (..)
import Style.Spring.Presets


type alias Position = (Int,Int)
type Board = EmptyBoard Cells | PlayBoard Cells | WinBoard Cells String (List Position) | TieBoard Cells String | ErrorBoard Cells String
type Player = PlayerO | PlayerX | NoPlayer
type BoardUpdateResponse = Success (List Cell)| Failure String
type Status = LastMove String | Default | NotValidMove String | GameResult String | Error String

type alias Cells = List Cell
type alias Cell = {
    position : Position,    
    player : Player,
    animation : Style.Animation     
}

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
      Failure msg -> {
        model  | board = ErrorBoard cells msg}
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
    EmptyBoard b-> Default
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
