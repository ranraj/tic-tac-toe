import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Svg exposing (Svg,g,rect,circle,line)
import Svg.Attributes exposing (..)
import Window exposing (Size)
import Task
import AnimationFrame

import Time exposing (Time, second)
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets

-- Custom Module
import MyStyle exposing (..)
import Api exposing (..)
import UI exposing (..)

type Msg = Played Position | Resize Size | Reset | Animate Time

type alias Model = {   
   status : Status 
  ,nextPlayer : Player
  ,board : Board
  ,lastMove : Position    
  ,screenSize : Size  
  }

main = App.program{ init=init,view = view ,update = update,subscriptions= subscriptions }

-- Update
update : Msg -> Model -> (Model,Cmd Msg)
update msg model =  case msg of
  Played position->  
    case (model.board) of 
        PlayBoard cells-> (updateBoardModel cells model position,Cmd.none)
        EmptyBoard cells -> (updateBoardModel cells model position,Cmd.none)
        ErrorBoard cells msg -> (updateBoardModel cells model position,Cmd.none)
        TieBoard cells msg ->  ({ model  | status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)         
        WinBoard cells msg sequence->  ({ model  | status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)                   
  Resize size -> ({model | screenSize = size},Cmd.none)
  Reset -> ({model | board = EmptyBoard defaultCells},sizeTask)
  Animate time -> case (model.board) of 
        PlayBoard cells-> ({model | board = PlayBoard (doAnimation time cells model.lastMove)},Cmd.none)
        EmptyBoard cells -> (model,Cmd.none)
        ErrorBoard cells msg -> ({model | board = ErrorBoard (doAnimation time cells model.lastMove) msg},Cmd.none)
        TieBoard cells msg ->  ({ model  | board = TieBoard (doAnimation time cells model.lastMove) msg ,status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)         
        WinBoard cells msg sequence ->  ({ model  | board = WinBoard (doAnimation time cells model.lastMove) msg sequence,status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)                   

doAnimation : Time -> List Cell -> Position -> List Cell
doAnimation time cells lastMove = List.map (cellMap time) cells
  
cellMap time cell = {cell | animation = Style.tick time cell.animation}


-- View
view model =    
    div [ Html.Attributes.style [margin]] [
     boardBox model
     ,toolBox model
    ] 

-- Init
init : (Model, Cmd Msg)
init = (Model Default PlayerO (EmptyBoard defaultCells) (0,0) (Size 0 0) ,sizeTask )

-- Task
sizeTask = Task.perform Resize Resize Window.size

-- Sub
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch 
  [ Window.resizes Resize  
  , AnimationFrame.times Animate
  ]

boardBox {board,screenSize} =       
  div [Html.Attributes.style [boxAlign screenSize]] [          
      Svg.svg [boardBoxStyle, version "1.1", x "0", y "0", viewBox "0 0" 
        ,width (px (windowSize screenSize)),
        height (px (windowSize screenSize))
      ] (drawBoard board screenSize)            
  ]

toolBox {board,lastMove} = 
  let           
    gameStatus = statusToMessage (buildStatusFromResult board lastMove)        
  in
    div [toolBoxStyle][
         button [buttonStyle,onClick Reset] [Html.text "New"]  
         ,div [Html.Attributes.style (fst gameStatus) ] [         
           span [notificationStyle] [Html.text (snd gameStatus)]
         ]
    ]  

drawBoard : Board -> Window.Size -> List (Svg Msg)
drawBoard board screenSize = case (board) of  
  EmptyBoard b -> List.map (mapCellToTile board screenSize) b 
  PlayBoard b -> List.map (mapCellToTile board screenSize) b 
  WinBoard b msg winSequence -> List.map (mapCellToTile board screenSize) b 
  TieBoard b msg -> List.map (mapCellToTile board screenSize) b  
  ErrorBoard b msg ->  List.map (mapCellToTile board screenSize) b 

mapCellToTile board screenSize cell = 
  let
    {position} = cell
    h = screenSize.height
    w = screenSize.width             
    size = tileSize screenSize
    times = size // 100
    xVal = ((fst cell.position) * size)
    yVal = ((snd cell.position) * size)   
    circleSize  = (size // 2)
    crossSize = (size // 4)
    symbolWidth = (size // 6)
    boardBgColor =  case (List.member position (getWinnerBoardSequence board)) of
      True -> winTileBg
      False -> defaultTileBg          
  in
  case cell.player of
    PlayerO ->        
      g [] [
      rect [Html.Attributes.style (cellStyle ++ (Style.render cell.animation)),onClick (Played (position)), fill boardBgColor, stroke symbolColor, x (toString xVal), y (toString yVal), width (toString size), height (toString size)][]                      
      ,circle [Html.Attributes.style (Style.render cell.animation),onClick (Played (position)), fill symbolColor, cx (toString ((xVal + circleSize) + times)), cy (toString ((yVal + circleSize) + times)), r (toString ((symbolWidth * 2) + times)) ] []
      ,circle [Html.Attributes.style (Style.render cell.animation),onClick (Played (position)), fill boardBgColor, cx (toString ((xVal + circleSize) + times)), cy (toString ((yVal + circleSize) + times)), r (toString (symbolWidth + times)) ] []
      ]
    PlayerX -> 
       g [] [
      rect [Html.Attributes.style (cellStyle ++ (Style.render cell.animation)),onClick (Played (position)), fill boardBgColor, stroke symbolColor, x (toString ((fst cell.position) * size)), y (toString ((snd cell.position) * size)), width (toString size), height (toString size)][]                
      ,line [Html.Attributes.style (Style.render cell.animation),onClick (Played (position)), x1 (toString (xVal + crossSize)), y1 (toString (yVal + crossSize)), x2 (toString (xVal + (size - crossSize))), y2 (toString (yVal + (size - crossSize))), stroke symbolColor,strokeWidth (toString symbolWidth)] []
      ,line [Html.Attributes.style (Style.render cell.animation),onClick (Played (position)), x1 (toString (xVal + crossSize)), y1 (toString (yVal + (size - crossSize ))), x2 (toString (xVal + (size - crossSize))), y2 (toString (yVal + crossSize)), stroke symbolColor,strokeWidth (toString symbolWidth)] []
      ]
    NoPlayer ->
      rect [Html.Attributes.style (cellStyle ++ (Style.render cell.animation)),onClick (Played (position)), fill symbolColor, stroke boardBgColor, x (toString ((fst cell.position) * size)), y (toString ((snd cell.position) * size)), width (toString size), height (toString size)][]   

statusToMessage : Status -> (List (String,String),String)
statusToMessage status = case status of
    Default -> ([("color","#f57f17")],"New Game...")
    LastMove msg -> ([("color","#2bbbad")],"LastMove - " ++ msg)
    NotValidMove msg -> ([("color","#e57373")],"Exception - " ++ msg)
    GameResult msg -> ([("color","#2bbbad")],"Result - " ++ msg)
    Error msg -> ([("color","#e57373")],"Error - " ++ msg)

