port module View exposing (..)

{-| gameMenu renders the game settings section.
  Player can choose of the mode of game from this menu.
  Modes are handled using PlayerMode type.  
  It has actions like create game , join game ,enter player name and online status information.
-}

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (class,style,checked,readonly,placeholder)
import Svg exposing (Svg,g,rect,circle,line)
import Svg.Attributes exposing (..)
import Window exposing (Size)
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import Array

-- Custom Module

import Api exposing (..)
import RenderHelper exposing (..)
import CommonTypes exposing (..)
import AnimationHelper exposing (..)

{-|
    Base view has two division.
        1) Board Box
        3) Menu Box
        2) Tool Box
-}
baseView : Model -> Html Msg
baseView model =     
    div [Html.Attributes.style []] [
     boardBox model     
     ,gameMenu model        
     ,toolBox model      
    ] 


{-| boardBox function genreates the Html Msg of Game Board.
  It use SVG to draw the tiles and player symbols
-}

boardBox : Model -> Html Msg
boardBox { board,screenSize } =       
  div [ Html.Attributes.style [ ("float", boxAlignStyle screenSize) ]] 
      [          
        Svg.svg 
          [ Svg.Attributes.class "board-box"
          , version "1.1"
          , x "0"
          , y "0"
          , viewBox "0 0" 
          ,width (px (boardSize screenSize))
          ,height (px (boardSize screenSize))
          ] 
        (drawBoard board screenSize)            
      ]

{-| toolBox function creates New / Join button , Status message.  
  This sections paints the game status and messages.
-}

toolBox : Model -> Html Msg
toolBox { board, lastMove, playerMode, connectionStatus, gameCode ,menuFlag } = 
  let               
    gameStatus = Api.renderStatus board lastMove
  in
    div 
      [] 
      [ 
        div [] 
            [ button 
              [Html.Attributes.class "btn-menu",onClick (MenuAction menuFlag)]
              [text "Menu"]
            ]
          ,div  [Html.Attributes.class "tool-box"]
                [
                  button 
                    [Html.Attributes.class "btn-green",onClick Reset] 
                    [text (playModeToMessage playerMode connectionStatus)]  
                  ,div 
                    [ Html.Attributes.style (fst gameStatus) ] 
                    [ 
                      span 
                        [Html.Attributes.class "notification"] 
                        [text (snd gameStatus)]
                    ]                  
                ] 
      ]
      

gameMenu : Model -> Html Msg
gameMenu model = 
  let     
    multiPlayerToolBoxVisible = 
      if (model.playerMode == MultiPlayer) 
        then "block" 
        else "none"
    
    gameMenuBody = div [ Html.Attributes.class "setting-box" ] [
        div [Html.Attributes.style [ ("padding","5px") ]] 
            [ 
              span [] 
                   [text " 2-Player "]
              ,button 
                  [Html.Attributes.class "btn-blue",onClick PlayerModeToggle] 
                  [text (if model.playerMode == SinglePlayer then "Standalone" else "Remote")]          
             ]         
             ,div 
                [Html.Attributes.style [("display",multiPlayerToolBoxVisible)]] 
                [ div []
                  [ br [] []
                    ,input 
                      [ placeholder "Player Name"
                      ,Html.Attributes.class "text-normal"
                      ,onInput InputPlayerName
                      ,Html.Attributes.value model.playerName] 
                      []           
              ]
              ,div [] [
                br [] []
                ,div[] [
                  button [Html.Attributes.class "btn-blue",onClick CreateGameEvent] [text "Create"]                                    
                ]
                ,div[][                        
                  div [][
                    input 
                      [ placeholder "Code"
                        ,Html.Attributes.class "text-normal"
                        ,onInput InputGameCode
                        ,Html.Attributes.value model.gameCode
                      ] 
                      []
                    ,button 
                      [Html.Attributes.class "btn-blue",onClick JoinApply] 
                      [text "Join"]
                  ] 
                ]
              ] 
              ,div 
                [Html.Attributes.style [("display",if (Array.length model.players > 0) then "block" else "none")]] 
                [         
                 span [] 
                      [text "Players : " ]         
                 ,span [] 
                       [text (Api.getPlayerJoiningStatus model.players model.playerName) ]
                 ,br [][]
                 ,span []
                       [
                         text ("Symbol : " ++ case model.currentPlayer of
                                      NoPlayer -> "_"
                                      PlayerX -> "X"
                                      PlayerO -> "O") 
                       ]
                 ]
              ,div [][
                label []
                  [ br [] []              
                  , button 
                     [ Html.Attributes.class ("connection-status " 
                         ++ 
                         (if model.isConnected 
                            then "connection-online" 
                            else "connection-offline"
                            )
                         )
                       ,onClick ConnectOrDisconnect] 
                       [ text (if model.isConnected then "Online" else "Offline")]
                  ]
                ]
             ]
             --UI Game Message logger
             ,div []
              [                     
                input [ Html.Attributes.class "text-normal",onInput InputMessage ] []
                , button [ Html.Attributes.class "btn-blue",onClick SendMessage ] [ text "Send" ]
             -- ,div [] (List.map (\msg -> div [] [ text msg ]) messages)-}         
              ]
      ]
    in
      div
       [Html.Attributes.class "menu-box",  Html.Attributes.style (Style.render model.menuStyle)]
       [ button [Html.Attributes.class "btn-red",onClick (MenuAction model.menuFlag)] [text "Close"]
       ,h1 [] [ text "Tic-Tac-Toe" ]
       ,gameMenuBody
       ]
{-| DrawBoard function renders the game board with help of SVG and Html
    It has different symbol for players and different color for completed board.
-}
    
drawBoard : Board -> Window.Size -> List (Svg Msg)
drawBoard board screenSize = 
  let
    drawTiles cells = List.map (mapCellToTile board screenSize) cells 
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
        symbolColor = "#ffffff"  
        boardBgColor =  
          case (List.member position (getWinnerBoardSequence board)) of
            True -> "#2bbbad"
            False -> "#9F9F9F"        
      in
        case player of
          PlayerO ->        
            g [] 
              [
                rect 
                  [ Svg.Attributes.class "cell"
                   ,Html.Attributes.style (Style.render animation)
                   ,onClick (Play (position))
                   ,fill boardBgColor
                   ,stroke symbolColor
                   ,x (toString xVal)
                   ,y (toString yVal)
                   ,width (toString size)
                   ,height (toString size)
                  ] []                      
                ,circle 
                  [ 
                    Html.Attributes.style (Style.render animation)
                    ,onClick (Play (position))
                    , fill symbolColor
                    , cx (toString ((xVal + circleSize) + times))
                    , cy (toString ((yVal + circleSize) + times))
                    , r (toString ((symbolWidth * 2) + times)) 
                  ] []
                ,circle 
                  [
                   Html.Attributes.style (Style.render animation)
                   ,onClick (Play (position))
                   , fill boardBgColor
                   , cx (toString ((xVal + circleSize) + times))
                   , cy (toString ((yVal + circleSize) + times))
                   , r (toString (symbolWidth + times)) 
                  ] []
             ]
          PlayerX -> 
             g [] 
              [
              rect 
                [
                  Svg.Attributes.class "cell"
                  ,Html.Attributes.style (Style.render animation)
                  ,onClick (Play (position))
                  , fill boardBgColor
                  , stroke symbolColor
                  , x (toString ((fst position) * size))
                  , y (toString ((snd position) * size))
                  , width (toString size)
                  , height (toString size)
                ] []                
             ,line 
               [
                 Html.Attributes.style (Style.render animation)
                 ,onClick (Play (position))
                 , x1 (toString (xVal + crossSize))
                 , y1 (toString (yVal + crossSize))
                 , x2 (toString (xVal + (size - crossSize)))
                 , y2 (toString (yVal + (size - crossSize)))
                 , stroke symbolColor,strokeWidth (toString symbolWidth)
                ] []
             ,line 
               [
                 Html.Attributes.style (Style.render animation)
                 ,onClick (Play (position))
                 , x1 (toString (xVal + crossSize))
                 , y1 (toString (yVal + (size - crossSize )))
                 , x2 (toString (xVal + (size - crossSize)))
                 , y2 (toString (yVal + crossSize))
                 , stroke symbolColor,strokeWidth (toString symbolWidth)
               ] []
            ]
          NoPlayer ->
            rect 
              [
                Svg.Attributes.class "cell"
                ,Html.Attributes.style (Style.render animation)
                ,onClick (Play (position))
                ,fill symbolColor
                ,stroke boardBgColor
                ,x (toString ((fst position) * size))
                ,y (toString ((snd position) * size))
                ,width (toString size)
                ,height (toString size)
              ] []   

  in        
  case board of  
    EmptyBoard cells msg-> drawTiles cells
    PlayBoard cells -> drawTiles cells
    WinBoard cells msg winSequence -> drawTiles cells
    TieBoard cells msg -> drawTiles cells
    ErrorBoard cells msg ->  drawTiles cells
