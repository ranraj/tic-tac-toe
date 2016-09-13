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

{-| Msg is Action type 
  All the game event handled as Msg
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

{-| Initial Model definition with game default values.
-}

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
  menuStyle = (Style.init menuStyles.closed),
  menuFlag = False,  
  screenSize = (Size 0 0)  
  } 


{-| 
  ProgramWithFlags useful when application state intracting with local storage
-}

main = App.programWithFlags{ init=init,view = view ,update = updateWithStorage,subscriptions= subscriptions }

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
              then model ! [ Api.sendMessage model.playerName model.gameCode "Game.Reset" ]
              else { model | board = EmptyBoard CommonTypes.defaultCells "New Game" } ! [ sizeTask ]

    Animate time ->                 
      case (model.board) of 
        PlayBoard cells -> 
          ({ model | 
            board = PlayBoard ( AnimationHelper.executeTileAnimation time cells),
            menuStyle = Style.tick time model.menuStyle
            },
          Cmd.none)

        EmptyBoard cells msg -> 
          ({model | 
            menuStyle = Style.tick time model.menuStyle
            },
          Cmd.none)

        ErrorBoard cells msg -> 
          ({model | 
            board = ErrorBoard ( AnimationHelper.executeTileAnimation time cells) msg,
            menuStyle = Style.tick time model.menuStyle
            },
          Cmd.none)

        TieBoard cells msg ->  
          ({ model  | 
            board = TieBoard ( AnimationHelper.executeTileAnimation time cells ) msg ,
            status = NotValidMove ("Game Over as match result :" ++ msg),
            menuStyle = Style.tick time model.menuStyle
            },
          Cmd.none)         

        WinBoard cells msg sequence ->  
          ({ model  | 
            board = WinBoard ( AnimationHelper.executeTileAnimation time cells ) msg sequence,
            status = NotValidMove ("Game Over as match result :" ++ msg),
            menuStyle = Style.tick time model.menuStyle
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
         then [ Api.sendMessage model.playerName model.gameCode model.inputMessage ] 
         else []

    ReceiveMessage str -> 
      Api.socketMessageHandler model str

    JoinApply -> 
      Api.joinGame model model.gameCode (EmptyBoard CommonTypes.defaultCells "New Game") ! []

    MenuAction flag -> 
      AnimationHelper.menuAnimation model flag

-- VIEW
view : Model -> Html Msg
view model =    
    div [Html.Attributes.style []] [
     boardBox model     
     ,div
        [Html.Attributes.class "menu-box",  Html.Attributes.style (Style.render model.menuStyle)]
        [ button [Html.Attributes.class "btn-red",onClick (MenuAction model.menuFlag)] [text "Close"]
          ,h1 [] [ text "Tic-Tac-Toe" ]
          ,gameMenu model
        ]        
     , div [] 
        [ div [] 
          [ button 
            [Html.Attributes.class "btn-menu",onClick (MenuAction model.menuFlag)]
            [text "Menu"]
          ]
          , toolBox model
        ]     
    ] 


port setStorage : Json.Encode.Value -> Cmd msg

port focus : String -> Cmd msg

{-| It stores the application status on every update.
-}

updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    (newModel
    , Cmd.batch [ Api.encodeJsonMessage newModel |> setStorage , cmds ]
    )
 
-- TASK
sizeTask = Task.perform Resize Resize Window.size

-- SUBSCRIPTION
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch (getSubscribtions model ReceiveMessage Resize Animate)
   
{-| boardBox function genreates the Html Msg of Game Board.
  It use SVG to draw the tiles and player symbols
  -}

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

toolBox { board, lastMove, playerMode, connectionStatus, gameCode } = 
  let               
    gameStatus = Api.renderStatus board lastMove
  in
    div 
      [Html.Attributes.class "tool-box"]
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

{-| gameMenu renders the game settings section.
  Player can choose of the mode of game from this menu.
  Modes are handled using PlayerMode type.  
  It has actions like create game , join game ,enter player name and online status information.
  -}

gameMenu { board,lastMove,playerMode,playerName,connectionStatus,gameCode,isConnected,messages,players,currentPlayer,menuFlag} = 
  let     
    multiPlayerToolBoxVisible = 
      if (playerMode == MultiPlayer) 
        then "block" 
        else "none"
  in 
  div [ Html.Attributes.class "setting-box" ] [
    div [Html.Attributes.style [ ("padding","5px") ]] 
        [ 
          span 
            [] 
            [text " 2-Player "]
          ,button 
            [Html.Attributes.class "btn-blue",onClick PlayerModeToggle] 
            [text (if playerMode == SinglePlayer then "Standalone" else "Remote")]          
         ]         
         ,div 
           [Html.Attributes.style [("display",multiPlayerToolBoxVisible)]] 
           [ div []
              [ br [] []
                ,input 
                  [ placeholder "Player Name"
                  ,Html.Attributes.class "text-normal"
                  ,onInput InputPlayerName
                  ,Html.Attributes.value playerName] 
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
                    ,Html.Attributes.value gameCode
                  ] 
                  []
                ,button 
                  [Html.Attributes.class "btn-blue",onClick JoinApply] 
                  [text "Join"]
              ] 
            ]
          ] 
          ,div 
            [Html.Attributes.style [("display",if (Array.length players > 0) then "block" else "none")]] 
            [         
             span [] [text "Players : " ]         
             ,span [] [text (Api.playerJoiningStatus players playerName) ]
             ,br [][]
             ,span []
               [
                 text ("Symbol : " ++ case currentPlayer of
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
                     (if isConnected 
                        then "connection-online" 
                        else "connection-offline"
                        )
                     )
                   ,onClick ConnectOrDisconnect] 
                   [ text (if isConnected then "Online" else "Offline")]
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




