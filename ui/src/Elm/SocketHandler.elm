module SocketHandler exposing (..)

{-|
  Application using WebSocket to communicate between players.
  WebSokcetHander act like a sender and receiver.
-}
import String
import Array
import Json.Encode
import Json.Decode exposing (list,decodeString, int)
import WebSocket

import CommonTypes exposing (..)
import JsonMapper
import AppConfig

socketListener model receiveMessageAction = WebSocket.listen (AppConfig.wSocketApiUrl model.playerName model.gameCode) receiveMessageAction

{- sendMessage api involves in the Remote play model
  It publish the current play position to other player.
-}

sendMessage playerName gameCode msg = WebSocket.send (AppConfig.wSocketApiUrl playerName gameCode) msg  

{- 
  processIncomingMessage helps to manage socket incoming messages
  If message received it undergoes for further process.
    That should be catched in the any of the defined message category.
    1 ) Game Message - Which is only used to share the play position
    2 ) Event Message - It has only Join and Left message. 
-}

{-processIncomingMessage model message playGameLocal getPlayerJoiningStatus = 
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

    {-parseGamePlayMessage = decodeString JsonMapper.jsonPlayMessageDecoder message    -} 

    handleGameEventMessage = model ! []
      {-case (decodeString JsonMapper.jsonEventMessageDecoder message) of
        Result.Err eventMessageError -> { model | messages = toString eventMessageError :: model.messages} ! []
        Result.Ok eventMessageValue -> 
          if (Array.length eventMessageValue.allMembers <= 1) 
            then 
              { model |
                messages = toString eventMessageValue :: model.messages 
                ,currentPlayer = playerMessageToTypeParser eventMessageValue model.playerName model.currentPlayer 
                ,board = EmptyBoard defaultCells (getPlayerJoiningStatus eventMessageValue.allMembers model.playerName) 
                ,connectionStatus = isOnline eventMessageValue
                ,players = eventMessageValue.allMembers
              } ! []                
            else 
              { model |
               messages = toString eventMessageValue :: model.messages 
               ,currentPlayer = playerMessageToTypeParser eventMessageValue model.playerName model.currentPlayer
               ,board = EmptyBoard defaultCells ((getPlayerJoiningStatus eventMessageValue.allMembers model.playerName) ++"! Ready to play")
               ,connectionStatus = isOnline eventMessageValue
               ,players = eventMessageValue.allMembers
              } ! []    
        -}      
    decodePostionFromSting position = decodeString (list int) position

    {- If possition received in message then parse position string to Position type -}          

    handlePositionMessage playMessageValue = model ! []        
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
-}
{-
  senderAndReceiverEquals check sender and reciver
-}

{-
senderAndReceiverEquals sender receiver = 
  case (sender == receiver) of
    True -> "You"
    False -> sender
-}