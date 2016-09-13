module JsonMapper exposing (..)

import Array exposing (Array)
import Json.Encode
import Json.Decode exposing (Decoder,tuple2,decodeString, int, string, object1,object2,object4, (:=),array,at)
import CommonTypes exposing (..)

type alias PlaySymbol = {
  messageType : String  
}

type alias StoredModel = {
  playerName : String,
  gameCode : String,
  lastMove : Position,
  isConnected : Bool,
  playerMode : PlayerMode,  
  cells : List StoredCell
}

type alias StoredCell = {
  position : Position,
  player : Player
}

playSymbolDecoder : Decoder PlaySymbol
playSymbolDecoder =
    object1 PlaySymbol ("$type" := Json.Decode.string)

type alias Member = {
  name : String
  ,playerSymbol : PlaySymbol
}
jsonMemberDetailsDecoder : Decoder Member
jsonMemberDetailsDecoder =
    object2 Member ("name" := Json.Decode.string) ("playerSymbol" := playSymbolDecoder)

type alias MessageMember =
  {
  messageType : String 
  ,member : Member
  ,gameId : String
  ,allMembers : Array String
}

jsonEventMessageDecoder : Decoder MessageMember
jsonEventMessageDecoder =
    object4 MessageMember ("$type" := Json.Decode.string) ("member" := jsonMemberDetailsDecoder) ("gameId" := Json.Decode.string) ("allMembers" := array Json.Decode.string)

type alias JsonMessageContent = {
  messageType : String
  ,sender : String
  ,gameId : String
  ,message : String
}
jsonPlayMessageDecoder : Decoder JsonMessageContent
jsonPlayMessageDecoder = object4 JsonMessageContent ("$type" := Json.Decode.string) ("sender" := Json.Decode.string) ("gameId" := Json.Decode.string) ("message" := Json.Decode.string)   

-- Http message parser
decodeGameCode = at ["code"] Json.Decode.string
-- Local storage Parsers

--Encode
encodeJson model cells =   
    Json.Encode.object    
      [ ("playerName", Json.Encode.string model.playerName)    
      , ("gameCode", Json.Encode.string model.gameCode)    
      , ("lastMove", postionTupleEncode model.lastMove)
      , ("isConnected", Json.Encode.bool model.isConnected)
      , ("playerMode",encodePlayerMode model.playerMode)    
      , ("cells",cells |> encodeCells )
      ]  

encodePlayerMode : PlayerMode -> Json.Encode.Value
encodePlayerMode playerMode =  
  case playerMode of
    SinglePlayer  -> Json.Encode.string "SinglePlayer"
    MultiPlayer   -> Json.Encode.string "MultiPlayer"   

encodePlayer : Player -> Json.Encode.Value
encodePlayer player =  
  case player of
    PlayerO  -> Json.Encode.string "PlayerO"
    PlayerX   -> Json.Encode.string "PlayerX"   
    NoPlayer   -> Json.Encode.string "NoPlayer"   

encodeCells cells = 
  let
    encodeCell cell = 
      Json.Encode.object    
        [ ("position", postionTupleEncode cell.position)    
        , ("player", encodePlayer cell.player)        
      ]   
  in
    List.map encodeCell cells |> Json.Encode.list

postionTupleEncode position = Array.fromList [ fst position |> Json.Encode.int  , snd position |> Json.Encode.int ] |> Json.Encode.array

--Decode

decodeJson : Json.Decode.Value -> Result String StoredModel
decodeJson modelJson =
  Json.Decode.decodeValue modelDecoder modelJson

modelDecoder : Json.Decode.Decoder StoredModel
modelDecoder =
  let
    cellDecoder =
      Json.Decode.object2 StoredCell        
        ("position"   := positionTupleDecoder)        
        ("player" := playerDecoder) 
    cellsDecoder = Json.Decode.list cellDecoder        
    
  in
    Json.Decode.object6 StoredModel        
    ("playerName"   := Json.Decode.string)        
    ("gameCode" := Json.Decode.string)        
    ("lastMove"   := positionTupleDecoder)        
    ("isConnected" := Json.Decode.bool)
    ("playerMode" := playerModeDecoder)    
    ("cells" := cellsDecoder)  

playerModeDecoder : Json.Decode.Decoder PlayerMode
playerModeDecoder =
  let
    decodePlyerMode playerMode =
      case playerMode of
        "SinglePlayer" -> Result.Ok SinglePlayer
        "MultiPlayer" -> Result.Ok MultiPlayer        
        _ -> Result.Err ("Not a valid PlayerMode: " ++ playerMode)
  in
    Json.Decode.customDecoder Json.Decode.string decodePlyerMode 

playerDecoder : Json.Decode.Decoder Player
playerDecoder =
  let
    decodePlayer player =
      case player of
        "PlayerO" -> Result.Ok PlayerO
        "PlayerX" -> Result.Ok PlayerX        
        "NoPlayer" -> Result.Ok NoPlayer
        _ -> Result.Err ("Not a valid Player: " ++ player)
  in
    Json.Decode.customDecoder Json.Decode.string decodePlayer     

positionTupleDecoder = Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int
