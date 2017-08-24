module JsonMapper exposing (..)

import Array exposing (Array)
import Json.Encode
import Json.Decode exposing (..)
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

{-playSymbolDecoder : Decoder PlaySymbol
playSymbolDecoder =
    map1 PlaySymbol (field "$type" string)
-}
type alias Member = {
  name : String
  ,playerSymbol : PlaySymbol
}

{-jsonMemberDetailsDecoder : Decoder Member
jsonMemberDetailsDecoder =
    map2 Member (field "name" string) (field "playerSymbol" playSymbolDecoder)
-}
type alias MessageMember =
  {
  messageType : String 
  ,member : Member
  ,gameId : String
  ,allMembers : Array String
}

{-jsonEventMessageDecoder : Decoder MessageMember
jsonEventMessageDecoder =
    map2 MessageMember (field "$type" string) (field "member" jsonMemberDetailsDecoder) (field "gameId" string) (decodeString (array string ) "allMembers")
-}
type alias JsonMessageContent = {
  messageType : String
  ,sender : String
  ,gameId : String
  ,message : String
}
{-jsonPlayMessageDecoder : Decoder JsonMessageContent
jsonPlayMessageDecoder = object4 JsonMessageContent (decodeString string "$type") (decodeString string "sender") (decodeString string "gameId") (decodeString string "message")   
-}
-- Http message parser
{-decodeGameCode = at decodeString string ["code"] -}
-- Local storage Parsers

--Encode
{-encodeJson model cells =   
    Json.Encode.object    
      [ ("playerName", Json.Encode.string model.playerName)    
      , ("gameCode", Json.Encode.string model.gameCode)    
      , ("lastMove", postionTupleEncode model.lastMove)
      , ("isConnected", Json.Encode.bool model.isConnected)
      , ("playerMode",encodePlayerMode model.playerMode)    
      , ("cells",cells |> encodeCells )
      ]  
-}      
{-
encodePlayerMode : PlayerMode -> Json.Encode.Value
encodePlayerMode playerMode =  
  case playerMode of
    SinglePlayer  -> Json.Encode.string "SinglePlayer"
    MultiPlayer   -> Json.Encode.string "MultiPlayer"   
-}    
{-
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
-}
--Decode

{-decodeJson : Json.Decode.Value -> Result String StoredModel
decodeJson modelJson =
  Json.Decode.decodeValue modelDecoder modelJson
-}
{-modelDecoder : Json.Decode.Decoder StoredModel
modelDecoder =
  let
    cellDecoder =
      Json.Decode.object2 StoredCell        
        ("position"   := positionTupleDecoder)        
        ("player" := playerDecoder) 
    cellsDecoder = Json.Decode.list cellDecoder        
    
  in
    Json.Decode.object6 StoredModel        
    (decodeString string "playerName")        
    (decodeString string "gameCode")        
    ("lastMove"   := positionTupleDecoder)        
    ("isConnected" := Json.Decode.bool)
    ("playerMode" := playerModeDecoder)    
    ("cells" := cellsDecoder)  
-}
{-playerModeDecoder : Json.Decode.Decoder PlayerMode
playerModeDecoder =
  let
    decodePlyerMode playerMode =
      case playerMode of
        "SinglePlayer" -> Result.Ok SinglePlayer
        "MultiPlayer" -> Result.Ok MultiPlayer        
        _ -> Result.Err ("Not a valid PlayerMode: " ++ playerMode)
  in
    Json.Decode.customDecoder decodeString decodePlyerMode 
-}
{-playerDecoder : Json.Decode.Decoder Player
playerDecoder =
  let
    decodePlayer player =
      case player of
        "PlayerO" -> Result.Ok PlayerO
        "PlayerX" -> Result.Ok PlayerX        
        "NoPlayer" -> Result.Ok NoPlayer
        _ -> Result.Err ("Not a valid Player: " ++ player)
  in
    Json.Decode.customDecoder decodeString decodePlayer     

positionTupleDecoder = Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int
-}