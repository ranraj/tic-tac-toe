module CommonTypes exposing (..)
import Style

type Board = EmptyBoard Cells String | PlayBoard Cells | WinBoard Cells String (List Position) | TieBoard Cells String | ErrorBoard Cells String

type PlayerMode = SinglePlayer | MultiPlayer

type Player = PlayerO | PlayerX | NoPlayer

type alias Cells = List Cell

type alias Cell = {
    position : Position,    
    player : Player,
    animation : Style.Animation     
}

type alias Position = (Int,Int)