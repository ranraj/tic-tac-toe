module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import CommonTypes
import JsonMapper
import RenderHelper
import Window

all : Test
all =
    describe "RenderHelper Suite"
        [
         windowPortrait
        ,windowLandscape
        ,tileSizePortrait
        ,tileSizeLandscape
        ,boardSizePortrait
        ,boardSizeLandscape
        ,boxAlignStylePortrait
        ,boxAlignStyleLandscape
        ]

windowPortrait = test "Portrait" <| 
    \() -> Expect.equal (Window.Size 200 700 |> RenderHelper.screenOrientation |> toString) "Portrait"
windowLandscape = test "Landscape" <| 
    \() -> Expect.equal (Window.Size 700 200 |> RenderHelper.screenOrientation |> toString) "Landscape"

{- BoardSize is calculated based on Screen Orientation -}

boardSizePortrait = test "boardSizePortrait" <| 
    \() -> Expect.lessThan 200 (Window.Size 200 700 |> RenderHelper.boardSize) 
boardSizeLandscape = test "boardSizeLandscape" <| 
    \() -> Expect.lessThan 200 (Window.Size 700 200 |> RenderHelper.boardSize)

{- TileSize is calculated based on the lowest value of either width or height -}

tileSizePortrait = test "tileSizePortrait" <| 
    \() -> Expect.greaterThan 60 (Window.Size 200 700 |> RenderHelper.tileSize) 
tileSizeLandscape = test "tileSizeLandscape" <| 
    \() -> Expect.greaterThan 60 (Window.Size 700 200 |> RenderHelper.tileSize)

{- boxAlignStyle is calculated based Orientation , It returns css float value -}

boxAlignStylePortrait = test "boxAlignStylePortrait" <| 
    \() -> Expect.equal "none" (Window.Size 200 700 |> RenderHelper.boxAlignStyle) 
boxAlignStyleLandscape = test "boxAlignStyleLandscape" <| 
    \() -> Expect.equal "left" (Window.Size 700 200 |> RenderHelper.boxAlignStyle)    

    