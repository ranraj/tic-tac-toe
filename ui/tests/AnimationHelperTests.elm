module AnimationHelperTests exposing (..)

import Test exposing (..)
import Expect
import String
import CommonTypes
import JsonMapper
import AnimationHelper
import Window

all : Test
all =
    describe "AnimationHelper Suite"
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
 