module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import RenderHelperTests

all : Test
all =
    describe "TicTacToe Suite"
        [
         RenderHelperTests.all
        ]
