module AnimationHelper exposing (..)

{-
  TicTacToe application use elm animation with help of elm-style-animation package.
  Style modules are imported from above package.  
-}

import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import Color exposing (rgb, rgba,white)
import Time exposing (Time, second)
import Ease

-- Custom Module
import CommonTypes exposing (..)

{- 
  Slide Menu animation style definition
  menuAnimation takes model and flag as parameter 
  model is used to update the menuStyle property
  and Menu will be opend or closed based on the flag value
-}

menuAnimation model flag = 
  let
    menuRender s = 
      ( { model
          | menuStyle =
              Style.animate
              |> Style.duration (second / 5)
              |> Style.easing (\x -> x)
              |> Style.to s
              |> Style.on model.menuStyle
              ,menuFlag = not flag    
        }
        , Cmd.none
      )
  in
  if(flag) then                                      
    menuRender menuStyles.closed
  else            
    menuRender menuStyles.open

{-
  menuStyle is object notation wish holds the menu animation open and close properties
-}

menuStyles =
    { open =
        [ Left 0.0 Px
        , Opacity 1.0
        , Color (white)
        ]
    , closed =
        [ Left -350.0 Px
        , Opacity 0.0
        , Color (white)
        ]
    }

{-
  executeTileAnimation is animating loaded spring effect when tiles in play
  It iterate all the tiles to identify the current tile and then applying the effect.
  This function being called in the Animate Msg type
-}

executeTileAnimation : Time -> Cells -> Cells
executeTileAnimation time cells = 
  let
    cellMap time cell = {cell | animation = Style.tick time cell.animation}
  in
    List.map (cellMap time) cells

{-
  applyTileAnimationProperties is applying/setting spring effect when tiles about to play.
-}

applyTileAnimationProperties animation =            
    Style.queue 
    |> Style.spring Style.Spring.Presets.noWobble
    |> Style.duration (0.1 * second)
    |> Style.to
        [ Scale 0.98
        ]
    |> Style.andThen
    |> Style.spring Style.Spring.Presets.wobbly
    |> Style.duration (0.1 * second)
    |> Style.to
        [ Scale 1.0
        ]
    |> (\act -> Style.on animation act)            


