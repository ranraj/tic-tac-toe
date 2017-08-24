module AnimationHelper exposing (..)

{-
  TicTacToe application use elm animation with help of elm-style-animation package.
  Style modules are imported from above package.  
-}

import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Animation
import Animation exposing (px, turn, percent)
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
    menuRender status = 
      ( { model
          | menuStyle = 
            (Animation.interrupt
               [ Animation.to status]
                model.menuStyle)
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
        [ Animation.left (px 0.0)
        , Animation.opacity 1.0
        , Animation.color Color.white
        ]
    , closed =
        [ Animation.left (px -350.0)
        , Animation.opacity 0.0
        , Animation.color (Color.white)
        ]
    }

{-
  executeTileAnimation is animating loaded spring effect when tiles in play
  It iterate all the tiles to identify the current tile and then applying the effect.
  This function being called in the Animate Msg type
-}

executeTileAnimation : Animation.Msg -> Cells -> Cells
executeTileAnimation msg cells = 
    List.map (\cell -> { cell | animation = (Animation.update msg) cell.animation }) cells
{-
  applyTileAnimationProperties is applying/setting spring effect when tiles about to play.
-}
applyTileAnimationProperties animation =  
     animation |> (Animation.interrupt
        [ Animation.to
            [ Animation.translate (px 2) (px 2)
            , Animation.scale 0.98
            , Animation.shadow
                { offsetX = 50
                , offsetY = 55
                , blur = 6
                , size = 4
                , color = rgba 0 0 0 0.1
                }
            ]
        , Animation.to
            [ Animation.translate (px 2) (px 2)
            , Animation.scale 1.0
            , Animation.shadow
                { offsetX = 50
                , offsetY = 55
                , blur = 6
                , size = 4                
                , color = rgba 0 0 0 0.1
                }
            ]
        ]
    )