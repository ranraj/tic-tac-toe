module AnimationHelper exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Style
import Time exposing (Time, second)
import CommonTypes exposing (..)
import Style.Properties exposing (..)
import Style.Spring.Presets
import Color exposing (rgb, rgba,white)
import Time exposing (Time, second)
import Ease

-- Animation style

menuAnimation model flag = 
  let
    menuRender s = 
      ( { model
          | style =
              Style.animate
              |> Style.duration (second / 5)
              |> Style.easing (\x -> x)
              |> Style.to s
              |> Style.on model.style
              ,menu = not flag    
        }
        , Cmd.none
      )
  in
  if(flag) then                                      
    menuRender menuStyles.closed
  else            
    menuRender menuStyles.open

cellAnimation : Time -> List Cell -> List Cell
cellAnimation time cells = 
  let
    cellMap time cell = {cell | animation = Style.tick time cell.animation}
  in
    List.map (cellMap time) cells


loadAnimation animation =            
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

initialWidgetStyle =
    Style.init
        [ Display InlineBlock
        , Rotate 0.0 Turn
        , RotateX 0.0 Turn
        , RotateY 0.0 Turn
        , TranslateY 0.0 Px
        , TranslateX 0.0 Px
        , Rotate 0.0 Turn
        , Opacity 1
        , BackgroundColor (rgba 58 40 69 1.0)
        , Color (rgba 255 255 255 1.0)
        , Scale 1.0                
        ]