module AppStyle exposing (..)
import Html.Attributes exposing (style)
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import Color exposing (rgb, rgba,white)
import Time exposing (Time, second)
import Ease

winTileBg = "#2bbbad"
defaultTileBg = "#9F9F9F"
strokeWidthSize = 20

symbolColor = "#ffffff"

buttonStyle1 =  Html.Attributes.style
    [ ("color","#fff")
    , ("border","none")
    , ("letter-spacing",".5px")
    , ("text-transform","uppercase")
    , ("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")    
    , ("text-decoration","none")
    , ("background-color", "#5C6BC0")
    , ("width", "90px")
    , ("height", "30px")
    , ("padding", "10px 0")
    , ("font-size", ".7em")
    , ("text-align", "center")    
    , ("cursor","pointer")
    ]

buttonStyle2 = Html.Attributes.style
    [ ("color","#fff")
    , ("border","none")
    , ("letter-spacing",".5px")
    , ("text-transform","uppercase")
    , ("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")    
    , ("text-decoration","none")
    , ("background-color", "#EF5350")
    , ("width", "90px")
    , ("height", "30px")
    , ("padding", "10px 0")
    , ("float","right")
    , ("font-size", ".7em")
    , ("text-align", "center")    
    , ("cursor","pointer")
    ]

buttonStyle3 =  Html.Attributes.style
    [ ("color","#fff")
    , ("border","none")
    , ("letter-spacing",".5px")
    , ("text-transform","uppercase")
    , ("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")    
    , ("text-decoration","none")
    , ("background-color", "#5C6BC0")
    , ("width", "90px")
    , ("height", "30px")
    , ("padding", "10px 0")
    , ("font-size", ".7em")
    , ("text-align", "center")    
    , ("cursor","pointer")
    ,("margin","5px")
    ]

statusButton connected =  Html.Attributes.style
    [ ("color","#fff")
    , ("border","none")
    , ("letter-spacing",".5px")
    , ("text-transform","uppercase")
    , ("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")    
    , ("text-decoration","none")
    , ("background-color", if(connected) then "#66BB6A" else "#BDBDBD")
    , ("width", "90px")
    , ("height", "30px")
    , ("padding", "10px 0")
    , ("font-size", ".7em")
    , ("text-align", "center")    
    , ("cursor","pointer")
    ]

floatLeft = ("float","left")    
textStyle1 = 
  Html.Attributes.style
    [ ("color","#000")
    , ("border","none")
    , ("letter-spacing",".5px")    
    , ("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")    
    , ("text-decoration","none")    
    , ("width", "200px")
    , ("height", "10px")
    , ("padding", "10px 0")
    , ("font-size", ".7em")
    , ("text-align", "left")    
    , ("cursor","pointer") 
    ]  
toolBoxStyle =
  Html.Attributes.style
    [ ("width", "100%")
    --, ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "1em")
    , ("text-align", "center")
    , ("margin","0 auto")
    ]

buttonStyle =
  Html.Attributes.style
    [ ("color","#fff")
    , ("border","none")
    , ("letter-spacing",".5px")
    , ("text-transform","uppercase")
    , ("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")    
    , ("text-decoration","none")
    , ("background-color","#F44336")
    , ("width", "40%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "1em")
    , ("text-align", "center")    
    , ("cursor","pointer")
    ]

notificationStyle =          
  Html.Attributes.style
    [ ("width", "40%")
    , ("height", "40px")
    , ("padding", "10")
    , ("font-size", "20px")
    , ("text-align", "center")    
    ]

boardBoxStyle =
  Html.Attributes.style [
  ("margin","5px")
  ,("border","none")
  , ("text-decoration","none")
  ,("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")
  ] 

cellStyle = [  
  ("margin","5px")
  ,("border","none")
  , ("text-decoration","none")
  ,("box-shadow","0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)")
  ,("cursor","pointer")
  ]
  

margin = ("margin","5px")

menuBoxStyle a= Html.Attributes.style ([ ( "position", "absolute" )
                 , ( "top", "-2px" )
                 , ( "margin-left", "-2px" )
                 , ( "padding", "25px" )
                 , ( "width", "300px" )
                 , ( "height", "92.5%" )
                 , ( "background-color", "#00897B" )
                 , ( "color", "white" )                 
                 ] ++ a )

gameSettingsBoxStyle = 
    Html.Attributes.style [
        ("text-align","left"),("margin","auto")
    ]    