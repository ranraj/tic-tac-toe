module AppConfig exposing (..)

serverAddress = "172.17.1.12:8080"
-- serverAddress = "localhost:8080"
-- serverAddress = "damp-forest-74836.herokuapp.com"


wSocketApiUrl name code = "ws://" ++ serverAddress ++ "/game/join?name=" ++ name ++ "&id=" ++ code

gameCodeApiUrl name= "http://" ++ serverAddress ++ "/game/code/request?name=" ++ name