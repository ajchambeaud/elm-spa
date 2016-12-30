module Main exposing (..)

import Types exposing (..)
import View exposing (..)
import State exposing (..)
import Html exposing (..)
import Navigation exposing (..)
import Routing exposing (locationToMsg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = View.rootView
        , subscriptions = subscriptions
        }
