module Main exposing (..)

import Types exposing (..)
import View exposing (..)
import State exposing (..)
import Html exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = View.rootView
        , subscriptions = subscriptions
        }
