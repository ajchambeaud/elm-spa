module Layouts.NotFound exposing (root)

import Html exposing (div, h2, Html, text)
import Helpers.Bootstrap exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ row []
            [ col [ Md 12 ]
                [ h2 [] [ text "404 -  Not Found" ] ]
            ]
        ]
