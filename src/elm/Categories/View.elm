module Categories.View exposing (root)

import RemoteData exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, value, for, disabled, scope)
import Categories.Types exposing (..)
import Html.Events exposing (..)
import Helpers.Bootstrap as Bootstrap exposing (..)


emptyData : Maybe Category -> Bool
emptyData formData =
    case formData of
        Just category ->
            False

        Nothing ->
            True


actionButton : Maybe Category -> Html Msg
actionButton formData =
    button
        [ onClick
            (if (emptyData formData) then
                ShowForm
             else
                Cancel
            )
        , class "btn btn-default pull-right"
        ]
        [ text
            (if (emptyData formData) then
                "Add Category"
             else
                "Cancel"
            )
        ]


textInput : (String -> msg) -> String -> Html msg
textInput msg textValue =
    input
        [ class "form-control"
        , type_ "text"
        , value textValue
        , onInput msg
        ]
        []


numberInput : (String -> msg) -> String -> Html msg
numberInput msg textValue =
    input
        [ class "form-control"
        , id "read"
        , type_ "number"
        , value textValue
        , onInput msg
        ]
        []


categoryForm : Category -> Bool -> Html Msg
categoryForm category showId =
    Html.form []
        [ div
            [ classList [ ( "form-group", True ), ( "hidden", not showId ) ] ]
            [ label [ for "id" ] [ text "Id" ]
            , input
                [ class "form-control"
                , id "id"
                , type_ "text"
                , disabled True
                , value (toString category.id)
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [ for "desc" ] [ text "Description" ]
            , textInput UpdateDesc category.desc
            ]
        , button
            [ class "btn btn-default"
            , type_ "button"
            , onClick Save
            ]
            [ text "Save" ]
        ]


categoryRow : Category -> Html Msg
categoryRow category =
    tr []
        [ th [ scope "row" ]
            [ text (toString category.id) ]
        , td []
            [ text category.desc ]
        , actionsRow category
        ]


actionsRow : Category -> Html Msg
actionsRow category =
    td []
        [ button
            [ class "btn btn-primary btn-xs"
            , type_ "button"
            , onClick (Select category.id)
            ]
            [ text "Edit" ]
        ]


categorysTable : WebData (List Category) -> Html Msg
categorysTable categorysResponse =
    case categorysResponse of
        Success categorys ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th []
                            [ text "#" ]
                        , th []
                            [ text "Description" ]
                        , th []
                            [ text "Actions" ]
                        ]
                    ]
                , tbody [] (List.map categoryRow categorys)
                ]

        Loading ->
            div [] [ text "Loading..." ]

        NotAsked ->
            div [] [ text "Initializing.." ]

        Failure err ->
            div [] [ text ("Error: " ++ toString err) ]


root : Model -> Html Msg
root model =
    let
        col =
            Bootstrap.col
    in
        div []
            [ row []
                [ col [ Md 12 ]
                    [ h2 [] [ text "My category list" ] ]
                ]
            , row []
                [ col [ Md 10 ] []
                , col [ Md 2 ]
                    [ actionButton model.formData ]
                ]
            , (case model.formData of
                Just category ->
                    row []
                        [ col [ Md 12 ]
                            [ categoryForm category False ]
                        ]

                Nothing ->
                    row [] []
              )
            , row []
                [ col [ Md 12 ]
                    [ hr [] []
                    , categorysTable model.categories
                    ]
                ]
            ]
