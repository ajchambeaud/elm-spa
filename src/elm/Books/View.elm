module Books.View exposing (root)

import RemoteData exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, value, for, disabled, scope)
import Books.Types exposing (..)
import Html.Events exposing (..)
import Bootstrap exposing (..)


emptyData : Maybe Book -> Bool
emptyData formData =
    case formData of
        Just book ->
            False

        Nothing ->
            True


actionButton : Maybe Book -> Html Msg
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
                "Add Book"
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


bookForm : Book -> Bool -> Html Msg
bookForm book showId =
    Html.form []
        [ div
            [ classList [ ( "form-group", True ), ( "hidden", not showId ) ] ]
            [ label [ for "id" ] [ text "Id" ]
            , input
                [ class "form-control"
                , id "id"
                , type_ "text"
                , disabled True
                , value (toString book.id)
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [ for "title" ] [ text "Title" ]
            , textInput UpdateTitle book.title
            ]
        , div [ class "form-group" ]
            [ label [ for "author" ] [ text "Author" ]
            , textInput UpdateAuthor book.author
            ]
        , div [ class "checkbox" ]
            [ label []
                [ input
                    [ type_ "checkbox", id "reading", onCheck UpdateReading ]
                    []
                , text "Reading"
                ]
            ]
        , div [ class "form-group" ]
            [ label [ for "reading" ] [ text "Read" ]
            , numberInput UpdateRead (toString book.read)
            ]
        , button
            [ class "btn btn-default"
            , type_ "button"
            , onClick Save
            ]
            [ text "Save" ]
        ]


bookRow : Book -> Html Msg
bookRow book =
    tr []
        [ th [ scope "row" ]
            [ text (toString book.id) ]
        , td []
            [ text book.title ]
        , td []
            [ text book.author ]
        , td []
            [ text
                (if book.reading then
                    "Yes"
                 else
                    "No"
                )
            ]
        , td []
            [ text (toString (book.read) ++ "%") ]
        , actionsRow book
        ]


actionsRow : Book -> Html Msg
actionsRow book =
    td []
        [ button
            [ class "btn btn-primary btn-xs"
            , type_ "button"
            , onClick (Select book.id)
            ]
            [ text "Edit" ]
        ]


booksTable : WebData (List Book) -> Html Msg
booksTable booksResponse =
    case booksResponse of
        Success books ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th []
                            [ text "#" ]
                        , th []
                            [ text "Title" ]
                        , th []
                            [ text "Author" ]
                        , th []
                            [ text "Reading" ]
                        , th []
                            [ text "% readed" ]
                        , th []
                            [ text "Actions" ]
                        ]
                    ]
                , tbody [] (List.map bookRow books)
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
                    [ h2 [] [ text "My book list" ] ]
                ]
            , row []
                [ col [ Md 10 ] []
                , col [ Md 2 ]
                    [ actionButton model.formData ]
                ]
            , (case model.formData of
                Just book ->
                    row []
                        [ col [ Md 12 ]
                            [ bookForm book False ]
                        ]

                Nothing ->
                    row [] []
              )
            , row []
                [ col [ Md 12 ]
                    [ hr [] []
                    , booksTable model.books
                    ]
                ]
            ]
