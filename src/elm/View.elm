module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap exposing (..)
import Types exposing (..)
import Books.View as Books
import Categories.View as Categories


sideMenuStyle : Attribute msg
sideMenuStyle =
    style
        [ ( "margin-top", "1em" ) ]


sideMenu : Page -> Html Msg
sideMenu page =
    ul [ class "nav nav-pills nav-stacked", sideMenuStyle ]
        [ li
            [ classList [ ( "active", page == BooksPage ) ]
            , attribute "role" "presentation"
            ]
            [ a
                [ href "#"
                , onClick (ChangePage BooksPage)
                ]
                [ text "Books" ]
            ]
        , li
            [ classList [ ( "active", page == CategoriesPage ) ]
            , attribute "role" "presentation"
            ]
            [ a
                [ href "#"
                , onClick (ChangePage CategoriesPage)
                ]
                [ text "Categories" ]
            ]
        ]


rootView : Model -> Html Msg
rootView model =
    let
        page =
            case model.page of
                BooksPage ->
                    Html.map BooksMsg
                        (Books.root model.books)

                CategoriesPage ->
                    Html.map CategoriesMsg
                        (Categories.root model.categories)

        col =
            Bootstrap.col
    in
        container Fluid
            []
            [ row []
                [ col [ Md 2 ] [ sideMenu model.page ]
                , col [ Md 10 ] [ page ]
                ]
            ]
