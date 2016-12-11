module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap exposing (..)
import Categories
import Books


-- model


type alias Model =
    { page : Page
    , books : Books.Model
    , categories : Categories.Model
    }


initModel : Model
initModel =
    { page = BooksPage
    , books = Books.initModel
    , categories = Categories.initModel
    }


type Page
    = BooksPage
    | CategoriesPage



-- update


type Msg
    = ChangePage Page
    | BooksMsg Books.Msg
    | CategoriesMsg Categories.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePage page ->
            { model
                | page = page
            }

        BooksMsg booksMsg ->
            { model
                | books =
                    Books.update booksMsg model.books
            }

        CategoriesMsg categoriesMsg ->
            { model
                | categories =
                    Categories.update categoriesMsg model.categories
            }



-- view


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


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                BooksPage ->
                    Html.map BooksMsg
                        (Books.view model.books)

                CategoriesPage ->
                    Html.map CategoriesMsg
                        (Categories.view model.categories)

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


main : Program Never Model Msg
main =
    beginnerProgram { model = initModel, view = view, update = update }
