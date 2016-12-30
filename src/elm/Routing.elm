module Routing exposing (..)

import Navigation exposing (..)
import Types exposing (..)


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "" ->
            BooksPage

        "#" ->
            BooksPage

        "#categories" ->
            CategoriesPage

        "#books" ->
            BooksPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        BooksPage ->
            "#books"

        CategoriesPage ->
            "#categories"

        NotFound ->
            "#notfound"


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage
