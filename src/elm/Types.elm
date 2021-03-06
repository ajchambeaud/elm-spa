module Types exposing (..)

import Categories.Types as Categories
import Books.Types as Books


type alias Model =
    { page : Page
    , books : Books.Model
    , categories : Categories.Model
    }


type Page
    = BooksPage
    | CategoriesPage
    | NotFound


type Msg
    = ChangePage Page
    | Navigate Page
    | BooksMsg Books.Msg
    | CategoriesMsg Categories.Msg
