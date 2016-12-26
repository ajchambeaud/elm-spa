module State exposing (..)

import Types exposing (..)
import Books.State
import Categories.State
import Books.Rest
import Categories.Rest


init : ( Model, Cmd Msg )
init =
    ( initModel, initialCmd )


initModel : Model
initModel =
    { page = BooksPage
    , books = Tuple.first Books.State.init
    , categories = Tuple.first Categories.State.init
    }


initialCmd : Cmd Msg
initialCmd =
    Cmd.map BooksMsg Books.Rest.getBooksCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            ( { model
                | page = page
              }
            , case page of
                BooksPage ->
                    Cmd.map BooksMsg Books.Rest.getBooksCmd

                CategoriesPage ->
                    Cmd.map CategoriesMsg Categories.Rest.getCategoriesCmd
            )

        BooksMsg booksMsg ->
            let
                ( bookModel, bookCmd ) =
                    model.books
                        |> Books.State.update booksMsg
            in
                ( { model
                    | books = bookModel
                  }
                , Cmd.map BooksMsg bookCmd
                )

        CategoriesMsg categoriesMsg ->
            let
                ( catModel, catCmd ) =
                    model.categories
                        |> Categories.State.update categoriesMsg
            in
                ( { model
                    | categories = catModel
                  }
                , Cmd.map CategoriesMsg catCmd
                )
