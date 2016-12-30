module State exposing (..)

import Types exposing (..)
import Books.State
import Categories.State
import Books.Rest
import Categories.Rest
import Navigation exposing (..)
import Routing exposing (..)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash
    in
        ( initModel page, initialCmd page )


initModel : Page -> Model
initModel page =
    { page = page
    , books = Tuple.first Books.State.init
    , categories = Tuple.first Categories.State.init
    }


initialCmd : Page -> Cmd Msg
initialCmd page =
    case page of
        BooksPage ->
            Cmd.map BooksMsg Books.Rest.getBooksCmd

        CategoriesPage ->
            Cmd.map CategoriesMsg Categories.Rest.getCategoriesCmd

        NotFound ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( model
            , newUrl <| pageToHash page
            )

        ChangePage page ->
            ( { model
                | page = page
              }
            , case page of
                BooksPage ->
                    Cmd.map BooksMsg Books.Rest.getBooksCmd

                CategoriesPage ->
                    Cmd.map CategoriesMsg Categories.Rest.getCategoriesCmd

                NotFound ->
                    Cmd.none
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
