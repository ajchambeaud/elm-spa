module Books.State exposing (..)

import Books.Types exposing (..)
import RemoteData exposing (..)
import Books.Rest exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { books = NotAsked
      , formData = Nothing
      , saved = NotAsked
      , deleted = NotAsked
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowForm ->
            ( { model
                | formData =
                    initFormData model.formData model.books
              }
            , Cmd.none
            )

        Select id ->
            ( { model
                | formData =
                    selectBook id model.books
              }
            , Cmd.none
            )

        Save ->
            ( model
            , upsertFormDataCmd model.formData model.books
            )

        Cancel ->
            ( { model
                | formData = Nothing
              }
            , Cmd.none
            )

        SavedBook response ->
            ( { model
                | books = addBook response model.books
                , formData = Nothing
                , saved = response
              }
            , Cmd.none
            )

        UpdatedBook response ->
            ( { model
                | books = updateBook response model.books
                , formData = Nothing
                , saved = response
              }
            , Cmd.none
            )

        DeletedBook response ->
            ( { model
                | books = removeBook response model.books
                , formData = Nothing
                , deleted = response
              }
            , Cmd.none
            )

        LoadBooks response ->
            ( { model
                | books = response
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | formData = updateFormData model.formData msg
              }
            , Cmd.none
            )


upsertFormDataCmd : Maybe Book -> WebData (List Book) -> Cmd Msg
upsertFormDataCmd formData wdList =
    case ( formData, wdList ) of
        ( Just book, Success books ) ->
            let
                inList =
                    books
                        |> List.map .id
                        |> List.member book.id
            in
                if inList then
                    updateBookCmd book
                else
                    saveBookCmd book

        _ ->
            Cmd.none


initFormData : Maybe book -> WebData (List Book) -> Maybe Book
initFormData formData wdList =
    case wdList of
        Success books ->
            Just
                { id = List.length books + 1
                , title = ""
                , author = ""
                , reading = False
                , read = 0
                }

        _ ->
            Nothing


selectBook : Int -> WebData (List Book) -> Maybe Book
selectBook id wdList =
    case wdList of
        Success books ->
            books
                |> List.filter (\book -> book.id == id)
                |> List.head

        _ ->
            Nothing


updateBook : WebData Book -> WebData (List Book) -> WebData (List Book)
updateBook wdBook wdList =
    case ( wdBook, wdList ) of
        ( Success book, Success list ) ->
            list
                |> List.map
                    (\item ->
                        if item.id == book.id then
                            book
                        else
                            item
                    )
                |> Success

        _ ->
            wdList


addBook : WebData Book -> WebData (List Book) -> WebData (List Book)
addBook wdBook wdList =
    case ( wdBook, wdList ) of
        ( Success book, Success list ) ->
            book :: list |> Success

        _ ->
            wdList


removeBook : WebData Book -> WebData (List Book) -> WebData (List Book)
removeBook wdBook wdList =
    case ( wdBook, wdList ) of
        ( Success book, Success list ) ->
            list
                |> List.filter (\item -> item.id /= book.id)
                |> Success

        _ ->
            wdList


updateFormData : Maybe Book -> Msg -> Maybe Book
updateFormData formData msg =
    case ( msg, formData ) of
        ( UpdateTitle value, Just book ) ->
            Just
                { book
                    | title = value
                }

        ( UpdateAuthor value, Just book ) ->
            Just
                { book
                    | author = value
                }

        ( UpdateReading value, Just book ) ->
            Just
                { book
                    | reading = value
                }

        ( UpdateRead value, Just book ) ->
            Just
                { book
                    | read = Result.withDefault 0 (String.toInt value)
                }

        _ ->
            formData
