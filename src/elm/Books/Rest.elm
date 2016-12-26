module Books.Rest exposing (..)

import Json.Decode exposing (string, int, bool, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode
import Http
import RemoteData exposing (..)
import Books.Types exposing (..)


bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" int
        |> required "title" string
        |> required "author" string
        |> required "reading" bool
        |> required "read" int


bookEncoder : Book -> Encode.Value
bookEncoder book =
    let
        list =
            [ ( "id", Encode.int book.id )
            , ( "title", Encode.string book.title )
            , ( "author", Encode.string book.author )
            , ( "reading", Encode.bool book.reading )
            , ( "read", Encode.int book.read )
            ]
    in
        list
            |> Encode.object


getBooksCmd : Cmd Msg
getBooksCmd =
    list bookDecoder
        |> Http.get "http://localhost:3000/books"
        |> RemoteData.sendRequest
        |> Cmd.map LoadBooks


saveBookCmd : Book -> Cmd Msg
saveBookCmd book =
    saveRequest book
        |> RemoteData.sendRequest
        |> Cmd.map SavedBook


updateBookCmd : Book -> Cmd Msg
updateBookCmd book =
    updateRequest book
        |> RemoteData.sendRequest
        |> Cmd.map UpdatedBook


deleteBookCmd : Book -> Cmd Msg
deleteBookCmd book =
    updateRequest book
        |> RemoteData.sendRequest
        |> Cmd.map DeletedBook


saveRequest : Book -> Http.Request Book
saveRequest book =
    Http.request
        { body = bookEncoder book |> Http.jsonBody
        , expect = Http.expectJson bookDecoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "http://localhost:3000/books"
        , withCredentials = False
        }


updateRequest : Book -> Http.Request Book
updateRequest book =
    Http.request
        { body = bookEncoder book |> Http.jsonBody
        , expect = Http.expectJson bookDecoder
        , headers = []
        , method = "PUT"
        , timeout = Nothing
        , url = "http://localhost:3000/books/" ++ (toString book.id)
        , withCredentials = False
        }


deleteRequest : Book -> Http.Request Book
deleteRequest book =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> (Ok book))
        , headers = []
        , method = "DELETE"
        , timeout = Nothing
        , url = "http://localhost:3000/books/" ++ (toString book.id)
        , withCredentials = False
        }
