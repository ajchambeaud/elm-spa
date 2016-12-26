module Categories.Rest exposing (..)

import Json.Decode exposing (string, int, bool, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode
import Http
import RemoteData exposing (..)
import Categories.Types exposing (..)


categoryDecoder : Decoder Category
categoryDecoder =
    decode Category
        |> required "id" int
        |> required "desc" string


categoryEncoder : Category -> Encode.Value
categoryEncoder category =
    let
        list =
            [ ( "id", Encode.int category.id )
            , ( "desc", Encode.string category.desc )
            ]
    in
        list
            |> Encode.object


getCategoriesCmd : Cmd Msg
getCategoriesCmd =
    list categoryDecoder
        |> Http.get "http://localhost:3000/categories"
        |> RemoteData.sendRequest
        |> Cmd.map LoadCategories


saveCategoryCmd : Category -> Cmd Msg
saveCategoryCmd category =
    saveRequest category
        |> RemoteData.sendRequest
        |> Cmd.map SavedCategory


updateCategoryCmd : Category -> Cmd Msg
updateCategoryCmd category =
    updateRequest category
        |> RemoteData.sendRequest
        |> Cmd.map UpdatedCategory


deleteCategoryCmd : Category -> Cmd Msg
deleteCategoryCmd category =
    updateRequest category
        |> RemoteData.sendRequest
        |> Cmd.map DeletedCategory


saveRequest : Category -> Http.Request Category
saveRequest category =
    Http.request
        { body = categoryEncoder category |> Http.jsonBody
        , expect = Http.expectJson categoryDecoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "http://localhost:3000/categories"
        , withCredentials = False
        }


updateRequest : Category -> Http.Request Category
updateRequest category =
    Http.request
        { body = categoryEncoder category |> Http.jsonBody
        , expect = Http.expectJson categoryDecoder
        , headers = []
        , method = "PUT"
        , timeout = Nothing
        , url = "http://localhost:3000/categories/" ++ (toString category.id)
        , withCredentials = False
        }


deleteRequest : Category -> Http.Request Category
deleteRequest category =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> (Ok category))
        , headers = []
        , method = "DELETE"
        , timeout = Nothing
        , url = "http://localhost:3000/categories/" ++ (toString category.id)
        , withCredentials = False
        }
