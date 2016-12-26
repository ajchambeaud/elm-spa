module Books.Types exposing (..)

import RemoteData exposing (..)


type alias Book =
    { id : Int
    , title : String
    , author : String
    , reading : Bool
    , read : Int
    }


type alias Model =
    { books : WebData (List Book)
    , formData : Maybe Book
    , saved : WebData Book
    , deleted : WebData Book
    }


type Msg
    = ShowForm
    | Select Int
    | UpdateTitle String
    | UpdateAuthor String
    | UpdateReading Bool
    | UpdateRead String
    | LoadBooks (WebData (List Book))
    | SavedBook (WebData Book)
    | UpdatedBook (WebData Book)
    | DeletedBook (WebData Book)
    | Save
    | Cancel
