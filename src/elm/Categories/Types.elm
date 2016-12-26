module Categories.Types exposing (..)

import RemoteData exposing (..)


type alias Category =
    { id : Int
    , desc : String
    }


type alias Model =
    { categories : WebData (List Category)
    , formData : Maybe Category
    , saved : WebData Category
    , deleted : WebData Category
    }


type Msg
    = ShowForm
    | Select Int
    | UpdateDesc String
    | LoadCategories (WebData (List Category))
    | SavedCategory (WebData Category)
    | UpdatedCategory (WebData Category)
    | DeletedCategory (WebData Category)
    | Save
    | Cancel
