module Categories.State exposing (..)

import Categories.Types exposing (..)
import RemoteData exposing (..)
import Categories.Rest exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { categories = NotAsked
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
                    initFormData model.formData model.categories
              }
            , Cmd.none
            )

        Select id ->
            ( { model
                | formData =
                    selectCategory id model.categories
              }
            , Cmd.none
            )

        Save ->
            ( model
            , upsertFormDataCmd model.formData model.categories
            )

        Cancel ->
            ( { model
                | formData = Nothing
              }
            , Cmd.none
            )

        SavedCategory response ->
            ( { model
                | categories = addCategory response model.categories
                , formData = Nothing
                , saved = response
              }
            , Cmd.none
            )

        UpdatedCategory response ->
            ( { model
                | categories = updateCategory response model.categories
                , formData = Nothing
                , saved = response
              }
            , Cmd.none
            )

        DeletedCategory response ->
            ( { model
                | categories = removeCategory response model.categories
                , formData = Nothing
                , deleted = response
              }
            , Cmd.none
            )

        LoadCategories response ->
            ( { model
                | categories = response
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | formData = updateFormData model.formData msg
              }
            , Cmd.none
            )


upsertFormDataCmd : Maybe Category -> WebData (List Category) -> Cmd Msg
upsertFormDataCmd formData wdList =
    case ( formData, wdList ) of
        ( Just category, Success categories ) ->
            let
                inList =
                    categories
                        |> List.map .id
                        |> List.member category.id
            in
                if inList then
                    updateCategoryCmd category
                else
                    saveCategoryCmd category

        _ ->
            Cmd.none


initFormData : Maybe category -> WebData (List Category) -> Maybe Category
initFormData formData wdList =
    case wdList of
        Success categories ->
            Just
                { id = List.length categories + 1
                , desc = ""
                }

        _ ->
            Nothing


selectCategory : Int -> WebData (List Category) -> Maybe Category
selectCategory id wdList =
    case wdList of
        Success categories ->
            categories
                |> List.filter (\categorie -> categorie.id == id)
                |> List.head

        _ ->
            Nothing


updateCategory : WebData Category -> WebData (List Category) -> WebData (List Category)
updateCategory wdCategory wdList =
    case ( wdCategory, wdList ) of
        ( Success category, Success list ) ->
            list
                |> List.map
                    (\item ->
                        if item.id == category.id then
                            category
                        else
                            item
                    )
                |> Success

        _ ->
            wdList


addCategory : WebData Category -> WebData (List Category) -> WebData (List Category)
addCategory wdCategory wdList =
    case ( wdCategory, wdList ) of
        ( Success category, Success list ) ->
            category :: list |> Success

        _ ->
            wdList


removeCategory : WebData Category -> WebData (List Category) -> WebData (List Category)
removeCategory wdCategory wdList =
    case ( wdCategory, wdList ) of
        ( Success category, Success list ) ->
            list
                |> List.filter (\item -> item.id /= category.id)
                |> Success

        _ ->
            wdList


updateFormData : Maybe Category -> Msg -> Maybe Category
updateFormData formData msg =
    case ( msg, formData ) of
        ( UpdateDesc value, Just category ) ->
            Just
                { category
                    | desc = value
                }

        _ ->
            formData
