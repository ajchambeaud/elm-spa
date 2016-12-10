module Categories exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Bootstrap exposing (..)


-- model


type alias Category =
  { id : Int
  , desc : String
  }

type alias Model = 
  { categories : List Category
  , formData : Maybe Category
  }

initModel : Model
initModel = 
  { categories = []
  , formData = Nothing
  }


-- update 


type Msg
  = ShowForm
  | Select Int
  | UpdateDesc String
  | Save
  | Cancel

update : Msg -> Model -> Model
update msg model =
  case msg of
    ShowForm ->
      { model 
      | formData = Just
          { id = List.length model.categories + 1
          , desc = ""
          }
      }

    Select id ->
      { model
      | formData =
          model.categories
          |> List.filter (\item -> item.id == id)
          |> List.head
      }

    Save ->
      { model
      | categories = 
          case model.formData of 
            Just category -> saveCategory category model.categories
            Nothing -> model.categories

      , formData = Nothing
      }

    Cancel -> 
      { model
      | formData = Nothing
      }

    _ -> 
      { model
      | formData = 
          ( case model.formData of 
              Just category -> Just (updateFormData category msg)
              Nothing -> model.formData
          )
      }


saveCategory : Category -> List Category -> List Category
saveCategory category list =
  let
    ids = List.map (\item -> item.id) list
  in
    if List.member category.id ids then
      List.map (\item -> if item.id == category.id then category else item) list
    else
      category :: list


updateFormData : Category -> Msg -> Category
updateFormData formData msg =
  case msg of
    UpdateDesc value ->
      { formData
      | desc = value
      }

    _ -> formData


-- view 


emptyData : Maybe Category -> Bool
emptyData formData =
  case formData of 
    Just category -> False
    Nothing -> True


actionButton : Maybe Category -> Html Msg
actionButton formData =
  button
    [ onClick 
      ( if (emptyData formData)
      then ShowForm
      else Cancel
      )
    , class "btn btn-default pull-right"        
    ] 
    [ text (if (emptyData formData) then "Add Category" else "Cancel") ]


textInput : (String -> msg) -> String -> Html msg
textInput msg textValue =
  input 
    [ class "form-control"
    , type' "text"
    , value textValue
    , onInput msg
    ] []


numberInput : (String -> msg) -> String -> Html msg
numberInput msg textValue =
  input 
    [ class "form-control"
    , id "read"
    , type' "number"
    , value textValue
    , onInput msg
    ] []


categoryForm : Category -> Bool -> Html Msg
categoryForm category showId=
  Html.form []
    [ div 
        [ classList [("form-group", True), ("hidden", not showId)] ]
        [ label [ for "id" ] [ text "Id" ]
        , input 
            [ class "form-control"
            , id "id"
            , type' "text"
            , disabled True
            , value (toString category.id)
            ]
            []
        ]
    
     , div [ class "form-group" ]
        [ label [ for "desc" ] [ text "Description" ]
        , textInput UpdateDesc category.desc
        ]

      , button 
        [ class "btn btn-default"
        , type' "button"
        , onClick Save
        ]
        [ text "Save" ]
    ]


categoryRow : Category -> Html Msg
categoryRow category = 
  tr []
    [ th [ scope "row" ]
        [ text (toString category.id) ]
    , td []
        [ text category.desc ]
    , actionsRow category
    ]

actionsRow : Category -> Html Msg
actionsRow category =
  td []
    [ button 
        [ class "btn btn-primary btn-xs"
        , type' "button"
        , onClick (Select category.id)
        ]
        [ text "Edit" ]
    ]

categoriesTable : List Category -> Html Msg
categoriesTable categories =
  table [ class "table" ]
      [ thead []
          [ tr []
              [ th []
                  [ text "#" ]
              , th []
                  [ text "Description" ]
              , th []
                  [ text "Actions" ]
              ]
          ]
      , tbody [] ( List.map categoryRow categories)
      ]

view : Model -> Html Msg
view model =
  let
    col = Bootstrap.col
  in
    div []
      [ row []
          [ col [ Md 12 ] 
              [ h2 [] [text "My category list"] ]
          ]

      , row []
          [ col [ Md 10 ] []
          , col [ Md 2 ]
            [ actionButton model.formData ]
          ]
    
      , ( case model.formData of 
            Just category ->
              row []
                [ col [ Md 12 ] 
                    [ categoryForm category False ]
                ]
            Nothing ->
              row [][]
        )

      , row []
          [ col [ Md 12 ] 
              [ hr [][]
              , categoriesTable model.categories
              ]
          ]
      ]
