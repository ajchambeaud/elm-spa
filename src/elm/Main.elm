module Books exposing (..)

import String exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App

-- model

type alias Book =
  { id : Int
  , title : String
  , author : String
  , reading : Bool
  , read : Int
  }

type alias Model = 
  { books : List Book
  , formData : Maybe Book
  }

initModel : Model
initModel = 
  { books = []
  , formData = Nothing
  }


-- update 

type Msg
  = ShowForm
  | Select Int
  | UpdateTitle String
  | UpdateAuthor String
  | UpdateReading Bool
  | UpdateRead String
  | Save
  | Cancel

update : Msg -> Model -> Model
update msg model =
  case msg of
    ShowForm ->
      { model 
      | formData = Just
          { id = List.length model.books
          , title = ""
          , author = ""
          , reading = False
          , read = 0
          }
      }

    Select id ->
      { model
      | formData =
          model.books
          |> List.filter (\item -> item.id == id)
          |> List.head
      }

    Save ->
      { model
      | books = 
          case model.formData of 
            Just book -> book :: model.books
            Nothing -> model.books

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
              Just book -> Just (updateFormData book msg)
              Nothing -> model.formData
          )
      }

updateFormData : Book -> Msg -> Book
updateFormData formData msg =
  case msg of
    UpdateTitle value ->
      { formData
      | title = value
      } 

    UpdateAuthor value ->
      { formData
      | author = value
      }

    UpdateReading value->
      { formData
      | reading = value
      }

    UpdateRead value->
      { formData
      | read = Result.withDefault 0 (String.toInt value)
      }

    _ -> formData

-- view 



emptyData : Maybe Book -> Bool
emptyData formData =
  case formData of 
    Just book -> False
    Nothing -> True


actionButton : Maybe Book -> Html Msg
actionButton formData =
  button
    [ onClick 
      ( if (emptyData formData)
      then ShowForm
      else Cancel
      )
    ] 
    [ text (if (emptyData formData) then "Add Book" else "Cancel") ]



bookForm : Book -> Html Msg
bookForm book =
  Html.form []
     [ div []
        [ label [] [text "Id"]
        , input 
            [ type' "text"
            , disabled True
            , value (toString book.id)
            ] []
        ]
    
     , div []
        [ label [] [text "Title"]
        , input
            [ type' "text"
            , value book.title
            , onInput UpdateTitle
            ] []
        ]

     , div []
        [ label [] [text "Author"]
        , input
            [ type' "text"
            , value book.author
            , onInput UpdateAuthor
            ] []
        ]
  
     , div []
        [ label [] [text "Reading"]
        , input
            [ type' "checkbox"
            , onCheck UpdateReading
            ] []
        ]

     , div []
        [ label [] [text "Read"]
        , input
            [ type' "number"
            , value (toString book.read)
            , onInput UpdateRead
            ] []
        ]
    ]


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "My book list"]
    , actionButton model.formData
    , ( case model.formData of 
          Just book ->
            bookForm book

          Nothing ->
            div [][]
      )
    , div [] [text (toString model)]
    ]


main : Program Never
main =
  App.beginnerProgram { model = initModel, view = view, update = update }
