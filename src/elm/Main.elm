module Main exposing (..)

import String exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App

import Bootstrap exposing (..)


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
          { id = List.length model.books + 1
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
            Just book -> saveBook book model.books
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


saveBook : Book -> List Book -> List Book
saveBook book list =
  let
    ids = List.map (\item -> item.id) list
  in
    if List.member book.id ids then
      List.map (\item -> if item.id == book.id then book else item) list
    else
      book :: list


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
    , class "btn btn-default pull-right"        
    ] 
    [ text (if (emptyData formData) then "Add Book" else "Cancel") ]


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


bookForm : Book -> Bool -> Html Msg
bookForm book showId=
  Html.form []
    [ div 
        [ classList [("form-group", True), ("hidden", not showId)] ]
        [ label [ for "id" ] [ text "Id" ]
        , input 
            [ class "form-control"
            , id "id"
            , type' "text"
            , disabled True
            , value (toString book.id)
            ]
            []
        ]
    
     , div [ class "form-group" ]
        [ label [ for "title" ] [ text "Title" ]
        , textInput UpdateTitle book.title
        ]

     , div [ class "form-group" ]
        [ label [ for "author" ] [text "Author"]
        , textInput UpdateAuthor book.author
        ]
  
    , div [ class "checkbox" ]
        [ label []
            [ input 
              [ type' "checkbox", id "reading", onCheck UpdateReading ] []
            , text "Reading"
            ]
        ]

     , div [ class "form-group" ]
        [ label [ for "reading" ] [text "Read"]
        , numberInput UpdateRead (toString book.read)
        ]
  
      , button 
        [ class "btn btn-default"
        , type' "button"
        , onClick Save
        ]
        [ text "Save" ]
    ]


bookRow : Book -> Html Msg
bookRow book = 
  tr []
    [ th [ scope "row" ]
        [ text (toString book.id) ]
    , td []
        [ text book.title ]
    , td []
        [ text book.author ]
    , td []
        [ text ( 
            if book.reading 
              then "Yes"
              else "No" 
            ) 
        ]
    , td []
        [ text (toString (book.read) ++ "%")]
    , actionsRow book
    ]

actionsRow : Book -> Html Msg
actionsRow book =
  td []
    [ button 
        [ class "btn btn-primary btn-xs"
        , type' "button"
        , onClick (Select book.id)
        ]
        [ text "Edit" ]
    ]

booksTable : List Book -> Html Msg
booksTable books =
  table [ class "table" ]
      [ thead []
          [ tr []
              [ th []
                  [ text "#" ]
              , th []
                  [ text "Title" ]
              , th []
                  [ text "Author" ]
              , th []
                  [ text "Reading" ]
              , th []
                  [ text "% readed" ]
              , th []
                  [ text "Actions" ]
              ]
          ]
      , tbody [] ( List.map bookRow books)
      ]

view : Model -> Html Msg
view model =
  let
    col = Bootstrap.col
  in
    container Fixed []
      [ row []
          [ col [ Md 12 ] 
              [ h2 [] [text "My book list"] ]
          ]

      , row []
          [ col [ Md 10 ] []
          , col [ Md 2 ]
            [ actionButton model.formData ]
          ]
    
      , ( case model.formData of 
            Just book ->
              row []
                [ col [ Md 12 ] 
                    [ bookForm book False ]
                ]
            Nothing ->
              row [][]
        )

      , row []
          [ col [ Md 12 ] 
              [ hr [][]
              , booksTable model.books
              ]
          ]
      ]


main : Program Never
main =
  App.beginnerProgram { model = initModel, view = view, update = update }
