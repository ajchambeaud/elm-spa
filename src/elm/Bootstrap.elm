module Bootstrap exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
 
type ContainerType = Fixed | Fluid

type ColumnType 
  = Xs Int 
  | Sm Int
  | Md Int
  | Lg Int


columnTypeAsString : ColumnType -> String
columnTypeAsString columnType =
  case columnType of
    Xs num -> "col-xs-" ++ (toString num)
    Sm num -> "col-sm-" ++ (toString num)
    Md num -> "col-md-" ++ (toString num)
    Lg num -> "col-lg-" ++ (toString num)


container : ContainerType -> List (Attribute msg) -> List (Html msg) -> Html msg
container containerType attrs children = 
  let
    newAttrs = 
      case containerType of
        Fixed -> (class "container") :: attrs

        Fluid -> (class "container-fluid") :: attrs
  in
    div newAttrs children


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children = 
  let 
    newAttrs = (class "row") :: attrs
  in
    div newAttrs children


col : List ColumnType -> List (Html msg) -> Html msg
col columnTypes children =
  let
    attr = columnTypes
      |> List.map columnTypeAsString
      |> List.map (\ className -> (className, True))
      |> classList
  in
    div [attr] children
