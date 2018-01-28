module Bubblegum.PanelModel exposing (Model, fromTriples, toTriples)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}

import Bubblegum.Vocabulary exposing(..)
import Bubblegum.ObjectTriple exposing(..)
import Bubblegum.FieldModel as FieldModel
import Bubblegum.WidgetModel as WidgetModel

{-| A model for a panel containing several widgets.
-}
type alias Model = {
        field: FieldModel.Model
        , widgets: List WidgetModel.Model
    }

findWidgetsInPanel: String -> List Triple -> List String
findWidgetsInPanel panelId list =
    List.filter (\t -> t.predicate == ui_partOfComponent && t.object == panelId) list |> List.map .subject |> unique


{-| Creates a panel model
-}
fromTriples: String -> List Triple -> Model
fromTriples panelId tripleList =
    {
        field = FieldModel.fromTriples panelId tripleList
        , widgets = List.map (\w -> WidgetModel.fromTriples w tripleList) (findWidgetsInPanel panelId tripleList)
    }

{-| Converts a panel model to a list of triples
-}
toTriples: Model -> List Triple
toTriples model =
    FieldModel.toTriples model.field ++ (List.map WidgetModel.toTriples model.widgets |> List.concat)
      