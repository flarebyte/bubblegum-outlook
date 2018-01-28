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

{-| Creates a panel model
-}
fromTriples: String -> List Triple -> Model
fromTriples panelId tripleList =
    {
        field = FieldModel.fromTriples panelId tripleList
        , widgets = findSubjects ui_partOfComponent panelId tripleList |> List.map (\w -> WidgetModel.fromTriples w tripleList)
    }

{-| Converts a panel model to a list of triples
-}
toTriples: Model -> List Triple
toTriples model =
    FieldModel.toTriples model.field ++ (List.map WidgetModel.toTriples model.widgets |> List.concat)
      