module Bubblegum.WidgetModel exposing(Model, fromTriples, toTriples)
{-| The widget model.
-}

import Bubblegum.FieldModel as FieldModel
import Bubblegum.ObjectTriple exposing(..)

{-| A model for a widget.
-}
type alias Model = {
        field: FieldModel.Model
    }

{-| Create a widget model from a list of tuples.
-}
fromTriples: String -> List Triple -> Model
fromTriples subject tripleList =
    {
        field = FieldModel.fromTriples subject tripleList
    }
        
{-| Convert a widget model to a list of tuples.
-}
toTriples: Model -> List Triple
toTriples model = FieldModel.toTriples model.field
