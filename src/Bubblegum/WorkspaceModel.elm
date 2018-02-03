module Bubblegum.WorkspaceModel exposing (Model)
{-| This module allow to manipulate a field model

# FieldValue functions
@docs 

-}

import Bubblegum.FieldModel as FieldModel
import Bubblegum.FieldValue as FieldValue
import Bubblegum.FieldEdge as FieldEdge

{-| The core representation of a value.
-}
type alias Model = {
    fields: List FieldModel.Model
    , edges: List FieldEdge.Model
    , values: List FieldValue.Model
}