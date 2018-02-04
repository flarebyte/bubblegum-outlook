module Bubblegum.FieldEdge exposing (Model)
{-| This module allow to manipulate a field model

# FieldValue functions
@docs 

-}

import Set exposing (Set)

{-| The core representation of a value.
-}
type alias Model = {
    id: String
    , activations: Set String
    , from: String -- field id
    , maxItems: Maybe Int
    , minItems: Maybe Int
    , to: String -- field id
    , traits: Set String
}