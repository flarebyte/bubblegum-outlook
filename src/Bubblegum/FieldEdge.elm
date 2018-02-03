module Bubblegum.FieldEdge exposing (Model)
{-| This module allow to manipulate a field model

# FieldValue functions
@docs 

-}

import Set exposing (Set)

{-| The core representation of a value.
-}
type alias Model = {
    from: String
    , to: String
    , tags: Set String
}