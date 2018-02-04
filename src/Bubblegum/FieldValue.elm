module Bubblegum.FieldValue exposing (Model)
{-| This module allow to manipulate a field model

# FieldValue functions
@docs 

-}

import Set exposing (Set)
import Result exposing (Result)
import Bubblegum.FieldError as FieldError

type Status = Visible | Invisible | Disabled

{-| The core representation of a value.
-}
type alias Model = {
    path: List String -- edge ids
    , result: Result FieldError.Model (Set String)
    , status: Status
}