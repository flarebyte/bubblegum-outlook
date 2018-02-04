module Bubblegum.FieldValue exposing (Model, resetValue)
{-| This module allow to manipulate a field model

# FieldValue functions
@docs 

-}

import Set exposing (Set)
import Bubblegum.FieldError as FieldError

type Status = Visible | Invisible | Disabled | Reset | Warning

{-| The core representation of a value.
-}
type alias Model = {
    path: List String -- edge ids
    , values: Set String
    , status: Status
    , error: Maybe FieldError.Model
}

{-| The core representation of a value.
-}
resetValue: List String -> Model
resetValue path=
    {
       path = path
       , values = Set.empty
       , status = Reset
       , error = Nothing
    }