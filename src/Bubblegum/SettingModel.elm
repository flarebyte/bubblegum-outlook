module Bubblegum.SettingModel exposing (Model, fromTriples, toTriples)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}

import Set exposing (Set)
import Bubblegum.Vocabulary exposing(..)
import Bubblegum.ObjectTriple exposing(..)

{-| The core representation of a field.
-}
type alias Model = {
    id: String
    , fieldId: String
    , key: Maybe String
    , values: Set String
    , facets: Set String
    }

fromTriples: String -> List Triple -> Model
fromTriples  subject keyValueList =
    { id = findProperty subject ui_id keyValueList |> Maybe.withDefault subject
    , fieldId = findProperty subject ui_settingOfField keyValueList |> Maybe.withDefault "!!fieldId?"
    , key = findProperty subject ui_settingKey keyValueList
    , values = findProperties subject ui_settingValue keyValueList
    , facets = findProperties subject ui_settingFacet keyValueList
   }
 
toProperties: Model -> List (String, Maybe String)
toProperties field =
    [ (ui_id, Just field.id) ,(ui_settingKey, field.key), (ui_settingOfField, Just field.fieldId)] 
    ++ (toPropertiesAsTuple ui_settingValue field.values) 
    ++ (toPropertiesAsTuple ui_settingFacet field.facets)


toTriples:  Model -> List Triple
toTriples model = createListOfTriple model.id (model |> toProperties)