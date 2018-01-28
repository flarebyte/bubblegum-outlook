module Bubblegum.FieldModel exposing (Model, fromTriples, toTriples)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}

import Set exposing (Set)
import Bubblegum.Vocabulary exposing(..)
import Bubblegum.ObjectTriple exposing(..)
import Bubblegum.SettingModel as SettingModel

{-| The core representation of a field.
-}
type alias Model = {
    id: String
    , helpInvalid: Maybe String
    , helpValid: Maybe String
    , hint: Maybe String
    , icons: Set String
    , label: Maybe String
    , languageSyntax: Maybe String --same as date format
    , maxItems: Maybe Int
    , maxLength: Maybe Int
    , maxLines: Maybe Int
    , minItems: Maybe Int
    , minLines: Maybe Int
    , placeholder: Maybe String
    , position: Maybe Int
    , prominence: Set String
    , regex: Maybe String
    , settings: List SettingModel.Model
    , styles: Set String
    , traits: Set String
    , validator: Maybe String
    , partOf: Maybe String
 }

findSettingsInField: String -> List Triple -> List String
findSettingsInField fieldId list =
    List.filter (\t -> t.predicate == ui_settingOfField && t.object == fieldId) list |> List.map .subject |> unique


fromTriples: String -> List Triple -> Model
fromTriples  subject triples =
    {
    id = findProperty subject ui_id triples |> Maybe.withDefault subject
    , helpInvalid = findProperty subject ui_helpInvalid triples
    , helpValid = findProperty subject ui_helpValid triples
    , hint = findProperty subject ui_hint triples
    , partOf = findProperty subject ui_partOfComponent triples
    , icons = findProperties subject ui_icon triples
    , label= findProperty subject ui_label triples
    , languageSyntax= findProperty subject ui_languageSyntax triples
    , maxItems = findProperty subject ui_maxItems triples |> toMaybeInt
    , maxLength = findProperty subject ui_maxLength triples |> toMaybeInt
    , maxLines = findProperty subject ui_maxLines triples |> toMaybeInt
    , minItems = findProperty subject ui_minItems triples |> toMaybeInt
    , minLines = findProperty subject ui_minLines triples |> toMaybeInt
    , placeholder = findProperty subject ui_placeholder triples
    , position = findProperty subject ui_position triples |> toMaybeInt
    , prominence = findProperties subject ui_prominence triples
    , regex = findProperty subject ui_regex triples
    , settings = findSettingsInField subject triples |> List.map (\setting-> SettingModel.fromTriples setting triples)
    , styles = findProperties subject ui_style triples
    , traits = findProperties subject ui_trait triples
    , validator = findProperty subject ui_validator triples
   }
 
toProperties: Model -> List (String, Maybe String)
toProperties field =
    [(ui_id, Just field.id)
    , (ui_helpInvalid, field.helpInvalid)
    , (ui_helpValid, field.helpValid)
    , (ui_hint, field.hint)
    , (ui_partOfComponent, field.partOf)
    , (ui_label, field.label)
    , (ui_languageSyntax, field.languageSyntax)
    , (ui_maxItems , field.maxItems |> toMaybeString)
    , (ui_maxLength , field.maxLength |> toMaybeString)
    , (ui_maxLines , field.maxLines |> toMaybeString)
    , (ui_minItems , field.minItems |> toMaybeString)
    , (ui_minLines , field.minLines |> toMaybeString)
    , (ui_placeholder, field.placeholder)
    , (ui_position , field.position |> toMaybeString)
    , (ui_regex, field.regex)
    , (ui_validator, field.validator)
    ] ++ (toPropertiesAsTuple ui_trait field.traits) 
    ++ (toPropertiesAsTuple ui_prominence field.prominence)
    ++ (toPropertiesAsTuple ui_style field.styles)
     ++ (toPropertiesAsTuple ui_icon field.icons)


settingsToTriples: String -> List SettingModel.Model -> List Triple
settingsToTriples fieldId settings =
   settings |> List.map SettingModel.toTriples |> List.concat
-- 
toTriples:  Model -> List Triple
toTriples model = createListOfTriple model.id (model |> toProperties) ++ (settingsToTriples model.id model.settings)

