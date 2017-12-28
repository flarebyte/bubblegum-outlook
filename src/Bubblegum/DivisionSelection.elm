module Bubblegum.DivisionSelection exposing (Model, fromTriples, toTriples)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}
import Set exposing (Set)
import Bubblegum.Vocabulary exposing(..)
import Bubblegum.ObjectTriple exposing(..)

{-| A model for the division selection.
-}
type alias Model = {
        language: Maybe String
        , divisionId: Maybe String
        , from: Maybe Int
        , selected: Set String -- unique instance ids
        , term: Maybe String
        , to: Maybe Int
        , viewMode: Maybe String
    }
        
{-| Create the division selection
-}
fromTriples: List Triple -> Model
fromTriples tripleList =
    {
        language = findProperty ui_mainSelection ui_language tripleList
        , divisionId = findProperty ui_mainSelection ui_divisionId tripleList
        , from = findProperty ui_mainSelection ui_searchFrom tripleList |>toMaybeInt
        , selected = findProperties ui_mainSelection ui_searchSelected tripleList
        , term = findProperty ui_mainSelection ui_searchTerm tripleList
        , to = findProperty ui_mainSelection ui_searchTo tripleList |>toMaybeInt
        , viewMode = findProperty ui_mainSelection ui_viewMode tripleList --ex: admin
    }

{-| Converts a division selection model to a list of triples
-}
toTriples: Model -> List Triple
toTriples model =
    [
        (ui_language, model.language) 
        ,(ui_viewMode, model.viewMode)
        ,(ui_divisionId, model.divisionId)
        ,(ui_searchTerm, model.term)
        ,(ui_searchFrom, (model.from |> toMaybeString))
        ,(ui_searchTo, (model.to |> toMaybeString))
    ]  |> createListOfTriple ui_mainSelection 
    |> (++) (toProperties ui_mainSelection ui_searchSelected model.selected)
   