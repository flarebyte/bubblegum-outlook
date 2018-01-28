module Bubblegum.SectionModel exposing (Model, fromTriples, toTriples)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}

import Bubblegum.Vocabulary exposing(..)
import Bubblegum.ObjectTriple exposing(..)
import Bubblegum.FieldModel as FieldModel
import Bubblegum.PanelModel as PanelModel

{-| A model for a section containing several panels.
-}
type alias Model = {
        field: FieldModel.Model
        , panels: List PanelModel.Model
    }

{-| Creates a section model
-}
fromTriples: String -> List Triple -> Model
fromTriples sectionId tripleList =
    {
        field = FieldModel.fromTriples sectionId tripleList
        , panels = findSubjects ui_partOfComponent sectionId tripleList |> List.map (\p -> PanelModel.fromTriples p tripleList)
    }

{-| Converts a section model to a list of triples
-}
toTriples: Model -> List Triple
toTriples model =
    FieldModel.toTriples model.field ++ (List.map PanelModel.toTriples model.panels |> List.concat)