module Bubblegum.DivisionModel exposing (Model, fromTriples, toTriples)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}

import Bubblegum.Vocabulary exposing(..)
import Bubblegum.ObjectTriple exposing(..)
import Bubblegum.FieldModel as FieldModel
import Bubblegum.SectionModel as SectionModel

{-| A model for a division containing several sections.
-}
type alias Model = {
        field: FieldModel.Model
        , sections: List SectionModel.Model
    }
 
{-| Creates a division model
-}
fromTriples: String -> List Triple -> Model
fromTriples divisionId tripleList =
    {
        field = FieldModel.fromTriples divisionId tripleList
        , sections = findSubjects ui_partOfComponent divisionId tripleList |> List.map (\s -> SectionModel.fromTriples s tripleList)
    }

{-| Converts a division model to a list of triples
-}
toTriples: Model -> List Triple
toTriples model =
    FieldModel.toTriples model.field ++ (List.map SectionModel.toTriples model.sections |> List.concat)

