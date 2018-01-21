module Bubblegum.Bulma.Utils exposing(..)
{-| This module exposes the rdf vocabulary for Bubblegum.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

See http://flarebyte.github.io/ontologies/2018/user-interface#
which should be abbreviated as the curie ui:

# RDF vocabulary
@docs  vocabulary

-}

import Html.Attributes exposing (class, type_)
import Set exposing(Set, toList, union)
import List exposing(append)

class_field = class "field"
class_control = class "control"
type_text = type_ "text"

maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Nothing -> []
        Just something -> [something]

toStyle: List String -> Set String -> Set String -> Set String -> String
toStyle base prominence styles icons =
    prominence |> Set.union styles |> Set.union icons |> Set.toList |> List.sort |> List.append base |> String.join " "
