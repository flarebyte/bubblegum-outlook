module Bubblegum.Bulma.Constants exposing(..)
{-| This module exposes the rdf vocabulary for Bubblegum.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

See http://flarebyte.github.io/ontologies/2018/user-interface#
which should be abbreviated as the curie ui:

# RDF vocabulary
@docs  vocabulary

-}

import Html.Attributes exposing (class, type_)

class_field = class "field"
class_control = class "control"
type_text = type_ "text"
class_input_is_primary = class "input is-primary"
class_input_is_info = class "input is-info"
class_input_is_succces = class "input is-success"
class_input_is_warning = class "input is-warning"
class_input_is_danger = class "input is-danger"

maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Nothing -> []
        Just something -> [something]
