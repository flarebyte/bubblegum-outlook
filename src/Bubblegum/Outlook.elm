module Bubblegum.Outlook exposing(render)

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Selection
@docs render

-}
import List
import Set exposing (Set)
import Maybe
import Bubblegum.FieldModel as FieldModel
import Bubblegum.FieldValue as FieldValue
import Bubblegum.FieldEdge as FieldEdge

{-| Render.
-}
render: String
render = ""

{-| The core representation of a value.
-}
type alias Model = {
    fields: List FieldModel.Model
    , edges: List FieldEdge.Model
    , values: List FieldValue.Model
    , states: Set String
 }

matchFieldId: String -> FieldModel.Model -> Bool
matchFieldId id field = field.id == id

findFieldModel: Model -> String -> Maybe FieldModel.Model
findFieldModel model id =
  model.fields |> List.filter (matchFieldId id)|> List.head

matchEdgeId: String -> FieldEdge.Model -> Bool
matchEdgeId id field = field.id == id

findFieldEdge: Model -> String -> Maybe FieldEdge.Model
findFieldEdge model id =
  model.edges |> List.filter (matchEdgeId id)|> List.head

matchEdgeSource: String -> FieldEdge.Model -> Bool
matchEdgeSource id field = field.from == id

findFieldEdgesBySource: Model -> String -> List FieldEdge.Model
findFieldEdgesBySource model id =
  model.edges |> List.filter (matchEdgeSource id)

findDestinationsBySource: Model -> String -> List String
findDestinationsBySource model id =
  findFieldEdgesBySource model id |> List.map .to

findDestinationsBySourceAsTuple: Model -> String -> (String, List String)
findDestinationsBySourceAsTuple model id =
  (id, (findDestinationsBySource model id))

findFieldEdges: Model -> List String -> List (Maybe FieldEdge.Model)
findFieldEdges model path =
    List.map (findFieldEdge model) path

matchPath: List String -> FieldValue.Model -> Bool
matchPath path value =
  value.path == path

findFieldValue: Model -> List String -> Maybe FieldValue.Model
findFieldValue model path =
  model.values |> List.filter (matchPath path)|> List.head

findSourceFieldModelByEdgeId: Model -> String -> Maybe FieldModel.Model
findSourceFieldModelByEdgeId model edgeId =
    findFieldEdge model edgeId |> Maybe.map .from |> Maybe.andThen (findFieldModel model)

findDestFieldModelByEdgeId: Model -> String -> Maybe FieldModel.Model
findDestFieldModelByEdgeId model edgeId =
    findFieldEdge model edgeId |> Maybe.map .to |> Maybe.andThen (findFieldModel model)

surelyHead: List String -> String
surelyHead list = List.head list |> Maybe.withDefault "/?"

matchLength: Int -> List String -> Bool
matchLength length list=
    (List.length list) == length

-- algorithm:
-- child to parent is left to right
-- all processes the longest path
--  [a]
--  add [b, a] add [c, a]
--  add [e, b, a] add [f, b , a] add [g, c, a]
findPathsFromSources: Model -> Int -> List (List String) -> List (List String)
findPathsFromSources model pathLength startingList =
    let
        sources = List.filter (matchLength pathLength) startingList |>  List.map surelyHead
        List.map (findDestinationsBySource model) sources
    in

    -- List.map (findDestinationsBySourceAsTuple model) fieldIds

-- findPathsFromSource: Model -> String -> List (List String)
-- findPathsFromSource model rootFieldId =
--     findDestinationsBySource model rootFieldId