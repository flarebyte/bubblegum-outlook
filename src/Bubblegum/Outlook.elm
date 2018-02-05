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

-- [b, a] -> return [e, b, a] and [f, b , a]
expandDestinations: Model -> List String -> List (List String)
expandDestinations model path =
    surelyHead path |> findDestinationsBySource model |> List.map (\p -> p :: path)


type alias TempPathStack = {
    total:  List (List String)
    , toExpand:  List (List String)
}
-- algorithm:
-- child to parent is left to right
-- all processes the longest path
--  [a]
--  add [b, a] add [c, a]
--  add [e, b, a] add [f, b , a] add [g, c, a]
findPathsFromSources: Model -> TempPathStack -> TempPathStack
findPathsFromSources model stack  =
    let
         toExpandNext = stack.toExpand |>  List.map (expandDestinations model) |> List.concat      
    in
        { toExpand = toExpandNext , total = toExpandNext ++ stack.total }
 