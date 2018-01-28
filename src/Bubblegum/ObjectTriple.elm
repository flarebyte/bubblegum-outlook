module Bubblegum.ObjectTriple exposing(..)

{-| This module exposes functions to deal with triples

# Object Triple functions
@docs 

-}

import List
import Set exposing (Set)
import Maybe
import Tuple exposing(first, second)
import String
import Result

type alias Triple = { subject : String, predicate : String, object: String }

findProperty : String -> String -> List Triple -> Maybe String
findProperty subject name list =
    case list of
        [] ->
            Nothing
        
        hd::rest ->
            if hd.subject == subject && hd.predicate == name then
                Just (hd.object)
            else
                findProperty subject name rest

findProperties: String -> String -> List Triple -> Set String
findProperties subject predicate list =
    List.filter (\t -> t.subject == subject && t.predicate == predicate) list |> List.map .object |> Set.fromList

toProperties: String -> String -> Set String -> List Triple
toProperties subject predicate set =
    set |> Set.toList |> List.map (\t -> { subject = subject , predicate = predicate, object = t })

toPropertiesAsTuple: String -> Set String -> List (String , Maybe String)
toPropertiesAsTuple predicate set =
    set |> Set.toList |> List.map (\t -> ( predicate,  Just t ))

tupleToTriple: String -> (String, String) -> Triple
tupleToTriple subject keyValue = {subject= subject, predicate= first(keyValue), object= second(keyValue)}

maybeTuple: (String, Maybe String) -> Maybe (String, String)
maybeTuple tuple =
    case second(tuple) of
        Nothing -> Nothing
        Just obj -> Just(first(tuple), obj)

createListOfTriple: String -> List (String, Maybe String) -> List Triple
createListOfTriple subject keyValueList =
     List.filterMap maybeTuple keyValueList |> List.map (tupleToTriple subject)

toMaybeString: Maybe Int -> Maybe String
toMaybeString value = Maybe.map toString value

toIntOrZero: String -> Int
toIntOrZero value =  String.toInt value |> Result.withDefault 0

toMaybeInt: Maybe String -> Maybe Int
toMaybeInt value = Maybe.map toIntOrZero value

unique: List String -> List String
unique list = list |> Set.fromList |> Set.toList

findSubjects: String -> String -> List Triple -> List String
findSubjects predicate obj list =
    List.filter (\t -> t.predicate == predicate && t.object == obj) list |> List.map .subject |> unique
