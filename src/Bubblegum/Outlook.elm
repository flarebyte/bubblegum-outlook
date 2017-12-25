module Bubblegum.Outlook exposing (createTriple, filter, FilterExpr(..), FieldComparator(..))

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Basics
@docs  createTriple, filter

# Definition
@docs FilterExpr, FieldComparator

-}
import List
import Set exposing (Set)
import Maybe
import Tuple exposing(first, second)
import String
import Regex exposing (Regex)
import Result
import Maybe.Extra exposing(or)

-- TYPES

{-| A simplified n-triple with a subject, a predicate and an object.
It is simplified because it does not discriminate between URI, blank node, language or datatype.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples
-}
type alias Triple = { subject : String, predicate : String, object: String }

{-| A boolean comparator for a string that can be used as part of filter query.
This comparator can be applied to any field: either the subject (WithSubject), the predicate (WithPredicate) or the object (WithObject).

## IsEmpty
Determine if the field is empty.

    WithObject (IsEmpty)

## Equals
Determine if the field is equal to the given value.

    WithSubject (Equals "http://example.org/show/211")

## StartsWith
Determine if the field starts with the given value.

    WithSubject (StartsWith "http://example.org")

## EndsWith
Determine if the field ends with the given value.

    WithSubject (EndsWith "show/211")

## Contains
Determine if the field contains the given value.

    WithSubject (Contains "show")


## Regx
Determine if the field satisfies the given regular expression.

    WithObject (Regx (regex "[Ss]eventies" ))

## IsTrue
Determine if the field is true (contains the string "true").

    WithObject (IsTrue)

## IsFalse
Determine if the field is true (contains the string "false").

    WithObject (IsFalse)

## EqualsAny
Determine if the field is equal any of the given values.

    WithObject (EqualsAny ["subject1", "subject4"])

## GreaterThan
Determine if the field is greater than the given float value.

    WithObject (GreaterThan 6.3)
    WithObject (GreaterThan -2)

## GreaterThanOrEqual
Determine if the field is greater or equal to the given float value.

    WithObject (GreaterThanOrEqual 6.3)

## LessThan
Determine if the field is less than the given float value.

    WithObject (LessThan 0)

## LessThanOrEqual
Determine if the field is less or equal to the given float value.

    WithObject (LessThanOrEqual 6.3)

## Custom
Determine if the field satisfies a custom function against the given value.

    custom: String -> String -> Bool
    custom a b = a == b
    WithObject (Custom custom "subject4")

-}
type FieldComparator = Ignore
  | IsEmpty
  | Equals String
  | StartsWith String
  | EndsWith String
  | Contains String
  | Regx Regex
  | IsTrue
  | IsFalse
  | EqualsAny (List String)
  | GreaterThan Float
  | GreaterThanOrEqual Float
  | LessThan Float
  | LessThanOrEqual Float
  | Custom (String ->String -> Bool) String


{-| A filter expression that can be used to query the list of triples.
In short, you define your expectation (criteria) for each field (WithSubject, WithPredicate, WithObject), and you can compine these criteria by using boolean operators (Or, And, Not).

## WithSubject
Specify a criteria for the subject field.

    WithSubject (Equals "http://example.org/show/211")

## WithPredicate
Specify a criteria for the predicate field.

    WithPredicate (Equals "http://www.w3.org/2000/01/rdf-schema#label")

## WithObject
Specify a criteria for the object field.

    WithObject (Contains "this term")

## Not
Inverts the effect of a filter expression and returns a list of triples that does not match it.

    -- with subject NOT equal to http://example.org/show/211
    Not(WithSubject (Equals "http://example.org/show/211"))

## And
Joins filter expressions clauses with a logical AND returns all the triples that match the conditions of both clauses.

    -- with predicate equals "name" and object "subject4"
    And (WithPredicate (Equals "name"))(WithObject (Equals "subject4"))

## Or
Joins filter expressions clauses with a logical OR returns all the triples that match the conditions of either clauses.

    -- with object equals "subject1" or "subject4"
    filter (Or (WithObject (Equals "subject1"))(WithObject (Equals "subject4")) ) allTriples

-}
type FilterExpr
    = Boolean Bool
    | Not FilterExpr
    | And FilterExpr FilterExpr
    | Or FilterExpr FilterExpr
    | WithSubject FieldComparator
    | WithPredicate FieldComparator
    | WithObject FieldComparator

{-| compare a single field and return true if must be selected -}
fieldCompare: FieldComparator -> String -> Bool
fieldCompare comparator value =
  case comparator of
    Ignore ->
      True
    IsEmpty ->
        String.isEmpty value
    Equals ref ->
        value == ref
    StartsWith ref ->
        String.startsWith ref value
    EndsWith ref ->
        String.endsWith ref value
    Contains ref ->
        String.contains ref value
    Regx regex ->
        Regex.contains regex value
    IsTrue ->
        value == "true"
    IsFalse ->
        value == "true"
    EqualsAny list ->
        List.any (\n -> n == value) list
    GreaterThan ref ->
      Result.map (\n -> n > ref) (String.toFloat value) |> Result.withDefault False
    GreaterThanOrEqual ref ->
      Result.map (\n -> n >= ref) (String.toFloat value) |> Result.withDefault False
    LessThan ref ->
      Result.map (\n -> n < ref) (String.toFloat value) |> Result.withDefault False
    LessThanOrEqual ref ->
      Result.map (\n -> n <= ref) (String.toFloat value) |> Result.withDefault False
    Custom func ref ->
        func ref value


{-| Checks if a triple satisfies a FilterExpr -}
tripleCompare: FilterExpr -> Triple -> Bool
tripleCompare expr triple =
  case expr of
    Boolean value ->
      value
    Not expr ->
        not (tripleCompare expr triple)
    And a b ->
        (tripleCompare a triple) && (tripleCompare b triple)
    Or a b  ->
        (tripleCompare a triple) || (tripleCompare b triple)
    WithSubject comp ->
      fieldCompare comp triple.subject
    WithPredicate comp ->
      fieldCompare comp triple.predicate
    WithObject comp ->
      fieldCompare comp triple.object

{-| Create a simplified n-triple with a subject, a predicate and an object.
It is simplified because it does not discriminate between URI, blank node, language or datatype.
More about rdf n-triples. https://en.wikipedia.org/wiki/N-Triples).

    createTriple "http://example.org/show/218" "http://www.w3.org/2000/01/rdf-schema#label" "That Seventies Show"

Please note that intentionally this library has a very casual approach to the specs, and any string will be accepted.
-}
createTriple: String -> String -> String -> Triple
createTriple s p o = { subject = s, predicate = p, object = o }

{-| filter a list of n-triples based on a filter expression.

    -- Select only the triples which have a given subject
    filter (WithSubject (Equals "http://example.org/show/218")) listOfTriples

    -- Select only the triples which have a label and which starts with "That"
    filter (And (WithPredicate (Equals "http://www.w3.org/2000/01/rdf-schema#label"))(WithObject (StartsWith "That")) ) listOfTriples

-}
filter: FilterExpr -> List Triple -> List Triple
filter tripleFilter list =
  List.filter (tripleCompare tripleFilter) list

-- here

type Prominence = Hidden | ReadOnly| Visible | Important 

{-| The core representation of a field.
    id: unique id for the field
    position: 0 to 7
    label: the label of the field
    hint: a hint about the purpose of the field
    prominence: the prominence of the field (Hidden, ReadOnly, Visible, Important)
    style: the style of the field
    query: a query attached to the field
-}
type alias FieldModel = {
    id: String
    , position: Int
    , label: String
    , hint: String
    , prominence: Prominence
    , style: String
    , query: String
    }

{-| A model for a single line of text.
-}
type alias TextModel = {
    field: FieldModel
    , regex: Maybe String
    , maxLength: Int
    }

{-| A model for multiple lines of text.
-}
type alias TextAreaModel = {
    field: FieldModel
    , minLines: Int
    , maxLines: Int
    }

{-| A model for field representing a relation to a different collection of data.
-}
type alias LinkedFieldModel = {
    field: FieldModel
    , filtering: Maybe String
    , sorting: Maybe String
    }


{-| A model for field representing a decimal counter.
-}
type alias IncSpinnerModel = {
    field: FieldModel
    , minimum: Int
    , maximum: Int
    , steps: Int
    }

{-| A model for field representing a date.
-}
type alias DateViewerModel = {
    field: FieldModel
    , format: Maybe String
    }

{-| A model for a widget.

## CheckboxWidget
A widget representing a checkbox.

    CheckboxWidget fieldModel

## IncSpinnerWidget
A widget representing a counter spinner.

    IncSpinnerWidget incSpinnerModel

## MediumTextWidget
A widget representing a text field of medium size.

    MediumTextWidget textModel

## BoundedListBoxWidget
A widget representing a listbox with a bounded list of items.

    BoundedListBoxWidget linkedFieldModel

## UnboundedListBoxWidget
A widget representing a listbox with an unlimited list of items.

    UnboundedListBoxWidget linkedFieldModel

## RangeSliderWidget
A widget representing a range slider.

    RangeSliderWidget incSpinnerModel

## DateViewerWidget
A widget representing a date.

    DateViewerWidget dateViewerModel

## LongTextWidget
A widget representing a long text field.

    LongTextWidget textModel

## TextAreaWidget
A widget representing a multiple lines text area.

    TextAreaWidget textAreaModel

## MarkdownAreaWidget
A widget representing a markdown text area.

    MarkdownAreaWidget textAreaModel

## BoundedRadioWidget
A widget representing a radio button.

    BoundedRadioWidget linkedFieldModel


-}
type WidgetModel =
    CheckboxWidget FieldModel
    | IncSpinnerWidget IncSpinnerModel
    | MediumTextWidget TextModel
    | BoundedListBoxWidget LinkedFieldModel
    | UnboundedListBoxWidget LinkedFieldModel
    | RangeSliderWidget IncSpinnerModel
    | DateViewerWidget DateViewerModel
    | LongTextWidget TextModel
    | TextAreaWidget TextAreaModel
    | MarkdownAreaWidget TextAreaModel
    | BoundedRadioWidget LinkedFieldModel

{-| A model for a panel containing several widgets.
-}
type alias PanelModel = {
        field: FieldModel
        , widgets: Set WidgetModel
    }

{-| A model for a section containing several panels.
-}
type alias SectionModel = {
        field: FieldModel
        , panels: Set PanelModel
    }

{-| A model for a division containing several sections.
-}
type alias DivisionModel = {
        field: FieldModel
        , sections: Set SectionModel
    }

{-| A widget possibly containing a value.
-}
type WidgetValue = Widget (Maybe String)

{-| A panel possibly containing values.
-}
type alias PanelValues = {
    model: PanelModel
    , values: List WidgetValue
}

{-| A section containing values for panels.
-}
type alias SectionValues = {
    model: SectionModel
    , values: List PanelValues
}
    
{-| A division containing values for sections.
-}
type alias DivisionValues = {
    model: DivisionModel
    , values: List SectionValues
}

findProperty : String -> List (String, String) -> Maybe String
findProperty name list =
    case list of
        [] ->
            Nothing
        
        hd::rest ->
            if first(hd) == name then
                Just (second(hd))
            else
                findProperty name rest

toIntOrDefault:  Int -> Maybe String -> Int
toIntOrDefault default str =
    case str of
        Nothing ->
            default
        Just s ->
            String.toInt s |> Result.withDefault default

toProminence:  Maybe String -> Prominence
toProminence str =
    case str of
        Nothing ->
            Visible
        Just "visible" ->
            Visible
        Just "hidden" ->
            Hidden
        Just "read-only" ->
            ReadOnly
        Just "important" ->
            Important
        Just anything ->
            Visible

createWidgetModel: List (String, String) -> WidgetModel
createWidgetModel keyValueList =
    let
        widgetType = findProperty "type" keyValueList |> Maybe.withDefault "long-text"
        fieldModel = {
            id = findProperty "id" keyValueList |> Maybe.withDefault ""
            , position = findProperty "position" keyValueList |> toIntOrDefault 0
            , label= findProperty "label" keyValueList |> Maybe.withDefault ""
            , hint = findProperty "hint" keyValueList |> Maybe.withDefault ""
            , prominence = findProperty "prominence" keyValueList |> toProminence
            , style = findProperty "style" keyValueList |> Maybe.withDefault ""
            , query = findProperty "query" keyValueList |> Maybe.withDefault ""
        }

        regex = findProperty "regex" keyValueList
        maxLength = findProperty "maxLength" keyValueList |> toIntOrDefault 80
        minLines = findProperty "minLines" keyValueList |> toIntOrDefault 3
        maxLines = findProperty "maxLines" keyValueList |> toIntOrDefault 10
        filtering = findProperty "filtering" keyValueList |> Maybe.withDefault ""
        sorting = findProperty "sorting" keyValueList |> Maybe.withDefault ""
        minimum = findProperty "minimum" keyValueList |> toIntOrDefault 0
        maximum = findProperty "maximum" keyValueList |> toIntOrDefault 10
        steps = findProperty "steps" keyValueList |> toIntOrDefault 1
        format = findProperty "format" keyValueList |> Maybe.withDefault ""
    in
        case widgetType of
            "checkbox" ->
                CheckboxWidget fieldModel
            "inc-spinner" ->
                IncSpinnerWidget {
                    field = fieldModel
                    , minimum = findProperty "minimum" keyValueList |> toIntOrDefault 0
                    , maximum = findProperty "maximum" keyValueList |> toIntOrDefault 10
                    , steps = findProperty "steps" keyValueList |> toIntOrDefault 1
                }
            "medium-text" ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty "regex" keyValueList
                    , maxLength = findProperty "maxLength" keyValueList |> toIntOrDefault 40
                }
            "bounded-listbox" ->    
                BoundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty "filtering" keyValueList
                    , sorting = findProperty "sorting" keyValueList
                }
            "unbounded-listbox" ->
                UnboundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty "filtering" keyValueList
                    , sorting = findProperty "sorting" keyValueList
                }
            "range-slider" ->    
                RangeSliderWidget {
                    field = fieldModel
                    , minimum = findProperty "minimum" keyValueList |> toIntOrDefault 0
                    , maximum = findProperty "maximum" keyValueList |> toIntOrDefault 10
                    , steps = findProperty "steps" keyValueList |> toIntOrDefault 1
                }
            "date-viewer" ->    
                DateViewerWidget {
                    field = fieldModel
                    , format = findProperty "format" keyValueList
                }
            "long-text" ->
                LongTextWidget { field = fieldModel
                    , regex = findProperty "regex" keyValueList
                    , maxLength = findProperty "maxLength" keyValueList |> toIntOrDefault 80
                }
            "text-area" ->
                TextAreaWidget {
                    field = fieldModel
                    , minLines = findProperty "minLines" keyValueList |> toIntOrDefault 3
                    , maxLines = findProperty "maximum" keyValueList |> toIntOrDefault 10
                }
            "markdown-area" ->    
                MarkdownAreaWidget {
                    field = fieldModel
                    , minLines = findProperty "minLines" keyValueList |> toIntOrDefault 3
                    , maxLines = findProperty "maximum" keyValueList |> toIntOrDefault 10
                }
            "bounded-radio" ->    
                BoundedRadioWidget {
                    field = fieldModel
                    , filtering = findProperty "filtering" keyValueList
                    , sorting = findProperty "sorting" keyValueList
                }
            _ ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty "regex" keyValueList
                    , maxLength = findProperty "maxLength" keyValueList |> toIntOrDefault 40
                }