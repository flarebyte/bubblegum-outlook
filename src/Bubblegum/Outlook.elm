module Bubblegum.Outlook exposing (createWidgetModel, widgetModelToPropertyList)

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Create the model
@docs  createWidgetModel, widgetModelToPropertyList

-}
import List
import Set exposing (Set)
import Maybe
import Tuple exposing(first, second)
import String
import Regex exposing (Regex)
import Result
import Maybe.Extra exposing(or)

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
    , format: String
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

prominenceToString: Prominence -> String
prominenceToString prominence =
    case prominence of
        Hidden -> "hidden"
        ReadOnly -> "read-only"
        Visible -> "visible"
        Important -> "important"


{-| Create a widget model from a list of tuples.
-}
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
                    , maxLength = findProperty "max-length" keyValueList |> toIntOrDefault 40
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
                    , format = findProperty "format" keyValueList |> Maybe.withDefault ""
                }
            "long-text" ->
                LongTextWidget { field = fieldModel
                    , regex = findProperty "regex" keyValueList
                    , maxLength = findProperty "max-length" keyValueList |> toIntOrDefault 80
                }
            "text-area" ->
                TextAreaWidget {
                    field = fieldModel
                    , minLines = findProperty "minimum-lines" keyValueList |> toIntOrDefault 3
                    , maxLines = findProperty "maximum-lines" keyValueList |> toIntOrDefault 10
                }
            "markdown-area" ->    
                MarkdownAreaWidget {
                    field = fieldModel
                    , minLines = findProperty "minimum-lines" keyValueList |> toIntOrDefault 3
                    , maxLines = findProperty "maximum-lines" keyValueList |> toIntOrDefault 10
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
                    , maxLength = findProperty "max-length" keyValueList |> toIntOrDefault 40
                }

{-| Convert a widget model to a list of tuples.
-}
widgetModelToPropertyList:  WidgetModel -> List (String, String)
widgetModelToPropertyList model =
    case model of
        CheckboxWidget widget ->
            [("id", widget.id), ("label", widget.label), ("hint", widget.hint), ("prominence", widget.prominence |> prominenceToString),("query", widget.query), ("style", widget.style)] |> List.sort
        IncSpinnerWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("minimum", widget.minimum |> toString), ("maximum", widget.maximum |> toString), ("steps", widget.steps |> toString)] |> List.sort
        MediumTextWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("max-length", widget.maxLength |> toString), ("regex", widget.regex |> Maybe.withDefault "")] |> List.sort
        BoundedListBoxWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("filtering", widget.filtering |> Maybe.withDefault ""), ("sorting", widget.sorting |> Maybe.withDefault "")] |> List.sort
        UnboundedListBoxWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("filtering", widget.filtering |> Maybe.withDefault ""), ("sorting", widget.sorting |> Maybe.withDefault "")] |> List.sort
        RangeSliderWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("minimum", widget.minimum |> toString), ("maximum", widget.maximum |> toString), ("steps", widget.steps |> toString)] |> List.sort
        DateViewerWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("format", widget.format)] |> List.sort
        LongTextWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("max-length", widget.maxLength |> toString), ("regex", widget.regex |> Maybe.withDefault "")] |> List.sort
        TextAreaWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("minimum-lines", widget.minLines |> toString), ("maximum-lines", widget.maxLines |> toString)] |> List.sort
        MarkdownAreaWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("minimum-lines", widget.minLines |> toString), ("maximum-lines", widget.maxLines |> toString)] |> List.sort
        BoundedRadioWidget widget ->
            [("id", widget.field.id), ("label", widget.field.label), ("hint", widget.field.hint), ("prominence", widget.field.prominence |> prominenceToString),("query", widget.field.query), ("style", widget.field.style), ("filtering", widget.filtering |> Maybe.withDefault ""), ("sorting", widget.sorting |> Maybe.withDefault "")] |> List.sort
