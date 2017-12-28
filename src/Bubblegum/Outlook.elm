module Bubblegum.Outlook exposing (createWidgetModel, widgetModelToTriple)

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Create the model
@docs  createWidgetModel, widgetModelToTriple

-}
import List
import Set exposing (Set)
import Maybe
import Tuple exposing(first, second)
import String
import Regex exposing (Regex)
import Result
import Maybe.Extra exposing(or)
import Ntriples.Filter exposing (createTriple, filter, FilterExpr(..), FieldComparator(..))

type Prominence = Hidden | ReadOnly| Visible | Important

type alias Triple = { subject : String, predicate : String, object: String }

p = "http://flarebyte.github.io/ontologies/2018/user-interface#"

u =
  {
    id = "http://flarebyte.github.io/ontologies/2018/user-interface#id"
    , widgetType = "http://flarebyte.github.io/ontologies/2018/user-interface#widget-type"
    , position = "http://flarebyte.github.io/ontologies/2018/user-interface#position"
    , label = "http://flarebyte.github.io/ontologies/2018/user-interface#label"
    , hint = "http://flarebyte.github.io/ontologies/2018/user-interface#hint"
    , prominence = "http://flarebyte.github.io/ontologies/2018/user-interface#prominence"
    , style = "http://flarebyte.github.io/ontologies/2018/user-interface#style"
    , query = "http://flarebyte.github.io/ontologies/2018/user-interface#query"
    , regex = "http://flarebyte.github.io/ontologies/2018/user-interface#regex"
    , maxLength = "http://flarebyte.github.io/ontologies/2018/user-interface#maximum-length"
    , minLines = "http://flarebyte.github.io/ontologies/2018/user-interface#minimum-lines"
    , maxLines = "http://flarebyte.github.io/ontologies/2018/user-interface#maximum-lines"
    , filtering = "http://flarebyte.github.io/ontologies/2018/user-interface#filtering"
    , sorting = "http://flarebyte.github.io/ontologies/2018/user-interface#sorting"
    , minimumInt = "http://flarebyte.github.io/ontologies/2018/user-interface#minimum-int"
    , maximumInt = "http://flarebyte.github.io/ontologies/2018/user-interface#maximum-int"
    , stepsInt = "http://flarebyte.github.io/ontologies/2018/user-interface#steps-int"
    , format = "http://flarebyte.github.io/ontologies/2018/user-interface#format"
    , checkbox = "http://flarebyte.github.io/ontologies/2018/user-interface#checkbox"
    , incSpinner = "http://flarebyte.github.io/ontologies/2018/user-interface#inc-spinner"
    , mediumText = "http://flarebyte.github.io/ontologies/2018/user-interface#medium-text"
    , boundedListbox = "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-listbox"
    , unboundedListbox = "http://flarebyte.github.io/ontologies/2018/user-interface#unbounded-listbox"
    , rangeSlider = "http://flarebyte.github.io/ontologies/2018/user-interface#range-slider"
    , dateViewer = "http://flarebyte.github.io/ontologies/2018/user-interface#date-viewer" 
    , longText = "http://flarebyte.github.io/ontologies/2018/user-interface#long-text"
    , textArea = "http://flarebyte.github.io/ontologies/2018/user-interface#text-area"
    , markdownArea = "http://flarebyte.github.io/ontologies/2018/user-interface#markdown-area"
    , boundedradio = "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-radio"
 }
 

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

findProperty : String -> List Triple -> Maybe String
findProperty name list =
    case list of
        [] ->
            Nothing
        
        hd::rest ->
            if hd.predicate == name then
                Just (hd.predicate)
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


createFieldModel: List Triple -> FieldModel
createFieldModel  keyValueList =
    {
    id = findProperty u.id keyValueList |> Maybe.withDefault ""
    , position = findProperty u.position keyValueList |> toIntOrDefault 0
    , label= findProperty u.label keyValueList |> Maybe.withDefault ""
    , hint = findProperty u.hint keyValueList |> Maybe.withDefault ""
    , prominence = findProperty u.prominence keyValueList |> toProminence
    , style = findProperty u.style keyValueList |> Maybe.withDefault ""
    , query = findProperty u.query keyValueList |> Maybe.withDefault ""
    }
 

{-| Create a widget model from a list of tuples.
-}
createWidgetModel: List Triple -> WidgetModel
createWidgetModel keyValueList =
    let
        widgetType = findProperty u.widgetType keyValueList |> Maybe.withDefault "long-text"
        fieldModel = createFieldModel keyValueList
    in
        case widgetType of
            "http://flarebyte.github.io/ontologies/2018/user-interface#checkbox" ->
                CheckboxWidget fieldModel
            "http://flarebyte.github.io/ontologies/2018/user-interface#inc-spinner" ->
                IncSpinnerWidget {
                    field = fieldModel
                    , minimum = findProperty u.minimumInt keyValueList |> toIntOrDefault 0
                    , maximum = findProperty u.maximumInt keyValueList |> toIntOrDefault 10
                    , steps = findProperty u.stepsInt keyValueList |> toIntOrDefault 1
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#medium-text" ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty u.regex keyValueList
                    , maxLength = findProperty u.maxLength keyValueList |> toIntOrDefault 40
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-listbox" ->    
                BoundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty u.filtering keyValueList
                    , sorting = findProperty u.sorting keyValueList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#unbounded-listbox" ->
                UnboundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty u.filtering keyValueList
                    , sorting = findProperty u.sorting keyValueList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#range-slider" ->    
                RangeSliderWidget {
                    field = fieldModel
                    , minimum = findProperty u.minimumInt keyValueList |> toIntOrDefault 0
                    , maximum = findProperty u.maximumInt keyValueList |> toIntOrDefault 10
                    , steps = findProperty u.stepsInt keyValueList |> toIntOrDefault 1
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#date-viewer" ->    
                DateViewerWidget {
                    field = fieldModel
                    , format = findProperty u.format keyValueList |> Maybe.withDefault ""
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#long-text" ->
                LongTextWidget { field = fieldModel
                    , regex = findProperty u.regex keyValueList
                    , maxLength = findProperty u.maxLength keyValueList |> toIntOrDefault 80
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#text-area" ->
                TextAreaWidget {
                    field = fieldModel
                    , minLines = findProperty u.minLines keyValueList |> toIntOrDefault 3
                    , maxLines = findProperty u.maxLines keyValueList |> toIntOrDefault 10
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#markdown-area" ->    
                MarkdownAreaWidget {
                    field = fieldModel
                    , minLines = findProperty u.minLines keyValueList |> toIntOrDefault 3
                    , maxLines = findProperty u.maxLines keyValueList |> toIntOrDefault 10
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-radio" ->    
                BoundedRadioWidget {
                    field = fieldModel
                    , filtering = findProperty u.filtering keyValueList
                    , sorting = findProperty u.sorting keyValueList
                }
            _ ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty u.regex keyValueList
                    , maxLength = findProperty u.maxLength keyValueList |> toIntOrDefault 40
                }

tupleToTriple: String -> (String, String) -> Triple
tupleToTriple subject keyValue = {subject= subject, predicate= first(keyValue), object= second(keyValue)}

createListOfTriple: String -> List (String, String) -> List Triple
createListOfTriple subject keyValueList =
    List.map (tupleToTriple subject) keyValueList

fieldToProperties: FieldModel -> List (String, String)
fieldToProperties field =
    [(u.id, field.id), (u.label, field.label), (u.hint, field.hint), (u.prominence, field.prominence |> prominenceToString),(u.query, field.query), (u.style, field.style)]
 
widgetModelToPropertyList:  WidgetModel -> List (String, String)
widgetModelToPropertyList model =
    case model of
        CheckboxWidget widget ->
            fieldToProperties widget
        IncSpinnerWidget widget ->
            fieldToProperties widget.field ++ [(u.minimumInt, widget.minimum |> toString), (u.maximumInt, widget.maximum |> toString), (u.stepsInt, widget.steps |> toString)]
        MediumTextWidget widget ->
            fieldToProperties widget.field ++ [(u.maxLength, widget.maxLength |> toString), (u.regex, widget.regex |> Maybe.withDefault "")]
        BoundedListBoxWidget widget ->
            fieldToProperties widget.field ++ [(u.filtering, widget.filtering |> Maybe.withDefault ""), (u.sorting, widget.sorting |> Maybe.withDefault "")]
        UnboundedListBoxWidget widget ->
            fieldToProperties widget.field ++ [(u.filtering, widget.filtering |> Maybe.withDefault ""), (u.sorting, widget.sorting |> Maybe.withDefault "")]
        RangeSliderWidget widget ->
            fieldToProperties widget.field ++ [(u.minimumInt, widget.minimum |> toString), (u.maximumInt, widget.maximum |> toString), (u.stepsInt, widget.steps |> toString)]
        DateViewerWidget widget ->
            fieldToProperties widget.field ++ [(u.format, widget.format)]
        LongTextWidget widget ->
            fieldToProperties widget.field ++ [(u.maxLength, widget.maxLength |> toString), (u.regex, widget.regex |> Maybe.withDefault "")]
        TextAreaWidget widget ->
            fieldToProperties widget.field ++ [(u.minLines, widget.minLines |> toString), (u.maxLines, widget.maxLines |> toString)]
        MarkdownAreaWidget widget ->
            fieldToProperties widget.field ++ [(u.minLines, widget.minLines |> toString), (u.maxLines, widget.maxLines |> toString)]
        BoundedRadioWidget widget ->
            fieldToProperties widget.field ++ [(u.filtering, widget.filtering |> Maybe.withDefault ""), (u.sorting, widget.sorting |> Maybe.withDefault "")]

{-| Convert a widget model to a list of tuples.
-}
widgetModelToTriple:  String -> WidgetModel -> List Triple
widgetModelToTriple subject model =
    widgetModelToPropertyList model |> List.sort |> createListOfTriple subject
