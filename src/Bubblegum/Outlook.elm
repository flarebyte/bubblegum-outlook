module Bubblegum.Outlook exposing (createWidgetModel, widgetModelToTriples, createPanelModel, panelModelToTriples, createSectionModel, sectionModelToTriples, createDivisionModel, divisionModelToTriples)

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Create the model
@docs  createWidgetModel, widgetModelToTriples, createPanelModel, panelModelToTriples, createSectionModel, sectionModelToTriples, createDivisionModel, divisionModelToTriples

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
    , partOfPanel = "http://flarebyte.github.io/ontologies/2018/user-interface#part-of-panel"
    , partOfSection = "http://flarebyte.github.io/ontologies/2018/user-interface#part-of-section"
    , partOfDivision = "http://flarebyte.github.io/ontologies/2018/user-interface#part-of-division"
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
        , widgets: List WidgetModel
    }

{-| A model for a section containing several panels.
-}
type alias SectionModel = {
        field: FieldModel
        , panels: List PanelModel
    }

{-| A model for a division containing several sections.
-}
type alias DivisionModel = {
        field: FieldModel
        , sections: List SectionModel
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


createFieldModel: String -> List Triple -> FieldModel
createFieldModel  subject keyValueList =
    {
    id = findProperty subject u.id keyValueList |> Maybe.withDefault ""
    , position = findProperty subject u.position keyValueList |> toIntOrDefault 0
    , label= findProperty subject u.label keyValueList |> Maybe.withDefault ""
    , hint = findProperty subject u.hint keyValueList |> Maybe.withDefault ""
    , prominence = findProperty subject u.prominence keyValueList |> toProminence
    , style = findProperty subject u.style keyValueList |> Maybe.withDefault ""
    , query = findProperty subject u.query keyValueList |> Maybe.withDefault ""
    }
 

{-| Create a widget model from a list of tuples.
-}
createWidgetModel: String -> List Triple -> WidgetModel
createWidgetModel subject tripleList =
    let
        widgetType = findProperty subject u.widgetType tripleList |> Maybe.withDefault "long-text"
        fieldModel = createFieldModel subject tripleList
    in
        case widgetType of
            "http://flarebyte.github.io/ontologies/2018/user-interface#checkbox" ->
                CheckboxWidget fieldModel
            "http://flarebyte.github.io/ontologies/2018/user-interface#inc-spinner" ->
                IncSpinnerWidget {
                    field = fieldModel
                    , minimum = findProperty subject u.minimumInt tripleList |> toIntOrDefault 0
                    , maximum = findProperty subject u.maximumInt tripleList |> toIntOrDefault 10
                    , steps = findProperty subject u.stepsInt tripleList |> toIntOrDefault 1
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#medium-text" ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , maxLength = findProperty subject u.maxLength tripleList |> toIntOrDefault 40
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-listbox" ->    
                BoundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty subject u.filtering tripleList
                    , sorting = findProperty subject u.sorting tripleList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#unbounded-listbox" ->
                UnboundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty subject u.filtering tripleList
                    , sorting = findProperty subject u.sorting tripleList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#range-slider" ->    
                RangeSliderWidget {
                    field = fieldModel
                    , minimum = findProperty subject u.minimumInt tripleList |> toIntOrDefault 0
                    , maximum = findProperty subject u.maximumInt tripleList |> toIntOrDefault 10
                    , steps = findProperty subject u.stepsInt tripleList |> toIntOrDefault 1
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#date-viewer" ->    
                DateViewerWidget {
                    field = fieldModel
                    , format = findProperty subject u.format tripleList |> Maybe.withDefault ""
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#long-text" ->
                LongTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , maxLength = findProperty subject u.maxLength tripleList |> toIntOrDefault 80
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#text-area" ->
                TextAreaWidget {
                    field = fieldModel
                    , minLines = findProperty subject u.minLines tripleList |> toIntOrDefault 3
                    , maxLines = findProperty subject u.maxLines tripleList |> toIntOrDefault 10
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#markdown-area" ->    
                MarkdownAreaWidget {
                    field = fieldModel
                    , minLines = findProperty subject u.minLines tripleList |> toIntOrDefault 3
                    , maxLines = findProperty subject u.maxLines tripleList |> toIntOrDefault 10
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-radio" ->    
                BoundedRadioWidget {
                    field = fieldModel
                    , filtering = findProperty subject u.filtering tripleList
                    , sorting = findProperty subject u.sorting tripleList
                }
            _ ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , maxLength = findProperty subject u.maxLength tripleList |> toIntOrDefault 40
                }

tupleToTriple: String -> (String, String) -> Triple
tupleToTriple subject keyValue = {subject= subject, predicate= first(keyValue), object= second(keyValue)}

createListOfTriple: String -> List (String, String) -> List Triple
createListOfTriple subject keyValueList =
    List.map (tupleToTriple subject) keyValueList

fieldToProperties: FieldModel -> List (String, String)
fieldToProperties field =
    [(u.id, field.id), (u.label, field.label), (u.hint, field.hint), (u.prominence, field.prominence |> prominenceToString),(u.query, field.query), (u.style, field.style)]

fieldToTriples:  FieldModel -> List Triple
fieldToTriples model = createListOfTriple model.id (model |> fieldToProperties)

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

widgetModelToFieldModel:  WidgetModel -> FieldModel
widgetModelToFieldModel model =
    case model of
        CheckboxWidget widget ->
            widget
        IncSpinnerWidget widget ->
            widget.field
        MediumTextWidget widget ->
           widget.field
        BoundedListBoxWidget widget ->
           widget.field
        UnboundedListBoxWidget widget ->
           widget.field
        RangeSliderWidget widget ->
           widget.field
        DateViewerWidget widget ->
           widget.field
        LongTextWidget widget ->
           widget.field
        TextAreaWidget widget ->
           widget.field
        MarkdownAreaWidget widget ->
           widget.field
        BoundedRadioWidget widget ->
           widget.field

{-| Convert a widget model to a list of tuples.
-}
widgetModelToTriples: WidgetModel -> List Triple
widgetModelToTriples model =
    widgetModelToPropertyList model |> createListOfTriple (model |> widgetModelToFieldModel |> .id)

unique: List String -> List String
unique list = list |> Set.fromList |> Set.toList

findWidgetsInPanel: String -> List Triple -> List String
findWidgetsInPanel panelId list =
    List.filter (\t -> t.predicate == u.partOfPanel && t.object == panelId) list |> List.map .subject |> unique

findPanelsInSection: String -> List Triple -> List String
findPanelsInSection sectionId list =
    List.filter (\t -> t.predicate == u.partOfSection && t.object == sectionId) list |> List.map .subject |> unique

findSectionsInDivision: String -> List Triple -> List String
findSectionsInDivision divisionId list =
    List.filter (\t -> t.predicate == u.partOfDivision && t.object == divisionId) list |> List.map .subject |> unique

{-| Creates a panel model
-}
createPanelModel: String -> List Triple -> PanelModel
createPanelModel panelId tripleList =
    {
        field = createFieldModel panelId tripleList
        , widgets = List.map (\w -> createWidgetModel w tripleList) (findWidgetsInPanel panelId tripleList)
    }

{-| Converts a panel model to a list of triples
-}
panelModelToTriples: PanelModel -> List Triple
panelModelToTriples model =
    fieldToTriples model.field ++ (List.map widgetModelToTriples model.widgets |> List.concat)
       
{-| Creates a section model
-}
createSectionModel: String -> List Triple -> SectionModel
createSectionModel sectionId tripleList =
    {
        field = createFieldModel sectionId tripleList
        , panels = List.map (\p -> createPanelModel p tripleList) (findPanelsInSection sectionId tripleList)
    }

{-| Converts a section model to a list of triples
-}
sectionModelToTriples: SectionModel -> List Triple
sectionModelToTriples model =
    fieldToTriples model.field ++ (List.map panelModelToTriples model.panels |> List.concat)

{-| Creates a division model
-}
createDivisionModel: String -> List Triple -> DivisionModel
createDivisionModel divisionId tripleList =
    {
        field = createFieldModel divisionId tripleList
        , sections = List.map (\s -> createSectionModel s tripleList) (findSectionsInDivision divisionId tripleList)
    }

{-| Converts a division model to a list of triples
-}
divisionModelToTriples: DivisionModel -> List Triple
divisionModelToTriples model =
    fieldToTriples model.field ++ (List.map sectionModelToTriples model.sections |> List.concat)