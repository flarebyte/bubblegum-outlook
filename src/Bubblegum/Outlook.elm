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
import Result

type Prominence = Hidden | ReadOnly | Visible | Required| Important

type CurrentView = SearchView | EditView

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
    , hidden = "http://flarebyte.github.io/ontologies/2018/user-interface#hidden"
    , readOnly = "http://flarebyte.github.io/ontologies/2018/user-interface#read-only"
    , visible = "http://flarebyte.github.io/ontologies/2018/user-interface#visible"
    , required = "http://flarebyte.github.io/ontologies/2018/user-interface#required"
    , important = "http://flarebyte.github.io/ontologies/2018/user-interface#important"
    , mainSelection = "http://flarebyte.github.io/ontologies/2018/user-interface#main-selection"
    , language = "http://flarebyte.github.io/ontologies/2018/user-interface#language"
    , viewMode = "http://flarebyte.github.io/ontologies/2018/user-interface#view-mode" --ex: /admin
    , divisionId = "http://flarebyte.github.io/ontologies/2018/user-interface#division-id" --ex: /character
    , instanceId = "http://flarebyte.github.io/ontologies/2018/user-interface#instance-id"
    , searchTerm = "http://flarebyte.github.io/ontologies/2018/user-interface#search-term"
    , searchFrom = "http://flarebyte.github.io/ontologies/2018/user-interface#search-from"
    , searchTo = "http://flarebyte.github.io/ontologies/2018/user-interface#search-to"
    , searchSelected = "http://flarebyte.github.io/ontologies/2018/user-interface#search-selected"
    , currentView = "http://flarebyte.github.io/ontologies/2018/user-interface#current-view"
    , searchView = "http://flarebyte.github.io/ontologies/2018/user-interface#search-view"
    , editView = "http://flarebyte.github.io/ontologies/2018/user-interface#edit-view"
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
    , style: Maybe String
    , query: Maybe String
    }

{-| A model for a single line of text.
-}
type alias TextModel = {
    field: FieldModel
    , regex: Maybe String
    , maxLength: Maybe Int
    }

{-| A model for multiple lines of text.
-}
type alias TextAreaModel = {
    field: FieldModel
    , minLines: Maybe Int
    , maxLines: Maybe Int
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
    , minimum: Maybe Int
    , maximum: Maybe Int
    , steps: Maybe Int
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

{-| A model for the search selection.
-}
type alias SearchSelection = {
        divisionId: Maybe String
        , term: Maybe String
        , from: Int
        , to: Int
        , selected: Set String -- unique instance ids
    }

{-| A model for the edit selection.
-}
type alias EditSelection = {
        divisionId: Maybe String
        , instanceId: Maybe String
     }    

{-| A model for the main selection.
-}
type alias MainSelection = {
        language: String
        , viewMode: String
        , currentView : CurrentView
        , searchSelection : SearchSelection
        , editSelection : EditSelection
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

findProperties: String -> String -> List Triple -> Set String
findProperties subject predicate list =
    List.filter (\t -> t.subject == subject && t.predicate == predicate) list |> List.map .object |> Set.fromList

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
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#visible" ->
            Visible
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#hidden" ->
            Hidden
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#read-only" ->
            ReadOnly
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#required" ->
            Required
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#important" ->
            Important
        Just anything ->
            Visible

prominenceToString: Prominence -> String
prominenceToString prominence =
    case prominence of
        Hidden -> u.hidden
        ReadOnly -> u.readOnly
        Visible -> u.visible
        Required -> u.required
        Important -> u.important


toCurrentView:  Maybe String -> CurrentView
toCurrentView str =
    case str of
        Nothing ->
            SearchView
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#search-view" ->
            SearchView
        Just "http://flarebyte.github.io/ontologies/2018/user-interface#edit-view" ->
            EditView
        Just anything ->
            SearchView

currentViewToString: CurrentView -> String
currentViewToString currentView =
    case currentView of
        SearchView -> u.searchView
        EditView -> u.editView
        
       
createFieldModel: String -> List Triple -> FieldModel
createFieldModel  subject keyValueList =
    {
    id = findProperty subject u.id keyValueList |> Maybe.withDefault ""
    , position = findProperty subject u.position keyValueList |> toIntOrDefault 0
    , label= findProperty subject u.label keyValueList |> Maybe.withDefault ""
    , hint = findProperty subject u.hint keyValueList |> Maybe.withDefault ""
    , prominence = findProperty subject u.prominence keyValueList |> toProminence
    , style = findProperty subject u.style keyValueList
    , query = findProperty subject u.query keyValueList
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
                    , minimum = findProperty subject u.minimumInt tripleList
                    , maximum = findProperty subject u.maximumInt tripleList
                    , steps = findProperty subject u.stepsInt tripleList

            "http://flarebyte.github.io/ontologies/2018/user-interface#medium-text" ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , maxLength = findProperty subject u.maxLength tripleList
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
                    , minimum = findProperty subject u.minimumInt tripleList
                    , maximum = findProperty subject u.maximumInt tripleList
                    , steps = findProperty subject u.stepsInt tripleList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#date-viewer" ->    
                DateViewerWidget {
                    field = fieldModel
                    , format = findProperty subject u.format tripleList |> Maybe.withDefault ""
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#long-text" ->
                LongTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , maxLength = findProperty subject u.maxLength tripleList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#text-area" ->
                TextAreaWidget {
                    field = fieldModel
                    , minLines = findProperty subject u.minLines tripleList
                    , maxLines = findProperty subject u.maxLines tripleList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#markdown-area" ->    
                MarkdownAreaWidget {
                    field = fieldModel
                    , minLines = findProperty subject u.minLines tripleList
                    , maxLines = findProperty subject u.maxLines tripleList
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
                    , maxLength = findProperty subject u.maxLength tripleList
                }

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
 
fieldToProperties: FieldModel -> List (String, Maybe String)
fieldToProperties field =
    [(u.id, Just field.id), (u.label, Just field.label), (u.hint, Just field.hint), (u.prominence, field.prominence |> prominenceToString |> Just),(u.query, Just field.query), (u.style, Just field.style)]

fieldToTriples:  FieldModel -> List Triple
fieldToTriples model = createListOfTriple model.id (model |> fieldToProperties)

toMaybeString: Maybe Int -> Maybe String
toMaybeString value = Maybe.map toString

widgetModelToPropertyList:  WidgetModel -> List (String, Maybe String)
widgetModelToPropertyList model =
    case model of
        CheckboxWidget widget ->
            fieldToProperties widget
        IncSpinnerWidget widget ->
            fieldToProperties widget.field ++ [(u.minimumInt, widget.minimum |> toMaybeString), (u.maximumInt, widget.maximum |> toMaybeString), (u.stepsInt, widget.steps |> toMaybeString)]
        MediumTextWidget widget ->
            fieldToProperties widget.field ++ [(u.maxLength, widget.maxLength |> toMaybeString), (u.regex, widget.regex)]
        BoundedListBoxWidget widget ->
            fieldToProperties widget.field ++ [(u.filtering, widget.filtering), (u.sorting, widget.sorting)]
        UnboundedListBoxWidget widget ->
            fieldToProperties widget.field ++ [(u.filtering, widget.filtering), (u.sorting, widget.sorting)]
        RangeSliderWidget widget ->
            fieldToProperties widget.field ++ [(u.minimumInt, widget.minimum |> toMaybeString), (u.maximumInt, widget.maximum |> toMaybeString), (u.stepsInt, widget.steps |> toMaybeString)]
        DateViewerWidget widget ->
            fieldToProperties widget.field ++ [(u.format, widget.format)]
        LongTextWidget widget ->
            fieldToProperties widget.field ++ [(u.maxLength, widget.maxLength |> toMaybeString), (u.regex, widget.regex)]
        TextAreaWidget widget ->
            fieldToProperties widget.field ++ [(u.minLines, widget.minLines |> toMaybeString), (u.maxLines, widget.maxLines |> toMaybeString)]
        MarkdownAreaWidget widget ->
            fieldToProperties widget.field ++ [(u.minLines, widget.minLines |> toMaybeString), (u.maxLines, widget.maxLines |> toMaybeString)]
        BoundedRadioWidget widget ->
            fieldToProperties widget.field ++ [(u.filtering, widget.filtering), (u.sorting, widget.sorting)]

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


{-| Create the main selection
-}
createMainSelection: List Triple -> MainSelection
createMainSelection tripleList =
    {
        language = findProperty u.mainSelection u.language tripleList |> Maybe.withDefault ""
        , viewMode = findProperty u.mainSelection u.viewMode tripleList |> Maybe.withDefault ""
        , currentView = findProperty u.mainSelection u.currentView tripleList |> toCurrentView
        , searchSelection = {
            divisionId = findProperty u.mainSelection u.divisionId tripleList
            , term = findProperty u.mainSelection u.searchTerm tripleList
            , from = findProperty u.mainSelection u.searchFrom tripleList |> toIntOrDefault 0
            , to = findProperty u.mainSelection u.searchTo tripleList |> toIntOrDefault 1000
            , selected = findProperties u.mainSelection u.searchSelected tripleList
        }
        , editSelection = {
            divisionId = findProperty u.mainSelection u.divisionId tripleList
            , instanceId = findProperty u.mainSelection u.instanceId tripleList
        }
    }

-- select = Triple u.mainSelection

-- {-| Converts a main selection model to a list of triples
-- -}
-- mainSelectionToTriples: MainSelection -> List Triple
-- mainSelectionToTriples model =
--     select u.language model.language 
--     :: select u.viewMode model.viewMode
--     :: select u.currentView (model.currentView |> currentViewToString)
--     -- :: select u.divisionId model.searchSelection.divisionId
--     :: select u.searchFrom (model.searchSelection.from |> toString)
--     :: select u.searchTo (model.searchSelection.to |> toString)  
--     :: []
   