module Bubblegum.Outlook exposing (vocabulary, createWidgetModel, widgetModelToTriples, createPanelModel, panelModelToTriples, createSectionModel, sectionModelToTriples, createDivisionModel, divisionModelToTriples, createMainSelection, mainSelectionToTriples)

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Create the model
@docs  vocabulary, createWidgetModel, widgetModelToTriples, createPanelModel, panelModelToTriples, createSectionModel, sectionModelToTriples, createDivisionModel, divisionModelToTriples

# Selection
@docs createMainSelection, mainSelectionToTriples
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
    , icon = "http://flarebyte.github.io/ontologies/2018/user-interface#icon"
    , validator = "http://flarebyte.github.io/ontologies/2018/user-interface#validator" --ex:email
    , helpValid = "http://flarebyte.github.io/ontologies/2018/user-interface#help-valid"
    , helpInvalid = "http://flarebyte.github.io/ontologies/2018/user-interface#help-invalid"
    , traits = "http://flarebyte.github.io/ontologies/2018/user-interface#traits" -- custom traits of the component
    , regex = "http://flarebyte.github.io/ontologies/2018/user-interface#regex"
    , placeholder = "http://flarebyte.github.io/ontologies/2018/user-interface#placeholder"
    , maxLength = "http://flarebyte.github.io/ontologies/2018/user-interface#maximum-length"
    , minLines = "http://flarebyte.github.io/ontologies/2018/user-interface#minimum-lines"
    , maxLines = "http://flarebyte.github.io/ontologies/2018/user-interface#maximum-lines"
    , languageSyntax = "http://flarebyte.github.io/ontologies/2018/user-interface#language-syntax"
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
    , boundedMultipleSelect = "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-multiple-select"
    , unboundedListbox = "http://flarebyte.github.io/ontologies/2018/user-interface#unbounded-listbox"
    , rangeSlider = "http://flarebyte.github.io/ontologies/2018/user-interface#range-slider"
    , dateViewer = "http://flarebyte.github.io/ontologies/2018/user-interface#date-viewer" 
    , longText = "http://flarebyte.github.io/ontologies/2018/user-interface#long-text"
    , textArea = "http://flarebyte.github.io/ontologies/2018/user-interface#text-area"
    , boundedRadio = "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-radio"
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
 
{-| The vocabulary (ontology) supported by the library
-}
vocabulary = u

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
    , label: String
    , hint: String
    , prominence: Prominence
    , position: Maybe Int
    , style: Maybe String
    , query: Maybe String
    , icon: Maybe String
    , validator: Maybe String
    , helpValid: Maybe String
    , helpInvalid: Maybe String
    , traits: Set String
    }

{-| A model for a single line of text.
-}
type alias TextModel = {
    field: FieldModel
    , regex: Maybe String
    , placeholder: Maybe String
    , maxLength: Maybe Int
    }

{-| A model for multiple lines of text.
-}
type alias TextAreaModel = {
    field: FieldModel
    , minLines: Maybe Int
    , maxLines: Maybe Int
    , placeholder: Maybe String
    , languageSyntax: Maybe String
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

## BoundedMultipleSelectWidget
A widget representing a bounded listbox with multiple selection

    BoundedMultipleSelectWidget linkedFieldModel

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

## BoundedRadioWidget
A widget representing a radio button.

    BoundedRadioWidget linkedFieldModel


-}
type WidgetModel =
    CheckboxWidget FieldModel
    | IncSpinnerWidget IncSpinnerModel
    | MediumTextWidget TextModel
    | BoundedListBoxWidget LinkedFieldModel
    | BoundedMultipleSelectWidget LinkedFieldModel
    | UnboundedListBoxWidget LinkedFieldModel
    | RangeSliderWidget IncSpinnerModel
    | DateViewerWidget DateViewerModel
    | LongTextWidget TextModel
    | TextAreaWidget TextAreaModel
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
        , from: Maybe Int
        , to: Maybe Int
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
        language: Maybe String
        , viewMode: Maybe String
        , currentView : Maybe CurrentView
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


toCurrentView:  String -> CurrentView
toCurrentView str =
    case str of
        "http://flarebyte.github.io/ontologies/2018/user-interface#search-view" ->
            SearchView
        "http://flarebyte.github.io/ontologies/2018/user-interface#edit-view" ->
            EditView
        anything ->
            SearchView

currentViewToString: CurrentView -> String
currentViewToString currentView =
    case currentView of
        SearchView -> u.searchView
        EditView -> u.editView
        
toSetOfStrings: Maybe String -> Set String
toSetOfStrings value =
    case value of
        Nothing ->Set.empty
        Just something -> String.split ";" something |> Set.fromList

fromSetOfStrings: Set String -> Maybe String
fromSetOfStrings value =
    case (Set.toList value) of
        [] -> Nothing
        list -> list |> String.join ";" |> Just

createFieldModel: String -> List Triple -> FieldModel
createFieldModel  subject keyValueList =
    {
    id = findProperty subject u.id keyValueList |> Maybe.withDefault ""
    , position = findProperty subject u.position keyValueList |> toMaybeInt
    , label= findProperty subject u.label keyValueList |> Maybe.withDefault ""
    , hint = findProperty subject u.hint keyValueList |> Maybe.withDefault ""
    , prominence = findProperty subject u.prominence keyValueList |> toProminence
    , style = findProperty subject u.style keyValueList
    , query = findProperty subject u.query keyValueList
    , icon = findProperty subject u.icon keyValueList
    , validator = findProperty subject u.validator keyValueList
    , helpValid = findProperty subject u.helpValid keyValueList
    , helpInvalid = findProperty subject u.helpInvalid keyValueList
    , traits = findProperty subject u.traits keyValueList |> toSetOfStrings
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
                    , minimum = findProperty subject u.minimumInt tripleList |> toMaybeInt
                    , maximum = findProperty subject u.maximumInt tripleList |> toMaybeInt
                    , steps = findProperty subject u.stepsInt tripleList |> toMaybeInt
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#medium-text" ->
                MediumTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , placeholder = findProperty subject u.placeholder tripleList
                    , maxLength = findProperty subject u.maxLength tripleList |> toMaybeInt
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-listbox" ->    
                BoundedListBoxWidget {
                    field = fieldModel
                    , filtering = findProperty subject u.filtering tripleList
                    , sorting = findProperty subject u.sorting tripleList
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-multiple-select" ->    
                BoundedMultipleSelectWidget {
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
                    , minimum = findProperty subject u.minimumInt tripleList |> toMaybeInt
                    , maximum = findProperty subject u.maximumInt tripleList |> toMaybeInt
                    , steps = findProperty subject u.stepsInt tripleList |> toMaybeInt
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#date-viewer" ->    
                DateViewerWidget {
                    field = fieldModel
                    , format = findProperty subject u.format tripleList |> Maybe.withDefault ""
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#long-text" ->
                LongTextWidget { field = fieldModel
                    , regex = findProperty subject u.regex tripleList
                    , placeholder = findProperty subject u.placeholder tripleList
                    , maxLength = findProperty subject u.maxLength tripleList |> toMaybeInt
                }
            "http://flarebyte.github.io/ontologies/2018/user-interface#text-area" ->
                TextAreaWidget {
                    field = fieldModel
                    , minLines = findProperty subject u.minLines tripleList |> toMaybeInt
                    , maxLines = findProperty subject u.maxLines tripleList |> toMaybeInt
                    , placeholder = findProperty subject u.placeholder tripleList
                    , languageSyntax = findProperty subject u.languageSyntax tripleList
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
                    , placeholder = findProperty subject u.placeholder tripleList
                    , maxLength = findProperty subject u.maxLength tripleList |> toMaybeInt
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
    [(u.id, Just field.id)
    ,(u.label, Just field.label)
    , (u.hint, Just field.hint)
    , (u.prominence , field.prominence |> prominenceToString |> Just)
    , (u.query, field.query)
    , (u.style, field.style)
    , (u.icon, field.icon)
    , (u.validator, field.validator)
    , (u.helpValid, field.helpValid)
    , (u.helpInvalid, field.helpInvalid)
    , (u.traits, field.traits |> fromSetOfStrings)
    ]

fieldToTriples:  FieldModel -> List Triple
fieldToTriples model = createListOfTriple model.id (model |> fieldToProperties)

toMaybeString: Maybe Int -> Maybe String
toMaybeString value = Maybe.map toString value

toIntOrZero: String -> Int
toIntOrZero value =  String.toInt value |> Result.withDefault 0

toMaybeInt: Maybe String -> Maybe Int
toMaybeInt value = Maybe.map toIntOrZero value

widgetModelToPropertyList:  WidgetModel -> List (String, Maybe String)
widgetModelToPropertyList model =
    case model of
        CheckboxWidget widget ->
            fieldToProperties widget ++ [(u.widgetType, Just u.checkbox)]
        IncSpinnerWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.incSpinner), (u.minimumInt, widget.minimum |> toMaybeString), (u.maximumInt, widget.maximum |> toMaybeString), (u.stepsInt, widget.steps |> toMaybeString)]
        MediumTextWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.mediumText), (u.maxLength, widget.maxLength |> toMaybeString), (u.regex, widget.regex), (u.placeholder, widget.placeholder)]
        BoundedListBoxWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.boundedListbox), (u.filtering, widget.filtering), (u.sorting, widget.sorting)]
        BoundedMultipleSelectWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.boundedMultipleSelect), (u.filtering, widget.filtering), (u.sorting, widget.sorting)]
        UnboundedListBoxWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.unboundedListbox), (u.filtering, widget.filtering), (u.sorting, widget.sorting)]
        RangeSliderWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.rangeSlider), (u.minimumInt, widget.minimum |> toMaybeString), (u.maximumInt, widget.maximum |> toMaybeString), (u.stepsInt, widget.steps |> toMaybeString)]
        DateViewerWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.dateViewer), (u.format, Just widget.format)]
        LongTextWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.longText), (u.maxLength, widget.maxLength |> toMaybeString), (u.regex, widget.regex), (u.placeholder, widget.placeholder)]
        TextAreaWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.textArea), (u.languageSyntax, widget.languageSyntax), (u.minLines, widget.minLines |> toMaybeString), (u.maxLines, widget.maxLines |> toMaybeString), (u.placeholder, widget.placeholder)]
        BoundedRadioWidget widget ->
            fieldToProperties widget.field ++ [(u.widgetType, Just u.boundedRadio), (u.filtering, widget.filtering), (u.sorting, widget.sorting)]

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
        BoundedMultipleSelectWidget widget ->
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
        language = findProperty u.mainSelection u.language tripleList
        , viewMode = findProperty u.mainSelection u.viewMode tripleList --ex: admin
        , currentView = findProperty u.mainSelection u.currentView tripleList |> Maybe.map toCurrentView
        , searchSelection = {
            divisionId = findProperty u.mainSelection u.divisionId tripleList
            , term = findProperty u.mainSelection u.searchTerm tripleList
            , from = findProperty u.mainSelection u.searchFrom tripleList |>toMaybeInt
            , to = findProperty u.mainSelection u.searchTo tripleList |>toMaybeInt
            , selected = findProperties u.mainSelection u.searchSelected tripleList
        }
        , editSelection = {
            divisionId = findProperty u.mainSelection u.divisionId tripleList
            , instanceId = findProperty u.mainSelection u.instanceId tripleList
        }
    }


{-| Converts a main selection model to a list of triples
-}
mainSelectionToTriples: MainSelection -> List Triple
mainSelectionToTriples model =
    [
        (u.language, model.language) 
        ,(u.viewMode, model.viewMode)
        ,(u.currentView, (model.currentView |> Maybe.map currentViewToString))
        ,(u.divisionId, model.searchSelection.divisionId)
        ,(u.searchTerm, model.searchSelection.term)
        ,(u.searchFrom, (model.searchSelection.from |> Maybe.map toString))
        ,(u.searchTo, (model.searchSelection.to |> Maybe.map toString))
        ,(u.searchSelected, (model.searchSelection.selected |> fromSetOfStrings)) 
    ] |> createListOfTriple u.mainSelection
   