module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.WidgetModel as WidgetModel
import Bubblegum.PanelModel as PanelModel
import Bubblegum.SectionModel as SectionModel
import Bubblegum.DivisionModel as DivisionModel
import Bubblegum.DivisionSelection as DivisionSelection  
import Bubblegum.Outlook exposing (render)
import Set exposing (Set, fromList, union, diff)
import Bubblegum.Vocabulary exposing(..)
import Bubblegum.Bulma.TextWidget exposing (fromModel)

type alias Triple = { subject : String, predicate : String, object: String }

subjectId = "subject:1234"

t: String -> String -> Triple
t predicate object = { subject = subjectId, predicate = predicate, object = object }

tt: String -> String -> String -> Triple
tt subject predicate object = { subject = subject, predicate = predicate, object = object }


setting1 = tt "/sett1" ui_id "/sett1" :: tt "/sett1" ui_settingKey "/sorting" :: tt "/sett1" ui_settingValue "asc":: tt "/sett1" ui_settingFacet "alpha" :: tt "/sett1" ui_settingOfField subjectId :: []
setting2 = tt "/sett2" ui_id "/sett2" :: tt "/sett2" ui_settingKey "/filtering" :: tt "/sett2" ui_settingValue ">=10" :: tt "/sett2" ui_settingValue "<=200" ::tt "/sett2" ui_settingOfField subjectId :: []
mySettings = setting1 ++ setting2

basic: List Triple
basic = t ui_id  subjectId :: t ui_label "some label" :: t ui_hint "some hint" :: t ui_prominence "important" :: [] ++ mySettings

createTextWidget: String -> String -> List Triple
createTextWidget panelId subj = tt subj ui_id  subj :: tt subj ui_partOfPanel panelId :: tt subj  ui_label "some label" :: tt subj  ui_hint "some hint" :: tt subj ui_prominence "important" :: tt subj  ui_style "my style" :: tt subj ui_regex "[0-9]+"  :: tt subj ui_maxLength "20"  :: []

createMetadata: String -> List Triple
createMetadata subj = tt subj ui_id  subj :: tt subj  ui_label "meta label" :: tt subj  ui_hint "meta hint" :: tt subj ui_prominence "important" :: tt subj  ui_style "meta style"  :: []

panelWidgets: String -> String -> String -> List Triple
panelWidgets divisionId sectionId panelId = (tt panelId ui_partOfSection sectionId :: tt panelId ui_partOfDivision divisionId :: createMetadata panelId) ++ (createTextWidget panelId "/1") ++ (createTextWidget panelId  "/2") ++ (createTextWidget panelId "/3") ++ (tt panelId ui_id  panelId :: [])

normTriples: List Triple -> Set String
normTriples triples = List.map (\t -> t.subject ++ "|" ++ t.predicate ++ "|" ++ t.object) triples |> Set.fromList

errDiff: Set String -> Set String -> Set String
errDiff a b = diff (union a b) b

section: String -> String -> List Triple
section divisionId sectionId = (createMetadata sectionId) ++ (panelWidgets divisionId sectionId "/p1") ++ (panelWidgets divisionId sectionId "/p2")

division: String -> List Triple
division divisionId = (createMetadata divisionId) ++ (section divisionId "/s1") ++ (section divisionId "/s2")

myIncSpinner =  t ui_trait "spinner" :: basic
myMediumText = t ui_placeholder "placeholder" :: t ui_icon "my icon" :: t ui_trait "alpha":: t ui_trait "beta" :: t ui_regex "[0-9]+"  :: t ui_maxLength "20" :: basic
myBoundedListbox =  t ui_trait "bounded" :: t ui_trait "listbox" :: basic
myUnboundedListbox =  t ui_trait "unbounded" :: t ui_trait "listbox" :: basic
myRangeSlider = t ui_trait "range-slider" :: basic
myTextArea = t ui_trait "text-area" :: t ui_placeholder "placeholder" :: t ui_languageSyntax "markdown" :: t ui_helpInvalid "help invalid" :: t ui_minLines "12" :: t ui_maxLines "15" :: basic

ts = tt ui_mainSelection

mySearchSelection = ts ui_language "en" :: ts ui_divisionId "district13" :: ts ui_searchTerm "search me" :: ts ui_searchFrom "10" :: ts ui_searchTo "20" :: ts ui_searchSelected "id1":: ts ui_searchSelected "id2" :: []

myEditSelection = ts ui_language "en" :: ts ui_viewMode "admin" :: ts ui_divisionId "district13" :: ts ui_searchSelected "/id123"  :: []

tripleEqual: List Triple -> List Triple -> Expect.Expectation
tripleEqual expected actual =
     errDiff (expected |> normTriples) (actual |> normTriples) |> Expect.equal Set.empty

all : Test
all =
    describe "Bubblegum.Outlook"
        [ describe "createWidgetModel" <|
            [ test "create a inc-spinner" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myIncSpinner |> WidgetModel.toTriples)
                    (myIncSpinner)               
              
           ,  test "create a medium-text" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myMediumText |> WidgetModel.toTriples)
                    (myMediumText)               
              
           ,  test "create a bounded-listbox" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myBoundedListbox |> WidgetModel.toTriples)
                    (myBoundedListbox)               
           
           ,  test "create a unbounded-listbox" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myUnboundedListbox |> WidgetModel.toTriples)
                    (myUnboundedListbox)               
              
           ,  test "create a range-slider" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myRangeSlider |> WidgetModel.toTriples)
                    (myRangeSlider)               
              
           ,  test "create a text-area" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myTextArea |> WidgetModel.toTriples)
                   (myTextArea)               
            ]
        , describe "createPanelModel" <|
            [ test "create a panel model" <|
                \() ->
                    tripleEqual
                    (panelWidgets "/d1" "/s1" "/p1" |> PanelModel.fromTriples "/p1" |> PanelModel.toTriples)
                    (panelWidgets "/d1" "/s1" "/p1")
                , test "create a section model" <|
                \() ->
                    tripleEqual
                    ( section "/d1" "/s1" |> SectionModel.fromTriples "/s1" |> SectionModel.toTriples)
                    (section "/d1" "/s1")
                , test "create a division model" <|
                \() ->
                    tripleEqual
                    (division "/d1" |> DivisionModel.fromTriples "/d1" |> DivisionModel.toTriples) 
                    (division "/d1")
            ]
        , describe "main selection" <|
            [ test "create  search selection" <|
                \() ->
                    tripleEqual
                    (mySearchSelection |> DivisionSelection.fromTriples |> DivisionSelection.toTriples)
                    (mySearchSelection)
               , test "create  edit selection" <|              
                \() ->
                    tripleEqual
                    (myEditSelection |> DivisionSelection.fromTriples |> DivisionSelection.toTriples) 
                    (myEditSelection)
            ]                 
        ]
