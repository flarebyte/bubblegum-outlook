module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Outlook exposing (vocabulary, createWidgetModel, widgetModelToTriples, createPanelModel, panelModelToTriples, createSectionModel, sectionModelToTriples, createDivisionModel, divisionModelToTriples, createMainSelection, mainSelectionToTriples)
import Set exposing (Set, fromList, union, diff)

u = vocabulary

type alias Triple = { subject : String, predicate : String, object: String }

subjectId = "subject:1234"

t: String -> String -> Triple
t predicate object = { subject = subjectId, predicate = predicate, object = object }

tt: String -> String -> String -> Triple
tt subject predicate object = { subject = subject, predicate = predicate, object = object }

basic: List Triple
basic = t u.id  subjectId :: t u.label "some label" :: t u.hint "some hint" :: t u.prominence u.important :: t u.query "my query" :: []

createTextWidget: String -> String -> List Triple
createTextWidget panelId subj = tt subj u.id  subj :: tt subj u.widgetType u.mediumText :: tt subj u.partOfPanel panelId :: tt subj  u.label "some label" :: tt subj  u.hint "some hint" :: tt subj u.prominence u.important :: tt subj  u.query "my query" :: tt subj  u.style "my style" :: tt subj u.regex "[0-9]+"  :: tt subj u.maxLength "20"  :: []

createMetadata: String -> List Triple
createMetadata subj = tt subj u.id  subj :: tt subj  u.label "meta label" :: tt subj  u.hint "meta hint" :: tt subj u.prominence u.important :: tt subj  u.query "meta query" :: tt subj  u.style "meta style"  :: []

panelWidgets: String -> String -> String -> List Triple
panelWidgets divisionId sectionId panelId = (tt panelId u.partOfSection sectionId :: tt panelId u.partOfDivision divisionId :: createMetadata panelId) ++ (createTextWidget panelId "/1") ++ (createTextWidget panelId  "/2") ++ (createTextWidget panelId "/3") ++ (tt panelId u.id  panelId :: [])

normTriples: List Triple -> Set String
normTriples triples = List.map (\t -> t.subject ++ "|" ++ t.predicate ++ "|" ++ t.object) triples |> Set.fromList

errDiff: Set String -> Set String -> Set String
errDiff a b = diff (union a b) b

section: String -> String -> List Triple
section divisionId sectionId = (createMetadata sectionId) ++ (panelWidgets divisionId sectionId "/p1") ++ (panelWidgets divisionId sectionId "/p2")

division: String -> List Triple
division divisionId = (createMetadata divisionId) ++ (section divisionId "/s1") ++ (section divisionId "/s2")

myCheckbox = t u.widgetType u.checkbox :: basic
myIncSpinner = t u.widgetType u.incSpinner :: t u.maximumInt "15" :: basic
myMediumText = t u.widgetType u.mediumText :: t u.placeholder "placeholder" :: t u.icon "my icon" :: t u.traits "alpha":: t u.traits "beta" :: t u.regex "[0-9]+"  :: t u.maxLength "20" :: basic
myBoundedListbox = t u.widgetType u.boundedListbox :: t u.filtering "my filter" :: t u.sorting "asc" :: basic
myBoundedMultipleSelect = t u.widgetType u.boundedMultipleSelect :: t u.filtering "my filter" :: t u.sorting "asc" :: basic
myUnboundedListbox = t u.widgetType u.unboundedListbox :: t u.filtering "my filter" :: t u.sorting "asc" :: basic
myRangeSlider = t u.widgetType u.rangeSlider :: t u.minimumInt "7" :: t u.maximumInt "13" :: t u.stepsInt "3" :: basic
myDateViewer = t u.widgetType u.dateViewer :: t u.format "YYYY" :: t u.traits "alpha":: basic
myLongText = t u.widgetType u.longText :: t u.placeholder "placeholder" :: t u.helpValid "help valid" :: t u.validator "my validator" :: t u.regex "[0-9]+"  :: t u.maxLength "20" :: basic
myTextArea = t u.widgetType u.textArea :: t u.placeholder "placeholder" :: t u.languageSyntax "markdown" :: t u.helpInvalid "help invalid" :: t u.minLines "12" :: t u.maxLines "15" :: basic
myBoundedRadio =  t u.widgetType u.boundedRadio :: t u.filtering "my filter" :: t u.sorting "asc" :: basic

ts = tt u.mainSelection

mySearchSelection = ts u.language "en" :: ts u.viewMode "admin" :: ts u.currentView u.searchView :: ts u.divisionId "district13" :: ts u.searchTerm "search me" :: ts u.searchFrom "10" :: ts u.searchTo "20" :: ts u.searchSelected "id1":: ts u.searchSelected "id2" :: []

myEditSelection = ts u.language "en" :: ts u.viewMode "admin" :: ts u.currentView u.editView :: ts u.divisionId "district13" :: ts u.instanceId "/id123"  :: []


all : Test
all =
    describe "Bubblegum.Outlook"
        [ describe "createWidgetModel" <|
            [ test "create a checkbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId ( t "anything" "some noise" :: myCheckbox) |> widgetModelToTriples |> List.sortBy .predicate)
                    (myCheckbox |> List.sortBy .predicate)               
             
           ,  test "create a inc-spinner" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myIncSpinner |> widgetModelToTriples |> List.sortBy .predicate)
                    (myIncSpinner |> List.sortBy .predicate)               
              
           ,  test "create a medium-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myMediumText |> widgetModelToTriples |> List.sortBy .predicate)
                    (myMediumText |> List.sortBy .predicate)               
              
           ,  test "create a bounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myBoundedListbox |> widgetModelToTriples |> List.sortBy .predicate)
                    (myBoundedListbox |> List.sortBy .predicate)               
           
           ,  test "create a bounded-multipe-select" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myBoundedMultipleSelect |> widgetModelToTriples |> List.sortBy .predicate)
                    (myBoundedMultipleSelect |> List.sortBy .predicate)               
              
           , test "create a unbounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myUnboundedListbox |> widgetModelToTriples |> List.sortBy .predicate)
                    (myUnboundedListbox |> List.sortBy .predicate)               
              
           ,  test "create a range-slider" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myRangeSlider |> widgetModelToTriples |> List.sortBy .predicate)
                    (myRangeSlider |> List.sortBy .predicate)               
              
           ,  test "create a date-viewer" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myDateViewer |> widgetModelToTriples |> List.sortBy .predicate)
                    (myDateViewer |> List.sortBy .predicate)               
              
           ,  test "create a long-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myLongText |> widgetModelToTriples |> List.sortBy .predicate)
                   ( myLongText |> List.sortBy .predicate)               
            ,  test "create a text-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myTextArea |> widgetModelToTriples |> List.sortBy .predicate)
                   (myTextArea |> List.sortBy .predicate)               
           ,  test "create a bounded-radio" <|
                \() ->
                    Expect.equal
                    (createWidgetModel subjectId myBoundedRadio|> widgetModelToTriples |> List.sortBy .predicate)
                   (myBoundedRadio |> List.sortBy .predicate)               
             ]
        , describe "createPanelModel" <|
            [ test "create a panel model" <|
                \() ->
                    Expect.equal
                    (errDiff (panelWidgets "/d1" "/s1" "/p1" |> createPanelModel "/p1" |> panelModelToTriples |> normTriples) (panelWidgets "/d1" "/s1" "/p1" |> normTriples))
                    Set.empty
                , test "create a section model" <|
                \() ->
                    Expect.equal
                    (errDiff (section "/d1" "/s1" |> createSectionModel "/s1" |> sectionModelToTriples |> normTriples) (section "/d1" "/s1" |> normTriples))
                    Set.empty
                , test "create a division model" <|
                \() ->
                    Expect.equal
                    (errDiff (division "/d1" |> createDivisionModel "/d1" |> divisionModelToTriples |> normTriples) (division "/d1" |> normTriples))
                    Set.empty
            ]
        , describe "main selection" <|
            [ test "create  search selection" <|
                \() ->
                    Expect.equal
                    (errDiff (mySearchSelection |> createMainSelection |> mainSelectionToTriples |> normTriples) (mySearchSelection |> normTriples))
                    Set.empty
               , test "create  edit selection" <|              
                \() ->
                    Expect.equal
                    (errDiff (myEditSelection |> createMainSelection |> mainSelectionToTriples |> normTriples) (myEditSelection |> normTriples))
                    Set.empty
            ]                 
        ]
