module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Outlook exposing (createWidgetModel, widgetModelToTriple)
import Set exposing (Set, fromList)

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
    , boundedRadio = "http://flarebyte.github.io/ontologies/2018/user-interface#bounded-radio"
 }

type alias Triple = { subject : String, predicate : String, object: String }

subjectId = "subject:1234"

t: String -> String -> Triple
t predicate object = { subject = subjectId, predicate = predicate, object = object }

basic: List Triple
basic = t u.id  "id123" :: t u.label "some label" :: t u.hint "some hint" :: t u.prominence "important" :: t u.query "my query" :: []

expectedBasic = t u.style "" :: basic

all : Test
all =
    describe "Bubblegum.Outlook"
        [ describe "createWidgetModel" <|
            [ test "create a checkbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ( t u.widgetType u.checkbox :: t "anything" "some noise" :: basic) |> widgetModelToTriple subjectId)
                    (expectedBasic |> List.sortBy .predicate)               
             
           ,  test "create a inc-spinner" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.incSpinner :: t u.maximumInt "15" :: basic) |> widgetModelToTriple subjectId)
                   (t u.maximumInt "15" :: t u.minimumInt "0" :: t u.stepsInt "1" :: expectedBasic |> List.sortBy .predicate)               
              
           ,  test "create a medium-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.mediumText :: t u.regex "[0-9]+"  :: t u.maxLength "20" :: basic) |> widgetModelToTriple subjectId)
                   (t u.regex "[0-9]+" :: t u.maxLength "20" :: expectedBasic |> List.sortBy .predicate)               
              
           ,  test "create a bounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.boundedListbox :: t u.filtering "my filter" :: t u.sorting "asc" :: basic) |> widgetModelToTriple subjectId)
                   (t u.filtering "my filter" :: t u.sorting "asc" :: expectedBasic |> List.sortBy .predicate)               
              
           , test "create a unbounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.unboundedListbox :: t u.filtering "my filter" :: t u.sorting "asc" :: basic) |> widgetModelToTriple subjectId)
                   (t u.filtering "my filter" :: t u.sorting "asc" :: expectedBasic |> List.sortBy .predicate)               
              
           ,  test "create a range-slider" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.rangeSlider :: t u.minimumInt "7" :: t u.maximumInt "13" :: t u.stepsInt "3" :: basic) |> widgetModelToTriple subjectId)
                   (t u.minimumInt "7" :: t u.maximumInt "13" :: t u.stepsInt "3" :: expectedBasic |> List.sortBy .predicate)               
              
           ,  test "create a date-viewer" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.dateViewer :: t u.format "YYYY" :: basic) |> widgetModelToTriple subjectId)
                    (t u.format "YYYY" :: expectedBasic |> List.sortBy .predicate)               
              
           ,  test "create a long-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.longText :: t u.regex "[0-9]+"  :: t u.maxLength "20" :: basic) |> widgetModelToTriple subjectId)
                   ( t u.regex "[0-9]+"  :: t u.maxLength "20" :: expectedBasic |> List.sortBy .predicate)               
            ,  test "create a text-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.textArea :: t u.minLines "12" :: t u.maxLines "15" :: basic) |> widgetModelToTriple subjectId)
                   (t u.minLines "12" :: t u.maxLines "15" :: expectedBasic |> List.sortBy .predicate)               
            ,  test "create a markdown-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.markdownArea :: t u.minLines "12" :: t u.maxLines "15" :: basic) |> widgetModelToTriple subjectId)
                   (t u.minLines "12" :: t u.maxLines "15" :: expectedBasic |> List.sortBy .predicate)               
           ,  test "create a bounded-radio" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (t u.widgetType u.boundedRadio :: t u.filtering "my filter" :: t u.sorting "asc" :: basic) |> widgetModelToTriple subjectId)
                   (t u.filtering "my filter" :: t u.sorting "asc" :: expectedBasic |> List.sortBy .predicate)               
             ]
        ]
