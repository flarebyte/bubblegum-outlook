module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Outlook exposing (createWidgetModel, widgetModelToPropertyList)

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

basic: List (String, String)
basic = [(u.id, "id123"), (u.label, "some label"),(u.hint, "some hint"),(u.prominence, "important"),(u.query, "my query")]
expectedBasic = (u.style,"") :: basic

all : Test
all =
    describe "Bubblegum.Outlook"
        [ describe "createWidgetModel" <|
            [ test "create a checkbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.checkbox):: ("anything", "some noise") :: basic) |> widgetModelToPropertyList)
                    (expectedBasic |> List.sort)               
             
           ,  test "create a inc-spinner" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.incSpinner) :: (u.maximumInt, "15") :: basic) |> widgetModelToPropertyList)
                   ((u.maximumInt,"15") :: (u.minimumInt,"0") :: (u.stepsInt,"1") :: expectedBasic |> List.sort)               
              
           ,  test "create a medium-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.mediumText) :: (u.regex, "[0-9]+")  :: (u.maxLength, "20") :: basic) |> widgetModelToPropertyList)
                   ((u.regex, "[0-9]+") :: (u.maxLength, "20") :: expectedBasic |> List.sort)               
              
           ,  test "create a bounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.boundedListbox) :: (u.filtering, "my filter") :: (u.sorting, "asc") :: basic) |> widgetModelToPropertyList)
                   ((u.filtering, "my filter") :: (u.sorting, "asc") :: expectedBasic |> List.sort)               
              
           , test "create a unbounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.unboundedListbox) :: (u.filtering, "my filter") :: (u.sorting, "asc") :: basic) |> widgetModelToPropertyList)
                   ((u.filtering, "my filter") :: (u.sorting, "asc") :: expectedBasic |> List.sort)               
              
           ,  test "create a range-slider" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.rangeSlider) :: (u.minimumInt, "7") :: (u.maximumInt, "13") :: (u.stepsInt, "3") :: basic) |> widgetModelToPropertyList)
                   ((u.minimumInt, "7") :: (u.maximumInt, "13") :: (u.stepsInt, "3") :: expectedBasic |> List.sort)               
              
           ,  test "create a date-viewer" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.dateViewer) :: (u.format, "YYYY") :: basic) |> widgetModelToPropertyList)
                    ((u.format, "YYYY") :: expectedBasic |> List.sort)               
              
           ,  test "create a long-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.longText) :: (u.regex, "[0-9]+")  :: (u.maxLength, "20") :: basic) |> widgetModelToPropertyList)
                   ((u.regex, "[0-9]+")  :: (u.maxLength, "20") :: expectedBasic |> List.sort)               
            ,  test "create a text-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.textArea) :: (u.minLines, "12") :: (u.maxLines, "15") :: basic) |> widgetModelToPropertyList)
                   ((u.minLines, "12") :: (u.maxLines, "15") :: expectedBasic |> List.sort)               
            ,  test "create a markdown-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.markdownArea) :: (u.minLines, "12") :: (u.maxLines, "15") :: basic) |> widgetModelToPropertyList)
                   ((u.minLines, "12") :: (u.maxLines, "15") :: expectedBasic |> List.sort)               
           ,  test "create a bounded-radio" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, u.boundedRadio) :: (u.filtering, "my filter") :: (u.sorting, "asc") :: basic) |> widgetModelToPropertyList)
                   ((u.filtering, "my filter") :: (u.sorting, "asc") :: expectedBasic |> List.sort)               
             ]
        ]
