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
                    (createWidgetModel ((u.widgetType, "checkbox"):: ("anything", "some noise") :: basic) |> widgetModelToPropertyList)
                    (expectedBasic |> List.sort)               
             
           ,  test "create a inc-spinner" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "inc-spinner") :: (u.maximumInt, "15") :: basic) |> widgetModelToPropertyList)
                   ((u.maximumInt,"15") :: (u.minimumInt,"0") :: (u.stepsInt,"1") :: expectedBasic |> List.sort)               
              
           ,  test "create a medium-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "medium-text") :: (u.regex, "[0-9]+")  :: (u.maxLength, "20") :: basic) |> widgetModelToPropertyList)
                   ((u.regex, "[0-9]+") :: (u.maxLength, "20") :: expectedBasic |> List.sort)               
              
           ,  test "create a bounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "bounded-listbox") :: (u.filtering, "my filter") :: (u.sorting, "asc") :: basic) |> widgetModelToPropertyList)
                   ((u.filtering, "my filter") :: (u.sorting, "asc") :: expectedBasic |> List.sort)               
              
           , test "create a unbounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "unbounded-listbox") :: (u.filtering, "my filter") :: (u.sorting, "asc") :: basic) |> widgetModelToPropertyList)
                   ((u.filtering, "my filter") :: (u.sorting, "asc") :: expectedBasic |> List.sort)               
              
           ,  test "create a range-slider" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "range-slider") :: (u.minimumInt, "7") :: (u.maximumInt, "13") :: (u.stepsInt, "3") :: basic) |> widgetModelToPropertyList)
                   ((u.minimumInt, "7") :: (u.maximumInt, "13") :: (u.stepsInt, "3") :: expectedBasic |> List.sort)               
              
           ,  test "create a date-viewer" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "date-viewer") :: (u.format, "YYYY") :: basic) |> widgetModelToPropertyList)
                    ((u.format, "YYYY") :: expectedBasic |> List.sort)               
              
           ,  test "create a long-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "long-text") :: (u.regex, "[0-9]+")  :: (u.maxLength, "20") :: basic) |> widgetModelToPropertyList)
                   ((u.regex, "[0-9]+")  :: (u.maxLength, "20") :: expectedBasic |> List.sort)               
            ,  test "create a text-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "text-area") :: (u.minLines, "12") :: (u.maxLines, "15") :: basic) |> widgetModelToPropertyList)
                   ((u.minLines, "12") :: (u.maxLines, "15") :: expectedBasic |> List.sort)               
            ,  test "create a markdown-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "markdown-area") :: (u.minLines, "12") :: (u.maxLines, "15") :: basic) |> widgetModelToPropertyList)
                   ((u.minLines, "12") :: (u.maxLines, "15") :: expectedBasic |> List.sort)               
           ,  test "create a bounded-radio" <|
                \() ->
                    Expect.equal
                    (createWidgetModel ((u.widgetType, "bounded-radio") :: (u.filtering, "my filter") :: (u.sorting, "asc") :: basic) |> widgetModelToPropertyList)
                   ((u.filtering, "my filter") :: (u.sorting, "asc") :: expectedBasic |> List.sort)               
             ]
        ]
