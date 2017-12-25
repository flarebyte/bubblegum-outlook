module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Outlook exposing (createWidgetModel, widgetModelToPropertyList)

basic: List (String, String)
basic = [("id", "id123"), ("label", "some label"),("hint", "some hint"),("prominence", "important"),("query", "my query")]
expectedBasic = ("style","") :: basic

all : Test
all =
    describe "Bubblegum.Outlook"
        [ describe "createWidgetModel" <|
            [ test "create a checkbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "checkbox"):: ("anything", "some noise") :: basic) |> widgetModelToPropertyList)
                    (expectedBasic |> List.sort)               
             
           ,  test "create a inc-spinner" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "inc-spinner") :: ("maximum", "15") :: basic) |> widgetModelToPropertyList)
                   (("maximum","15") :: ("minimum","0") :: ("steps","1") :: expectedBasic |> List.sort)               
              
           ,  test "create a medium-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "medium-text") :: ("regex", "[0-9]+")  :: ("max-length", "20") :: basic) |> widgetModelToPropertyList)
                   (("regex", "[0-9]+") :: ("max-length", "20") :: expectedBasic |> List.sort)               
              
           ,  test "create a bounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "bounded-listbox") :: ("filtering", "my filter") :: ("sorting", "asc") :: basic) |> widgetModelToPropertyList)
                   (("filtering", "my filter") :: ("sorting", "asc") :: expectedBasic |> List.sort)               
              
           , test "create a unbounded-listbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "unbounded-listbox") :: ("filtering", "my filter") :: ("sorting", "asc") :: basic) |> widgetModelToPropertyList)
                   (("filtering", "my filter") :: ("sorting", "asc") :: expectedBasic |> List.sort)               
              
           ,  test "create a range-slider" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "range-slider") :: ("minimum", "7") :: ("maximum", "13") :: ("steps", "3") :: basic) |> widgetModelToPropertyList)
                   (("minimum", "7") :: ("maximum", "13") :: ("steps", "3") :: expectedBasic |> List.sort)               
              
           ,  test "create a date-viewer" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "date-viewer") :: ("format", "YYYY") :: basic) |> widgetModelToPropertyList)
                    (("format", "YYYY") :: expectedBasic |> List.sort)               
              
           ,  test "create a long-text" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "long-text") :: ("regex", "[0-9]+")  :: ("max-length", "20") :: basic) |> widgetModelToPropertyList)
                   (("regex", "[0-9]+")  :: ("max-length", "20") :: expectedBasic |> List.sort)               
            ,  test "create a text-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "text-area") :: ("minimum-lines", "12") :: ("maximum-lines", "15") :: basic) |> widgetModelToPropertyList)
                   (("minimum-lines", "12") :: ("maximum-lines", "15") :: expectedBasic |> List.sort)               
            ,  test "create a markdown-area" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "markdown-area") :: ("minimum-lines", "12") :: ("maximum-lines", "15") :: basic) |> widgetModelToPropertyList)
                   (("minimum-lines", "12") :: ("maximum-lines", "15") :: expectedBasic |> List.sort)               
           ,  test "create a bounded-radio" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "bounded-radio") :: ("filtering", "my filter") :: ("sorting", "asc") :: basic) |> widgetModelToPropertyList)
                   (("filtering", "my filter") :: ("sorting", "asc") :: expectedBasic |> List.sort)               
             ]
        ]
