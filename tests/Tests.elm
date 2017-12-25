module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Outlook exposing (createWidgetModel, widgetModelToPropertyList)

basic: List (String, String)
basic = [("id", "id123"), ("label", "some label"),("hint", "some hint"),("prominence", "important"),("query", "my query")]
expectedBasic = [("hint","some hint"),("id","id123"),("label","some label"),("prominence","important"),("query","my query"),("style","")]

all : Test
all =
    describe "Bubblegum.Outlook"
        [ describe "createWidgetModel" <|
            [ test "create a checkbox" <|
                \() ->
                    Expect.equal
                    (createWidgetModel (("type", "checkbox") :: basic) |> widgetModelToPropertyList)
                    expectedBasic                
             ]
        ]
