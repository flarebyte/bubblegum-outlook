module Bubblegum.Bulma.TextWidget exposing (fromModel)
{-| This module allow to manipulate a field model

# FieldModel functions
@docs 

-}

import Html exposing (Html, text, div, input, label, p)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)
import Bubblegum.FieldModel as FieldModel
import Bubblegum.Bulma.Utils exposing(..)

fromModel: (String -> msg) -> FieldModel.Model -> Html msg
fromModel msg fieldModel =
    let
        styles =  toStyle ["input"] fieldModel.prominence fieldModel.styles fieldModel.icons   
        btnAttrs = [ type_text , onInput msg, class styles]
        placeholderOpt = Maybe.map placeholder fieldModel.placeholder |> maybeToList
        labelText = fieldModel.label |> Maybe.withDefault fieldModel.id
        helpParag = Maybe.map text fieldModel.hint |> maybeToList
     in       
        div [ class_field ]
        [ label [ class_label ]
            [ text labelText ]
        , div [ class_control ]
            [ input ( btnAttrs ++ placeholderOpt)
                []
            ]
        , p [ class_help ]
            helpParag
        ]