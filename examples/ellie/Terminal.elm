module Main exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


type Msg
    = NoOp


terminal : Element msg
terminal =
    row
        [ width (px 200)
        , Element.alignLeft
        , Element.alignTop
        , Element.padding 20
        , Element.spacing 8
        ]
        [ text "Hello stylish friend!", cursorRectangle ]


cursorRectangle : Element msg
cursorRectangle =
    Element.el
        [ Element.height Element.fill
        , Background.color Color.white
        , Element.width (Element.px 18)
        ]
        (Element.text "")


main : Html Msg
main =
    Element.layout
        [ Background.color black
        , Font.color white
        , Font.size 32
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=EB+Garamond"
                , name = "Courier New"
                }
            , Font.sansSerif
            ]
        ]
    <|
        terminal
