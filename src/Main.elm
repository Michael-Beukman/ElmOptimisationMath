module Main exposing (..)

import DFP exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Plots exposing (..)
import Render3d exposing (..)


main =
    let
        --a =stepsStart f derivative 0.001 (makeMatrix [ [ 5 ], [ 10 ] ])
        b =
            Debug.log "dfp a" "lol"
    in
    Html.div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        ([]
            ++ [ Html.div []
                    [ renderAxes 1000 1000
                    ]
               ]
        )
