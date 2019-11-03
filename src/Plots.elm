module Plots exposing (..)

import Canvas exposing (circle, lineTo, path, rect, shapes)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color exposing (..)
import DFP exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Random exposing (..)
import RandomWalk exposing (..)
import Simplex exposing (..)


clearScreen screenwidth screenheight =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat screenwidth) (toFloat screenheight) ]


renderAxes : Int -> Int -> Html msg
renderAxes screenwidth screenheight =
    Canvas.toHtml
        ( screenwidth, screenheight )
        [ style "border" "10px solid rgba(0,0,0,0.1)" ]
        ([ clearScreen screenwidth screenheight ]
            ++ [ shapes [ fill Color.red, stroke Color.black ]
                    [ path ( 0, toFloat (screenheight // 2) )
                        [ lineTo ( toFloat screenwidth, toFloat (screenheight // 2) ) ]
                    ]
               ]
            ++ [ shapes [ fill Color.red, stroke Color.black ] [ path ( toFloat (screenwidth // 2), 0 ) [ lineTo ( toFloat (screenheight // 2), toFloat screenwidth ) ] ] ]
            ++ [ contourplots (\( x, y ) -> f (makeMatrix [ [ x ], [ y ] ])) ]
            --++ renderDFP
            ++ renderSimplex
        )


makeLi : Float -> Float -> Float -> List Float
makeLi min max spacing =
    if min >= max then
        []

    else
        [ min ] ++ makeLi (min + spacing) max spacing


cross : List Float -> List Float -> List ( Float, Float )
cross li1 li2 =
    List.map (\num1 -> List.map (\num2 -> ( num1, num2 )) li2) li1
        |> List.foldl (++) []


scale ( x, y ) =
    ( ((x + 10) / 20) * 1000, ((y + 10) / 20) * 1000 )


contourplots : (( Float, Float ) -> Float) -> Canvas.Renderable
contourplots func =
    let
        li =
            makeLi -2 2 (1 / 100)

        points =
            cross li li

        zs =
            List.map (\( x, y ) -> func ( x, y )) points

        all =
            List.map2 (\point z -> ( point, z )) points zs
    in
    shapes [ stroke Color.red ]
        (List.map
            (\zpoint ->
                let
                    goods =
                        List.filter (\( p, z ) -> abs (z - zpoint) < 0.005) all

                    goodsp =
                        List.map (\( p, z ) -> p) goods
                            |> List.sortWith
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    let
                                        angle1 =
                                            atan2 y1 x1

                                        angle2 =
                                            atan2 y2 x2

                                        -- d =Debug.log "angles " ( angle1, angle2 )
                                    in
                                    if angle1 > angle2 then
                                        GT

                                    else if angle1 < angle2 then
                                        LT

                                    else
                                        EQ
                                )

                    -- zzzz =Debug.log "points " goods
                    todraw =
                        List.map
                            (\( x, y ) -> scale ( x, y ))
                            goodsp

                    firstF =
                        Maybe.withDefault ( 0, 0 ) (List.head todraw)
                in
                path firstF
                    (List.map
                        (\p -> lineTo p)
                        todraw
                    )
            )
            [ 0, 1, 2, 3 ]
        )


renderDFP : List Canvas.Renderable
renderDFP =
    let
        li =
            -- stepsStart f derivative 0.001 (makeMatrix [ [ 1 ], [ -1 ] ])
            -- now random walk
            randomWalk f (makeMatrix [ [ 1 ], [ -1 ] ]) 0.001 2 100 (Random.initialSeed 10) 0

        limat =
            List.map
                (\value ->
                    ( first value
                    , second value
                    )
                )
                li

        ff =
            Maybe.withDefault ( 0, 0 ) (List.head limat)

        lollog =
            Debug.log "list " li
    in
    List.map
        (\matrix ->
            let
                x =
                    first matrix

                y =
                    second matrix
            in
            shapes [ fill Color.blue ] [ circle (scale ( x, y )) 1 ]
        )
        li
        ++ [ shapes [ stroke Color.green ] [ path (scale ff) (List.map (\ffff -> lineTo (scale ffff)) limat) ] ]


first : Matrix -> Float
first vec =
    Maybe.withDefault 0 (List.head (Maybe.withDefault [ 0 ] (List.head vec.data)))


second : Matrix -> Float
second vec =
    first (Maybe.withDefault [ [ 0 ] ] (List.tail vec.data) |> makeMatrix)


f : Function
f x =
    let
        x1 =
            first x

        x2 =
            second x
    in
    x1 ^ 2 - 4 * x1 + x2 ^ 2 - x2 - x1 * x2



-- x1 ^ 2 + x2 ^ 2 + x1 ^ 2 * x2 ^ 4


derivative : VecFunction
derivative x =
    let
        x1 =
            first x

        x2 =
            second x
    in
    [ [ 2 * x1 + 2 * x1 * x2 ^ 4 ]
    , [ 2 * x2 + x1 ^ 2 * 4 * x2 ^ 3 ]
    ]
        |> makeMatrix


renderSimplex : List Canvas.Renderable
renderSimplex =
    let
        -- b
        bI =
            Node 1.2 0 -99

        -- g
        gI =
            Node 0.0 0.8 -99

        -- w
        wI =
            Node 0 0 -99

        initsimplex =
            Simplex
                { bI | value = getFVal f bI }
                { gI | value = getFVal f gI }
                { wI | value = getFVal f wI }

        finalSimplex =
            simplexMethod f 300 initsimplex

        z =
            [ Debug.log "simplex" finalSimplex, Debug.log "init" initsimplex ]
    in
    []
