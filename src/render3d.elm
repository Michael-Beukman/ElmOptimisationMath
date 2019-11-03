module Render3d exposing (..)

import Angle
import Browser exposing (..)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (..)
import Html.Attributes exposing (..)
import LineSegment2d exposing (LineSegment2d)
import Pixels
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (..)
import Svg.Attributes as Attributes exposing (..)



-- main =Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { x : Int }


init =
    { x = 0 }


update msg model =
    model


view =
    div [ Html.Attributes.style "height" "1000px" ]
        [ svg [ Attributes.height "1000px" ]
            [ let
                axes =
                    Frame2d.atPoint (Point2d.pixels 150 150)
                        |> Frame2d.rotateBy (Angle.degrees 20)
              in
              Svg.rectangle2d
                [ Attributes.stroke "blue"
                , Attributes.fill "orange"
                , Attributes.strokeWidth "4"
                , Attributes.rx "15"
                , Attributes.ry "15"
                ]
                (Rectangle2d.withAxes axes
                    ( Pixels.pixels 120, Pixels.pixels 80 )
                )
            ]
        ]
