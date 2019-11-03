module Main exposing (..)

import Browser exposing (..)
import Browser.Events exposing (onClick)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import DFP exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Matrix exposing (..)
import Plots exposing (..)
import Random exposing (..)
import RandomWalk exposing (..)
import Render3d exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { step : Int
    , method : String
    , renderables : List Canvas.Renderable
    , lastPoint : Matrix
    }


type Msg
    = Click
    | KeyPress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress ->
            ( model, Cmd.none )

        Click ->
            let
                li =
                    stepsStart Plots.f derivative 0.001 model.lastPoint 1

                -- randomWalk Plots.f model.lastPoint 0.001 2 1 (Random.initialSeed 10) 0
                z =
                    [ Debug.log "clicked" li ]

                s =
                    Debug.log "renders" (List.length model.renderables)

                lastPoint =
                    Maybe.withDefault (makeMatrix []) (List.head (List.reverse li))
            in
            ( { model | renderables = model.renderables ++ renderDFP (Maybe.withDefault [] (List.tail li)), lastPoint = lastPoint }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        b =
            1
    in
    Html.div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        ([]
            ++ [ Html.div []
                    [ renderAxes 1000 1000 model.renderables

                    -- , Canvas.toHtml ( 1000, 1000 )
                    --     [ style "border" "10px solid rgba(0,0,0,0.1)" ]
                    --     ([]
                    --      --renderDFP
                    --      -- ++ renderSimplex
                    --     )
                    ]
               ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    onClick
        -- Click
        (Decode.succeed
            Click
        )


init : () -> ( Model, Cmd Msg )
init () =
    ( { step = 0, method = "none", renderables = [], lastPoint = makeMatrix [ [ 1 ], [ -1 ] ] }, Cmd.none )
