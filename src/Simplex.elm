module Simplex exposing (..)

import Matrix exposing (..)


type alias Node =
    { x : Float
    , y : Float
    , value : Float
    }


type alias Simplex =
    { b : Node
    , g : Node
    , w : Node
    }



-- Simplex method stuffs


midpoint : Node -> Node -> Node
midpoint a b =
    let
        mid func na nb =
            (func na + func nb) / 2
    in
    Node
        (mid .x a b)
        (mid .y a b)
        -999


getFVal : Function -> Node -> Float
getFVal func node =
    let
        matrix =
            makeMatrix [ [ node.x ], [ node.y ] ]
    in
    func matrix


reflect : Simplex -> Function -> Node
reflect simplex f =
    let
        m =
            midpoint simplex.b simplex.g

        r =
            Node (m.x * 2 - simplex.w.x) (m.y * 2 - simplex.w.y) -99
    in
    { r | value = getFVal f r }


expand : Function -> Simplex -> Node -> Node
expand f simplex r =
    let
        m =
            midpoint simplex.b simplex.g

        e =
            Node (r.x * 2 - m.x) (r.y * 2 - m.y) -99
    in
    { e | value = getFVal f e }


contract : Function -> Simplex -> Node -> Node
contract f simplex r =
    let
        m =
            midpoint simplex.b simplex.g

        c1 =
            midpoint m simplex.w

        c2 =
            midpoint m r

        fc1 =
            getFVal f c1

        fc2 =
            getFVal f c2

        c =
            if fc1 < fc2 then
                c1

            else
                c2

        fc =
            if fc1 < fc2 then
                fc1

            else
                fc2
    in
    { c | value = fc }


shrink : Function -> Simplex -> Simplex
shrink f simplex =
    let
        m =
            midpoint simplex.b simplex.g

        sTemp =
            midpoint simplex.b simplex.w

        s =
            { sTemp | value = getFVal f sTemp }
    in
    { simplex | w = s, g = { m | value = getFVal f m } }


orderSimplex : Simplex -> Simplex
orderSimplex simplex =
    let
        li =
            [ simplex.b, simplex.g, simplex.w ]

        sorted =
            List.sortWith
                (\n1 n2 ->
                    compare n1.value n2.value
                )
                li

        b =
            List.head sorted

        g =
            List.head (Maybe.withDefault [] (List.tail sorted))

        w =
            List.head (Maybe.withDefault [] (List.tail (Maybe.withDefault [] (List.tail sorted))))
    in
    Simplex (Maybe.withDefault (Node -160 -160 -160) b) (Maybe.withDefault (Node -160 -160 -160) g) (Maybe.withDefault (Node -160 -160 -160) w)


simplexMethod : Function -> Int -> Simplex -> Simplex
simplexMethod func numruns simplexI =
    let
        simplex =
            orderSimplex simplexI

        k =
            Debug.log "Simplex " simplex
    in
    if numruns == 0 then
        simplex

    else
        (let
            r : Node
            r =
                reflect simplex func
         in
         if r.value < simplex.w.value then
            -- then we expand
            let
                e =
                    expand func simplex r

                bestNewNode =
                    if e.value < r.value then
                        e

                    else
                        r
            in
            { simplex | w = bestNewNode }

         else
            -- then we contract
            let
                c : Node
                c =
                    contract func simplex r
            in
            if c.value < simplex.w.value then
                -- done
                { simplex | w = c }

            else
                -- then we contract
                shrink func simplex
        )
            |> simplexMethod func (numruns - 1)
