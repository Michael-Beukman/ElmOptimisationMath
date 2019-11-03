module RandomWalk exposing (..)

import Matrix exposing (..)
import Random exposing (..)


getLi : Int -> Seed -> ( List Float, Seed )
getLi num seed =
    let
        gen =
            Random.float -1 1

        ( val, newseed ) =
            Random.step gen seed
    in
    if num == 1 then
        ( [ val ], newseed )

    else
        let
            ( li, newnewseed ) =
                getLi (num - 1) newseed
        in
        ( [ val ] ++ li, newnewseed )


randomWalk : Function -> Matrix -> Float -> Float -> Int -> Seed -> Int -> List Matrix
randomWalk f x0 tol lambda maxK seed currK =
    let
        dddd =
            Debug.log "here " [ toFloat maxK, toFloat currK, lambda, tol ]

        -- dir: Matrix
        ( li, newseed ) =
            getLi 2 seed

        dir : Matrix
        dir =
            makeMatrix (List.map (\v -> [ v ]) li)

        mag =
            norm dir
    in
    if mag <= 1 then
        randomWalk f x0 tol lambda maxK newseed (currK + 1)

    else
        --
        let
            x =
                add x0 (multNum dir lambda)

            f0 =
                f x0

            f1 =
                f x
        in
        if f1 < f0 then
            [ x0 ] ++ randomWalk f x tol lambda maxK newseed maxK

        else if currK < maxK then
            randomWalk f x0 tol lambda maxK newseed (currK + 1)

        else
            let
                newlambda =
                    lambda / 2
            in
            if newlambda < tol then
                -- we are done
                [ x0 ]

            else
                randomWalk f x0 tol newlambda maxK newseed (currK + 1)
