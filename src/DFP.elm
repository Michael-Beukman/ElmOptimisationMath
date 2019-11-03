module DFP exposing (..)

import Matrix exposing (..)



-- This is the DFP Quasi Newton's method


getnewG : Matrix -> Matrix -> Matrix -> Matrix
getnewG delta gamma oldG =
    let
        gGamma =
            mult oldG gamma
    in
    add (add oldG (div (mult delta (transpose delta)) (dot delta gamma)))
        (div
            (mult gGamma (transpose gGamma))
            (-1
                * dot gamma gGamma
            )
        )


stepsStart : Function -> VecFunction -> Float -> Matrix -> List Matrix
stepsStart f g tol xstart =
    steps f g (g xstart) (makeMatrix [ [ 1, 0 ], [ 0, 1 ] ]) tol xstart 1000


steps : Function -> VecFunction -> Matrix -> Matrix -> Float -> Matrix -> Int -> List Matrix
steps f g currG gG tol xstart counter =
    let
        xnew =
            add xstart (div (mult gG currG) -1)

        newG =
            g xnew

        delta =
            sub xnew xstart

        gamma =
            sub newG currG

        newGG =
            getnewG delta gamma gG

        -- z =
        --     Debug.log "dfp g" newG
        -- zz =
        --     Debug.log "dfp G" newGG
        -- zzz =
        --     Debug.log "dfp delta" delta
        -- zzzz =
        --     Debug.log "dfp gamma" gamma
        -- zzzzzzzz =
        --     Debug.log "dfp newX" xnew
        -- zzzzzz =
        --     Debug.log "dfp newX" (div (mult gG newG) -1)
        -- zzzzz =
        --     Debug.log "--------------\n" "--"
    in
    if norm newG < tol || counter == 0 then
        [ xnew ]

    else
        [ xstart ] ++ steps f g newG newGG tol xnew (counter - 1)
