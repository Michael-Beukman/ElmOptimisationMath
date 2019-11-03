module Matrix exposing (..)


type alias Matrix =
    { data : List (List Float)
    , rows : Int
    , cols : Int
    }


type alias Function =
    Matrix -> Float


type alias VecFunction =
    Matrix -> Matrix


makeNothingMatrix : Int -> Int -> List (List (Maybe Float))
makeNothingMatrix rows cols =
    let
        makeCol numcols =
            if numcols == 0 then
                []

            else
                [ Nothing ] ++ makeCol (numcols - 1)
    in
    if rows == 0 then
        []

    else
        [ makeCol cols ] ++ makeNothingMatrix (rows - 1) cols


findFirstNothing : Int -> List (Maybe Float) -> Int
findFirstNothing index li =
    if List.length li == 0 then
        index

    else if List.head li == Just Nothing then
        index

    else
        findFirstNothing (index + 1) (Maybe.withDefault [] (List.tail li))



-- transposes a matrix


div : Matrix -> Float -> Matrix
div mat num =
    List.map (List.map (\n -> n / num)) mat.data
        |> makeMatrix


multNum : Matrix -> Float -> Matrix
multNum mat num =
    List.map (List.map (\n -> n * num)) mat.data
        |> makeMatrix


dot : Matrix -> Matrix -> Float
dot mat1 mat2 =
    mult (transpose mat1) mat2
        |> (\m -> Maybe.withDefault [ 0 ] (List.head m.data))
        |> (\r -> Maybe.withDefault 0 (List.head r))


add : Matrix -> Matrix -> Matrix
add =
    matrixFunc (+)


sub : Matrix -> Matrix -> Matrix
sub =
    matrixFunc (-)


norm : Matrix -> Float
norm mat =
    List.foldl
        (\row num -> List.foldl (\curr old -> old + curr ^ 2) num row)
        0
        mat.data
        |> sqrt


matrixFunc : (Float -> Float -> Float) -> Matrix -> Matrix -> Matrix
matrixFunc fff mat1 mat2 =
    List.map2
        (List.map2 (\e1 e2 -> fff e1 e2))
        mat1.data
        mat2.data
        |> makeMatrix


transpose : Matrix -> Matrix
transpose mat =
    let
        nothing =
            makeNothingMatrix mat.cols mat.rows
    in
    -- find first pos where is nothing
    List.foldl
        (\new old ->
            List.map2
                (\el row ->
                    let
                        index =
                            findFirstNothing 0 row
                    in
                    List.take index row
                        ++ [ Just el ]
                        ++ List.take (List.length row - index - 1) (List.reverse row)
                )
                new
                old
        )
        nothing
        mat.data
        |> List.map (\row -> List.map (\el -> Maybe.withDefault 0 el) row)
        |> makeMatrix


zeroeslist : Int -> List Float
zeroeslist len =
    if len == 0 then
        []

    else
        [ 0 ] ++ zeroeslist (len - 1)


makeMatrix : List (List Float) -> Matrix
makeMatrix li =
    Matrix li (List.length li) (List.length (Maybe.withDefault [] (List.head li)))


mult : Matrix -> Matrix -> Matrix
mult mat1 mat2 =
    -- First map over rows of mat1
    List.map
        (\row ->
            List.map2
                -- el1 is a, row2 is [e, f]
                (\el1 row2 ->
                    List.map (\el2 -> el1 * el2) row2
                )
                row
                mat2.data
         -- This results in [
         --    [a * e, a * f]
         --    [b * g, b * h]
         --  ]
        )
        -- and now sum the columns
        mat1.data
        |> List.map (\row -> List.foldl (\a b -> List.map2 (\x y -> x + y) a b) (zeroeslist mat1.rows) row)
        |> makeMatrix



-- 2 x 3


m1 =
    [ [ 1, 2, 7 ]
    , [ 3, 4, 7 ]

    -- , [ 10, 4, 7 ]
    ]
        |> makeMatrix



-- 3 x 2


m2 =
    [ [ 5, 6 ]
    , [ 7, 8 ]
    , [ 10, 4 ]
    ]
        |> makeMatrix


f =
    let
        x =
            Debug.log "mat ans " (transpose m1)
    in
    True
