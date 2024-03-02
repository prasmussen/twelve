module Game exposing
    ( Fields
    , Model
    , Number(..)
    , init
    , isSolved
    , moveLeft
    , moveRight
    , solvedFields
    , swap
    , toList
    )

import List.Extra



-- NUMBER


type Number
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve



-- FIELDS


type alias Fields =
    { first : Number
    , second : Number
    , third : Number
    , forth : Number
    , fifth : Number
    , sixth : Number
    , seventh : Number
    , eighth : Number
    , ninth : Number
    , tenth : Number
    , eleventh : Number
    , twelfth : Number
    }


mapFields : (List Number -> List Number) -> Fields -> Fields
mapFields mapper fields =
    fields
        |> fieldsToList
        |> mapper
        |> fieldsFromList
        |> Maybe.withDefault fields


solvedFields : Fields
solvedFields =
    { first = One
    , second = Two
    , third = Three
    , forth = Four
    , fifth = Five
    , sixth = Six
    , seventh = Seven
    , eighth = Eight
    , ninth = Nine
    , tenth = Ten
    , eleventh = Eleven
    , twelfth = Twelve
    }


fieldsToList : Fields -> List Number
fieldsToList fields =
    [ fields.first
    , fields.second
    , fields.third
    , fields.forth
    , fields.fifth
    , fields.sixth
    , fields.seventh
    , fields.eighth
    , fields.ninth
    , fields.tenth
    , fields.eleventh
    , fields.twelfth
    ]


fieldsFromList : List Number -> Maybe Fields
fieldsFromList numbers =
    case numbers of
        [ first, second, third, forth, fifth, sixth, seventh, eighth, ninth, tenth, eleventh, twelfth ] ->
            Just
                { first = first
                , second = second
                , third = third
                , forth = forth
                , fifth = fifth
                , sixth = sixth
                , seventh = seventh
                , eighth = eighth
                , ninth = ninth
                , tenth = tenth
                , eleventh = eleventh
                , twelfth = twelfth
                }

        _ ->
            Nothing



-- MODEL


type Model
    = Model Fields


init : Fields -> Model
init fields =
    Model fields


isSolved : Model -> Bool
isSolved (Model fields) =
    let
        movedFields =
            mapFields (moveUntilStartsWith One) fields
    in
    movedFields == solvedFields


toList : Model -> List Number
toList (Model fields) =
    fieldsToList fields



-- OPERATIONS


moveRight : Model -> Model
moveRight (Model fields) =
    Model (mapFields moveRightHelper fields)


moveLeft : Model -> Model
moveLeft (Model numbers) =
    Model (mapFields moveLeftHelper numbers)


swap : Model -> Model
swap (Model numbers) =
    Model (mapFields swapHelper numbers)



-- LIST HELPERS


moveRightHelper : List a -> List a
moveRightHelper list =
    case List.Extra.unconsLast list of
        Just ( last, rest ) ->
            last :: rest

        Nothing ->
            list


moveLeftHelper : List a -> List a
moveLeftHelper list =
    case List.Extra.uncons list of
        Just ( first, rest ) ->
            rest ++ [ first ]

        Nothing ->
            list


swapHelper : List a -> List a
swapHelper list =
    case list of
        first :: second :: third :: forth :: rest ->
            second :: first :: forth :: third :: rest

        _ ->
            list


moveUntilStartsWith : a -> List a -> List a
moveUntilStartsWith target numbers =
    if List.member target numbers then
        moveUntilStartsWithHelper target numbers

    else
        numbers


moveUntilStartsWithHelper : a -> List a -> List a
moveUntilStartsWithHelper target numbers =
    case numbers of
        first :: _ ->
            if first == target then
                numbers

            else
                moveUntilStartsWith target (moveLeftHelper numbers)

        _ ->
            numbers
