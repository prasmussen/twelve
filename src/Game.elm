module Game exposing
    ( Fields
    , Model
    , Number(..)
    , fields
    , fieldsToList
    , init
    , isSolved
    , knownFields
    , numberToInt
    , randomFields
    , shiftLeft
    , shiftRight
    , solvedFields
    , swap
    )

import List.Extra
import Random
import Random.List



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


numberToInt : Number -> Int
numberToInt number =
    case number of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Eleven ->
            11

        Twelve ->
            12



-- FIELDS


type alias Fields a =
    { first : a
    , second : a
    , third : a
    , forth : a
    , fifth : a
    , sixth : a
    , seventh : a
    , eighth : a
    , ninth : a
    , tenth : a
    , eleventh : a
    , twelfth : a
    }


mapFields : (List a -> List a) -> Fields a -> Fields a
mapFields mapper fields_ =
    fields_
        |> fieldsToList
        |> mapper
        |> fieldsFromList
        |> Maybe.withDefault fields_


solvedFields : Fields Number
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


knownFields : Fields Number
knownFields =
    { first = One
    , second = Three
    , third = Ten
    , forth = Five
    , fifth = Seven
    , sixth = Nine
    , seventh = Eight
    , eighth = Eleven
    , ninth = Two
    , tenth = Nine
    , eleventh = Twelve
    , twelfth = Four
    }


randomFields : Random.Seed -> Fields Number
randomFields seed =
    let
        generator =
            Random.List.shuffle (fieldsToList solvedFields)
    in
    Random.step generator seed
        |> Tuple.first
        |> fieldsFromList
        |> Maybe.withDefault solvedFields


fieldsToList : Fields a -> List a
fieldsToList fields_ =
    [ fields_.first
    , fields_.second
    , fields_.third
    , fields_.forth
    , fields_.fifth
    , fields_.sixth
    , fields_.seventh
    , fields_.eighth
    , fields_.ninth
    , fields_.tenth
    , fields_.eleventh
    , fields_.twelfth
    ]


fieldsFromList : List a -> Maybe (Fields a)
fieldsFromList list =
    case list of
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
    = Model (Fields Number)


init : Fields Number -> Model
init fields_ =
    Model fields_


isSolved : Model -> Bool
isSolved (Model fields_) =
    mapFields (shiftUntilStartsWith One) fields_ == solvedFields


fields : Model -> Fields Number
fields (Model fields_) =
    fields_



-- OPERATIONS


shiftRight : Model -> Model
shiftRight (Model fields_) =
    Model (mapFields shiftRightHelper fields_)


shiftLeft : Model -> Model
shiftLeft (Model fields_) =
    Model (mapFields shiftLeftHelper fields_)


swap : Model -> Model
swap (Model fields_) =
    Model (mapFields swapHelper fields_)



-- LIST HELPERS


shiftRightHelper : List a -> List a
shiftRightHelper list =
    case List.Extra.unconsLast list of
        Just ( last, rest ) ->
            last :: rest

        Nothing ->
            list


shiftLeftHelper : List a -> List a
shiftLeftHelper list =
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


shiftUntilStartsWith : a -> List a -> List a
shiftUntilStartsWith target list =
    if List.member target list then
        shiftUntilStartsWithHelper target list

    else
        list


shiftUntilStartsWithHelper : a -> List a -> List a
shiftUntilStartsWithHelper target list =
    case list of
        first :: _ ->
            if first == target then
                list

            else
                shiftUntilStartsWithHelper target (shiftLeftHelper list)

        _ ->
            list
