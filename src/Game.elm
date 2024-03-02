module Game exposing
    ( Fields
    , Model
    , Number(..)
    , init
    , isSolved
    , randomFields
    , shiftLeft
    , shiftRight
    , solvedFields
    , swap
    , toList
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


randomFields : Random.Seed -> Fields
randomFields seed =
    let
        generator =
            Random.List.shuffle (fieldsToList solvedFields)
    in
    Random.step generator seed
        |> Tuple.first
        |> fieldsFromList
        |> Maybe.withDefault solvedFields


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
    mapFields (shiftUntilStartsWith One) fields == solvedFields


toList : Model -> List Number
toList (Model fields) =
    fieldsToList fields



-- OPERATIONS


shiftRight : Model -> Model
shiftRight (Model fields) =
    Model (mapFields shiftRightHelper fields)


shiftLeft : Model -> Model
shiftLeft (Model numbers) =
    Model (mapFields shiftLeftHelper numbers)


swap : Model -> Model
swap (Model numbers) =
    Model (mapFields swapHelper numbers)



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
shiftUntilStartsWith target numbers =
    if List.member target numbers then
        shiftUntilStartsWithHelper target numbers

    else
        numbers


shiftUntilStartsWithHelper : a -> List a -> List a
shiftUntilStartsWithHelper target numbers =
    case numbers of
        first :: _ ->
            if first == target then
                numbers

            else
                shiftUntilStartsWithHelper target (shiftLeftHelper numbers)

        _ ->
            numbers
