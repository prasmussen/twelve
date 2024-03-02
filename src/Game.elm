module Game exposing
    ( Fields
    , Model
    , Number(..)
    , fields
    , fieldsToList
    , init
    , isSolved
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
mapFields mapper fields_ =
    fields_
        |> fieldsToList
        |> mapper
        |> fieldsFromList
        |> Maybe.withDefault fields_


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
init fields_ =
    Model fields_


isSolved : Model -> Bool
isSolved (Model fields_) =
    mapFields (shiftUntilStartsWith One) fields_ == solvedFields


fields : Model -> Fields
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
