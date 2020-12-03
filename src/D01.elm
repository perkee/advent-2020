module D01 exposing (main)

import Browser
import Dict
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra as LE
import Parser as P exposing ((|.), (|=), Parser)
import Set


type alias Model =
    String


type Msg
    = InputChanged String


main : Program () String Msg
main =
    Browser.element
        { init = \() -> ( "paste your input here", Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd.Cmd msg )
update (InputChanged s) _ =
    ( s, Cmd.none )


view : String -> H.Html Msg
view s =
    H.div []
        [ processInput s |> H.text |> List.singleton |> H.pre []
        , H.textarea [ HA.value s, HE.onInput InputChanged ] []
        ]


processInput : String -> String
processInput s =
    case P.run statements s of
        Ok ints ->
            "part1: "
                ++ part1 ints
                ++ ", part2: "
                ++ part2 ints

        Err errs ->
            List.map Debug.toString errs |> String.join "\n"


statements : Parser (List Int)
statements =
    P.loop [] statementsHelp


statementsHelp : List Int -> Parser (P.Step (List Int) (List Int))
statementsHelp soFar =
    P.oneOf
        [ P.succeed (\next -> P.Loop (next :: soFar))
            |= P.int
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse soFar))
        ]


part1 : List Int -> String
part1 ints =
    let
        s =
            Set.fromList ints
    in
    case find (filter1 s) ints of
        Just n ->
            (2020 - n) * n |> String.fromInt

        Nothing ->
            "¿¿¿"


filter1 : Set.Set Int -> Int -> Bool
filter1 s int =
    Set.member (2020 - int) s


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        l :: ls ->
            if predicate l then
                Just l

            else
                find predicate ls

        [] ->
            Nothing


part2 : List Int -> String
part2 list =
    let
        sumToProd =
            -- reduces complexity to O(n*2)ish, probably O(n*2*log n)
            -- compared to lift3
            -- in practice it goes from a long wait to instant
            Dict.fromList <| LE.lift2 addAndMult list list
    in
    case findButCooler (filter2 sumToProd) <| list of
        Just prod ->
            String.fromInt prod

        Nothing ->
            "???"


filter2 : Dict.Dict Int Int -> Int -> Maybe Int
filter2 sumToProd n =
    Dict.get (2020 - n) sumToProd
        |> Maybe.map ((*) n)


addAndMult : Int -> Int -> ( Int, Int )
addAndMult a b =
    ( a + b, a * b )


findButCooler : (a -> Maybe b) -> List a -> Maybe b
findButCooler predicate list =
    case list of
        l :: ls ->
            case predicate l of
                Just b ->
                    Just b

                Nothing ->
                    findButCooler predicate ls

        [] ->
            Nothing
