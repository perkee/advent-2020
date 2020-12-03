module D02 exposing (main)

import Array
import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser as P exposing ((|.), (|=), Parser)
import Set


type PasswordResult
    = Valid
    | Invalid


type alias Model =
    String


type Msg
    = InputChanged String


type alias PasswordsResult =
    ( PasswordResult, PasswordResult )


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
        Ok ( part1, part2 ) ->
            String.fromInt part1
                ++ " "
                ++ String.fromInt part2

        Err errs ->
            List.map renderErr errs |> String.join "\n"


renderErr : P.DeadEnd -> String
renderErr { row, problem, col } =
    case problem of
        P.ExpectingSymbol s ->
            "Incomplete row " ++ String.fromInt row ++ " at col " ++ String.fromInt col ++ ": expecting symbol " ++ s

        _ ->
            Debug.toString problem


statements : Parser ( Int, Int )
statements =
    P.loop [] statementsHelp


statementsHelp : List PasswordsResult -> Parser (P.Step (List PasswordsResult) ( Int, Int ))
statementsHelp soFar =
    P.oneOf
        [ P.succeed (handlePassword soFar)
            |= P.int
            |. P.symbol "-"
            |= P.int
            |. P.symbol " "
            |= P.getChompedString
                (P.chompIf (\_ -> True))
            |. P.symbol ": "
            |= P.variable
                { start = \_ -> True
                , inner = Char.isLower
                , reserved = Set.empty
                }
            |. P.spaces
        , P.succeed ()
            |> P.map (finalize soFar)
        ]


finalize : List PasswordsResult -> () -> P.Step a ( Int, Int )
finalize soFar () =
    P.Done (sumResults soFar)


sumResults : List PasswordsResult -> ( Int, Int )
sumResults results =
    case results of
        ( part1, part2 ) :: rs ->
            addPairs ( toInt part1, toInt part2 ) <| sumResults rs

        [] ->
            ( 0, 0 )


toInt : PasswordResult -> Basics.Int
toInt pr =
    case pr of
        Valid ->
            1

        Invalid ->
            0


addPairs : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
addPairs ( a, b ) ( aa, bb ) =
    ( a + aa, b + bb )


handlePassword : List PasswordsResult -> Int -> Int -> String -> String -> P.Step (List PasswordsResult) a
handlePassword soFar start end c pw =
    P.Loop (( validate1 start end c pw, validate2 start end c pw ) :: soFar)


validate1 : Int -> Int -> String -> String -> PasswordResult
validate1 start end c pw =
    let
        n =
            pw
                |> String.split ""
                |> List.filter ((==) c)
                |> List.length
    in
    if n >= start && n <= end then
        Valid

    else
        Invalid


validate2 : Int -> Int -> String -> String -> PasswordResult
validate2 start end c pw =
    let
        letters =
            pw
                |> String.split ""
                |> Array.fromList
    in
    case ( Array.get (start - 1) letters == Just c, Array.get (end - 1) letters == Just c ) of
        ( False, True ) ->
            Valid

        ( True, False ) ->
            Valid

        _ ->
            Invalid
