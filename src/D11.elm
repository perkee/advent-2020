module D11 exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser as P exposing ((|=), Parser)
import Set exposing (Set)


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
        Ok ferry ->
            "part1: "
                ++ part1 ferry

        Err errs ->
            List.map Debug.toString errs |> String.join "\n"


type alias Seat =
    ( Int, Int )


type Status
    = Open
    | Occupied


type Ferry
    = Ferry Int Int (Dict Seat Status)


emptyFerry : Ferry
emptyFerry =
    Ferry 0 0 Dict.empty


statements : Parser Ferry
statements =
    P.loop emptyFerry statementsHelp


statementsHelp : Ferry -> Parser (P.Step Ferry Ferry)
statementsHelp soFar =
    P.oneOf
        [ P.succeed (grab soFar)
            |= P.getPosition
            |= P.getChompedString (P.chompIf (\_ -> True))
        , P.succeed ()
            |> P.map (\_ -> P.Done soFar)
        ]


grab : Ferry -> Seat -> String -> P.Step Ferry any
grab (Ferry height_ width_ soFar) ( row, col ) str =
    let
        ferry =
            Ferry
                (max height_ row)
                (max width_ col)
    in
    case str of
        "L" ->
            P.Loop <| ferry (Dict.insert ( row, col ) Open soFar)

        "." ->
            P.Loop <| ferry soFar

        "\n" ->
            P.Loop <| Ferry height_ width_ soFar

        _ ->
            P.Loop <| ferry soFar


groundhog : (a -> a) -> a -> a
groundhog fn a =
    let
        next =
            fn a
    in
    if a == next then
        next

    else
        groundhog fn next


allOccupiedSeats : Ferry -> Int
allOccupiedSeats (Ferry _ _ ferry) =
    Dict.values ferry
        |> List.map toInt
        |> List.sum


nextFerry : Ferry -> Ferry
nextFerry (Ferry h w ferry) =
    Dict.map (newStatus <| Ferry h w ferry) ferry |> Ferry h w


neighbors : Seat -> Ferry -> Int
neighbors ( row, col ) (Ferry _ _ ferry) =
    [ ( row - 1, col - 1 ), ( row - 1, col ), ( row - 1, col + 1 ) ]
        ++ [ ( row, col - 1 ), ( row, col + 1 ) ]
        ++ [ ( row + 1, col - 1 ), ( row + 1, col ), ( row + 1, col + 1 ) ]
        |> List.map (\seat -> Dict.get seat ferry)
        |> List.filterMap (Maybe.map toInt)
        |> List.sum


toInt : Status -> Int
toInt status =
    if status == Occupied then
        1

    else
        0


newStatus : Ferry -> Seat -> Status -> Status
newStatus ferry seat current =
    case current of
        Occupied ->
            if neighbors seat ferry >= 4 then
                Open

            else
                Occupied

        Open ->
            if neighbors seat ferry == 0 then
                Occupied

            else
                Open


part1 : Ferry -> String
part1 ferry =
    groundhog nextFerry ferry |> allOccupiedSeats |> String.fromInt
