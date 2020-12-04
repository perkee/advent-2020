module D03 exposing (main)

{-
   JS solution to be run in console on problem input page

   const rows = $0.innerText.trim().split('\n');
   const getCol = (n, i) =>
     ((i * n) % (rows[0].length));
   const getCel = (v, h, i) =>
     (rows[i * v] || [])[ getCol(h, i) ] == '#';
   const slopes = '11,13,15,17,21'.split(',');
   const trees = slopes.map(([rise, run]) =>
     rows.reduce(
       (count, _, i) =>
         count + getCel(rise, run, i), 0
     )
   );
   console.log(
     'part1',
     trees[3],
     'part2',
     trees.reduce((p, n) => p * n)
   );
-}

import Browser
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
        Ok forest ->
            "part1: "
                ++ part1 forest
                ++ ", part2: "
                ++ part2 forest

        Err errs ->
            List.map Debug.toString errs |> String.join "\n"


type alias Tree =
    ( Int, Int )


type Forest
    = Forest Int Int (Set Tree)


emptyForest : Forest
emptyForest =
    Forest 0 0 Set.empty


statements : Parser Forest
statements =
    P.loop emptyForest statementsHelp


statementsHelp : Forest -> Parser (P.Step Forest Forest)
statementsHelp soFar =
    P.oneOf
        [ P.succeed (grab soFar)
            |= P.getPosition
            |= P.getChompedString (P.chompIf (\_ -> True))
        , P.succeed ()
            |> P.map (\_ -> P.Done soFar)
        ]


grab : Forest -> Tree -> String -> P.Step Forest any
grab (Forest height_ width_ soFar) ( row, col ) str =
    let
        forest =
            Forest
                (max height_ row)
                (max width_ col)
    in
    case str of
        "#" ->
            P.Loop <| forest (Set.insert ( row - 1, col - 1 ) soFar)

        "." ->
            P.Loop <| forest soFar

        "\n" ->
            P.Loop <| Forest height_ width_ soFar

        _ ->
            P.Loop <| forest soFar


part1 : Forest -> String
part1 forest =
    solveForSlope forest ( 1, 3 )
        |> String.fromInt


part2 : Forest -> String
part2 forest =
    [ ( 1, 1 ), ( 1, 3 ), ( 1, 5 ), ( 1, 7 ), ( 2, 1 ) ]
        |> List.map (solveForSlope forest)
        |> List.product
        |> String.fromInt


points : Int -> Int -> ( Int, Int ) -> List ( Int, Int )
points height width ( rise, run ) =
    List.range 0 (height // rise - 1)
        |> List.map (timesTwice rise run width)


timesTwice : Int -> Int -> Int -> Int -> ( Int, Int )
timesTwice rise run width idx =
    ( idx * rise, modBy width (idx * run) )


solveForSlope : Forest -> ( Int, Int ) -> Int
solveForSlope (Forest h w trees) slope =
    slope
        |> points h w
        |> List.filter (\point -> Set.member point trees)
        |> List.length
