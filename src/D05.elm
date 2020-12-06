module D05 exposing (main)

{-
   const seats = $0.innerText
     .trim()
     .split('\n')
     .map(
       s => parseInt(s.replace(/F|L/g, '0').replace(/B|R/g, '1'), 2)
     ).sort((a, b) => b - a);

   console.log(
     'part1',
     seats[0] ,
     'part2',
     seats.find((v, i, a) => i && a[i - 1] - v > 1 ) + 1
   );
-}

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser as P exposing ((|.), (|=), Parser)
import Set


type alias Model =
    String


type Msg
    = InputChanged String


main : Program () String Msg
main =
    Browser.element
        { init = \() -> ( "", Cmd.none )
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



-- yes I can represent the seat as only an int, but I kind of like
-- playing the game of representing the row and seat separately


type alias Seat =
    { row : Int
    , col : Int
    }


toID : Seat -> Int
toID { row, col } =
    row * 8 + col


type Airplane
    = OccupiedPlane PlaneData
    | EmptyPlane


type alias PlaneData =
    { occupiedSeats : List Seat
    , first : Seat
    , last : Seat
    }


minSeat : Seat -> Seat -> Seat
minSeat a b =
    case ( compare a.row b.row, compare a.col b.col ) of
        ( LT, _ ) ->
            a

        ( GT, _ ) ->
            b

        ( EQ, LT ) ->
            a

        ( EQ, _ ) ->
            b


maxSeat : Seat -> Seat -> Seat
maxSeat a b =
    case ( compare a.row b.row, compare a.col b.col ) of
        ( LT, _ ) ->
            b

        ( GT, _ ) ->
            a

        ( EQ, LT ) ->
            b

        ( EQ, _ ) ->
            a


addFlier : Airplane -> Seat -> Airplane
addFlier soFar seat =
    case soFar of
        EmptyPlane ->
            OccupiedPlane
                { occupiedSeats = [ seat ]
                , first = seat
                , last = seat
                }

        OccupiedPlane { occupiedSeats, first, last } ->
            OccupiedPlane
                { occupiedSeats = seat :: occupiedSeats
                , first = minSeat seat first
                , last = maxSeat seat last
                }


parseSeats : Parser Airplane
parseSeats =
    P.loop EmptyPlane parseInnerSeats


doBinary : Char -> Char -> Int -> List Char -> Int
doBinary zero one soFar str =
    case str of
        [] ->
            soFar

        fst :: rest ->
            case ( fst == zero, fst == one ) of
                ( True, _ ) ->
                    doBinary zero one (soFar * 2) rest

                ( _, True ) ->
                    doBinary zero one (soFar * 2 + 1) rest

                _ ->
                    doBinary zero one soFar rest


seatFromCodes : String -> String -> Seat
seatFromCodes row col =
    Seat
        (doBinary 'F' 'B' 0 <| String.toList row)
        (doBinary 'L' 'R' 0 <| String.toList col)



-- basically a placeholder now


isColChar : Char -> Bool
isColChar c =
    c == 'L' || c == 'R'


isRowChar : Char -> Bool
isRowChar c =
    c == 'F' || c == 'B'


addFlierFromCodes : Airplane -> String -> String -> P.Step Airplane a
addFlierFromCodes soFar row col =
    seatFromCodes row col |> addFlier soFar |> P.Loop


parseInnerSeats : Airplane -> Parser (P.Step Airplane Airplane)
parseInnerSeats soFar =
    P.oneOf
        [ P.succeed (addFlierFromCodes soFar)
            |= P.variable
                { inner = isRowChar
                , reserved = Set.empty
                , start = isRowChar
                }
            |= P.variable
                { inner = isColChar
                , reserved = Set.empty
                , start = isColChar
                }
            |. P.oneOf
                [ P.symbol "\n"
                , P.end
                ]
        , P.succeed ()
            |> P.map (\_ -> P.Done soFar)
        ]


missingId : PlaneData -> Int
missingId { first, last, occupiedSeats } =
    let
        f =
            toID first - 1

        l =
            toID last

        sum =
            (l * (l + 1)) // 2 - (f * (f + 1)) // 2
    in
    List.foldl (\s t -> t - toID s) sum occupiedSeats


processInput : String -> String
processInput s =
    case P.run parseSeats s of
        Ok (OccupiedPlane airplane) ->
            "part1: "
                ++ (airplane.last |> toID |> String.fromInt)
                ++ " part2: "
                ++ (missingId airplane |> String.fromInt)

        Ok EmptyPlane ->
            "empty plane!"

        Err errs ->
            List.map Debug.toString errs |> String.join "\n"
