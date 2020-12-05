module D04 exposing (main)

{-
   // JS solution to be run in console on problem input page

   const input = $0.innerText.split('\n\n');

   const haveFields = input.map(pspt => new Map(
       pspt.trim().split(/\s+/).map(x => x.trim().split(':')).filter(x => x)
     )).filter(m =>
       m.size === 8 || (m.size === 7 && !m.has('cid'))
     )
   ;

   const validations = {
       byr : y => y >= 1920 && y <= 2002,
       iyr : y => y >= 2010 && y <= 2020,
       eyr : y => y >= 2020 && y <= 2030,
       hgt : h => 'true' === h.replace(/(\d+)(cm|in)/,(_, num, unit) => {
           switch (unit) {
               case 'in': return num >= 59 && num <= 76;
               case 'cm': return num >= 150 && num <= 193;
           }
           return false
       }),
       hcl : c => /^#[0-9a-f]{6}$/.test(c),
       pid : c => /^[0-9]{9}$/.test(c),
       ecl : c => /^(amb|blu|brn|gry|grn|hzl|oth)$/.test(c)

   }

   const valid = haveFields.filter(pspt =>
       Object.keys(validations).every(key =>
           validations[key](pspt.get(key))
       )
   )

   console.log('p1', haveFields.length, 'p2', valid.length);
-}

import Browser
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser as P exposing ((|.), (|=), Parser)


type alias Model =
    String


type Msg
    = InputChanged String


type alias HalfPassport =
    Dict String String


main : Program () String Msg
main =
    Browser.element
        { init = \() -> ( "foo:bar", Cmd.none )
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
        [ (part1 s ++ " " ++ part2 s) |> H.text |> List.singleton |> H.pre []
        , H.textarea [ HA.value s, HE.onInput InputChanged ] []
        ]


part1 : String -> String
part1 =
    String.split "\n\n"
        >> List.filterMap validatePassport
        >> List.length
        >> String.fromInt


part2 : String -> String
part2 =
    String.split "\n\n"
        -- could not think of how to split on double newline in parser!
        >> List.filter verifyPassport
        >> List.length
        >> String.fromInt


validatePassport : String -> Maybe HalfPassport
validatePassport s =
    case P.run parsePassport s of
        Ok passport ->
            passport

        Err _ ->
            Nothing


verifyPassport : String -> Bool
verifyPassport s =
    case validatePassport s of
        Just hp ->
            testResult hp

        Nothing ->
            False


parsePassport : Parser (Maybe HalfPassport)
parsePassport =
    P.loop Dict.empty parseField


isNotWhiteSpace : Char -> Basics.Bool
isNotWhiteSpace c =
    c /= ' ' && c /= '\n'


parseField : HalfPassport -> Parser (P.Step HalfPassport (Maybe HalfPassport))
parseField soFar =
    P.oneOf
        [ P.succeed (handleField soFar)
            |= P.getChompedString
                (P.chompWhile ((/=) ':'))
            |. P.symbol ":"
            |= P.getChompedString
                (P.chompWhile isNotWhiteSpace)
            |. P.spaces
        , P.succeed ()
            |> P.map (finalize soFar)
        ]


handleField :
    HalfPassport
    -> String
    -> String
    -> P.Step HalfPassport (Maybe HalfPassport)
handleField soFar key val =
    P.Loop <| Dict.insert key val soFar


finalize : HalfPassport -> () -> P.Step HalfPassport (Maybe HalfPassport)
finalize soFar () =
    P.Done <|
        if hasAllFields soFar then
            Just soFar

        else
            Nothing


hasAllFields : HalfPassport -> Bool
hasAllFields hp =
    Dict.size hp
        == 8
        || (Dict.size hp
                == 7
                && (not <| Dict.member "cid" hp)
           )


testResult : HalfPassport -> Bool
testResult hp =
    [ ( "byr", inRange 1920 2002 )
    , ( "iyr", inRange 2010 2020 )
    , ( "eyr", inRange 2020 2030 )
    , ( "ecl"
      , \c ->
            String.contains c "amb blu brn gry grn hzl oth"
      )
    , ( "hcl", passesParser parseHcl )
    , ( "pid", passesParser parsePid )
    , ( "hgt", passesParser parseHgt )
    ]
        |> List.map (applyTest hp)
        |> List.member False
        |> not


inRange : Int -> Int -> String -> Bool
inRange btm top s =
    case String.toInt s of
        Just n ->
            btm <= n && n <= top

        Nothing ->
            False


passesParser : Parser Bool -> String -> Bool
passesParser parser s =
    case P.run parser s of
        Ok b ->
            b

        Err _ ->
            False


parseHcl : Parser Bool
parseHcl =
    P.succeed checkHcl
        |. P.symbol "#"
        |= P.getChompedString
            (P.chompWhile Char.isHexDigit)
        |. P.end


checkHcl : String -> Bool
checkHcl s =
    String.length s == 6


parsePid : Parser Bool
parsePid =
    P.succeed checkPid
        |= P.getChompedString
            (P.chompWhile Char.isDigit)
        |. P.end


checkPid : String -> Bool
checkPid s =
    String.length s == 9


parseHgt : Parser Bool
parseHgt =
    P.succeed checkHgt
        |= P.getChompedString
            (P.chompWhile Char.isDigit)
        |= P.getChompedString
            (P.chompWhile Char.isLower)
        |. P.end


checkHgt : String -> String -> Bool
checkHgt numeric unit =
    case ( String.toInt numeric, unit ) of
        ( Just n, "in" ) ->
            59 <= n && n <= 76

        ( Just n, "cm" ) ->
            150 <= n && n <= 193

        _ ->
            False


applyTest : HalfPassport -> ( String, String -> Bool ) -> Bool
applyTest hp ( key, test ) =
    Dict.get key hp |> Maybe.map test |> flattenMaybeBool


flattenMaybeBool : Maybe Bool -> Bool
flattenMaybeBool mb =
    case mb of
        Just b ->
            b

        Nothing ->
            False
