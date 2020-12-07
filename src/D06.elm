module D06 exposing (main)

{-
   const groups = $0.innerText.split('\n\n');
   const intersect = (l, r) => new Set(
       [...l].filter(v => r.has(v))
   );
   Array.prototype.sumSizes = function() {
       return this.reduce((s,v) => s + v.size, 0);
   }
   console.log({
       part1: groups.map(
           g => new Set(g.trim().split(/\s?/))
       ).sumSizes(),

       part2: groups.map(
           g => g.trim().split('\n').map(x => new Set(x)).reduce(intersect)
       ).sumSizes()
   })
-}

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser as P exposing ((|.), (|=), Parser)
import Set exposing (Set)


type alias Model =
    String


type Msg
    = InputChanged String


rawSeparator : String
rawSeparator =
    "\n\n"


preppedSeparator : String
preppedSeparator =
    "#"


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
        [ processInput s |> H.text |> List.singleton |> H.pre []
        , H.textarea [ HA.value s, HE.onInput InputChanged ] []
        ]


prep : String -> String
prep s =
    (s |> String.split rawSeparator |> String.join preppedSeparator) ++ preppedSeparator


type alias Flier =
    Set Char


type alias Group =
    List Flier


parseGroups : Parser (List Group)
parseGroups =
    P.loop [] parseGroupsInner


parseGroupsInner : List Group -> Parser (P.Step (List Group) (List Group))
parseGroupsInner soFar =
    P.oneOf
        [ P.succeed (handleGroup soFar)
            |= P.loop [] parseGroupInner
            |. P.symbol preppedSeparator
        , P.succeed ()
            |> P.map (finalize soFar)
        ]


finalize : List a -> () -> P.Step (List a) (List a)
finalize soFar () =
    P.Done <| soFar


handleGroup :
    List Group
    -> Group
    -> P.Step (List Group) (List Group)
handleGroup soFar group =
    P.Loop <| group :: soFar


parseGroupInner : Group -> Parser (P.Step Group Group)
parseGroupInner soFar =
    P.oneOf
        [ P.succeed (handleFlier soFar)
            |= P.variable
                { inner = Char.isLower
                , reserved = Set.empty
                , start = Char.isLower
                }
            |. P.oneOf
                [ P.spaces
                , P.end
                ]
        , P.succeed ()
            |> P.map (finalize soFar)
        ]


handleFlier : Group -> String -> P.Step Group Group
handleFlier soFar flier =
    (flier |> String.toList |> Set.fromList) :: soFar |> P.Loop


processInput : String -> String
processInput s =
    case s |> prep |> P.run parseGroups of
        Ok groups ->
            "part1: "
                ++ (groups
                        |> List.map (uunion >> Set.size)
                        |> List.sum
                        |> String.fromInt
                   )
                ++ " part2: "
                ++ (groups
                        |> List.map (iintersect >> Set.size)
                        |> List.sum
                        |> String.fromInt
                   )

        Err errs ->
            List.map Debug.toString errs |> String.join "\n"


uunion : List (Set comparable) -> Set comparable
uunion =
    List.foldl Set.union Set.empty


iintersect : List (Set comparable) -> Set comparable
iintersect sets =
    case sets of
        s :: ss ->
            List.foldl Set.intersect s ss

        [] ->
            Set.empty
