module Autom exposing (DFA, isValid, simpleDFA)

import Dict exposing (Dict)
import Set exposing (Set)


type alias DFA =
    { states : Set String
    , alpha : Set String
    , trans : Dict ( String, String ) String
    , start : String
    , final : Set String
    }


simpleDFA : DFA
simpleDFA =
    { states = Set.fromList [ "q0", "q1", "q2" ]
    , alpha = Set.fromList [ "0", "1" ]
    , trans =
        Dict.fromList
            [ ( ( "q0", "0" ), "q0" )
            , ( ( "q0", "1" ), "q1" )
            , ( ( "q1", "0" ), "q2" )
            , ( ( "q1", "1" ), "q0" )
            , ( ( "q2", "0" ), "q1" )
            , ( ( "q2", "1" ), "q2" )
            ]
    , start = "q0"
    , final = Set.fromList [ "q0" ]
    }


isValid : DFA -> String -> String -> Bool
isValid d state str =
    if String.isEmpty str then
        Set.member state d.final

    else
        case String.uncons str of
            Nothing ->
                False

            Just ( c, tail ) ->
                Dict.get ( state, String.fromChar c ) d.trans
                    |> Maybe.map (\name -> isValid d name tail)
                    |> Maybe.withDefault False
