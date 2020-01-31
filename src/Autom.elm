module Autom exposing (..)

import Dict exposing (Dict)


type alias Node =
    { name : String
    , edges : Dict String String
    , final : Bool
    }


type alias DFA =
    { start : String
    , nodes : Dict String Node
    }


simpleDFA : DFA
simpleDFA =
    let
        q0 =
            { name = "q0"
            , edges = Dict.fromList [ ( "0", "q0" ), ( "1", "q1" ) ]
            , final = True
            }

        q1 =
            { name = "q1"
            , edges = Dict.fromList [ ( "0", "q2" ), ( "1", "q0" ) ]
            , final = False
            }

        q2 =
            { name = "q2"
            , edges = Dict.fromList [ ( "0", "q1" ), ( "1", "q2" ) ]
            , final = False
            }
    in
    { start = "q0", nodes = Dict.fromList [ ( "q0", q0 ), ( "q1", q1 ), ( "q2", q2 ) ] }


isValid : DFA -> String -> String -> Bool
isValid d name t =
    case Dict.get name d.nodes of
        Nothing ->
            False

        Just node ->
            if String.isEmpty t then
                node.final

            else
                case String.uncons t of
                    Nothing ->
                        False

                    Just ( c, tail ) ->
                        case Dict.get (String.fromChar c) node.edges of
                            Nothing ->
                                False

                            Just name1 ->
                                isValid d name1 tail
