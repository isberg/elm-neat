module Genome exposing (Genome, createUnconnected, createConnected, toString)

import List
import Random exposing (Generator)
import List.Extra

type Genome = Genome 
    { sensors : List Int
    , outputs : List Int
    , hidden : List Int
    , connections : List (Int, Int)
    }

createUnconnected : Int -> Int -> Genome
createUnconnected sensors outputs =
    Genome 
    { sensors = List.range 0 sensors
    , outputs = List.range (sensors + 1) (sensors + outputs)
    , hidden = []
    , connections = []
    }

createConnected : Int -> Int -> Float -> Int -> List a -> Generator (Genome, Int, List a)
createConnected sensors outputs weightSpan id cache =
    let
        (Genome genome) = createUnconnected sensors outputs 
        edges : List (Int, Int)
        edges = 
            List.Extra.lift2 (\from to -> (from, to)) genome.sensors genome.outputs
            --|> List.indexedMap (\i (f, t) -> (i, f, t))
    in
    (Genome {genome | connections = edges}, id, cache) |> Random.constant

empty : Genome
empty = Genome {sensors = [], outputs = [], hidden = [], connections = []}

toString : Genome -> String
toString genome =
    let
        c2s = 
            List.map (\(from, to) -> [s from, s to] |> format "{0}->{1}")
            >> String.join ","
    in
    case genome of
        Genome {sensors, outputs, hidden, connections} ->
            [s sensors, s outputs, s hidden, c2s connections] |> format "Genome {0} {1} {2} [{3}]"

s : a -> String
s = Debug.toString

format : String -> List String -> String
format template values =
    let
        format_rec index vals templ =
            case vals of
                [] -> templ
                head::tail -> 
                    templ 
                    |> String.replace ("{" ++ (index |> String.fromInt) ++ "}") head
                    |> format_rec (index + 1) tail
    in
    format_rec 0 values template 