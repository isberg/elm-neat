module Genome exposing (Genome, createUnconnected, createConnected, distance, toString)

import Connection exposing (Connection, Cache)
import List
import Random exposing (Generator)
import Utils exposing (format, s)
import IntDict exposing (IntDict)

type Genome = Genome 
    { sensors : List Int
    , outputs : List Int
    , hidden : List Int
    , connections : IntDict Connection
    }

createUnconnected : Int -> Int -> Genome
createUnconnected sensors outputs =
    Genome 
    { sensors = List.range 0 sensors
    , outputs = List.range (sensors + 1) (sensors + outputs)
    , hidden = []
    , connections = IntDict.fromList []
    }

createConnected : Int -> Int -> Cache -> Generator (Genome, Cache)
createConnected sensors outputs cache =
    let
        (Genome genome) = createUnconnected sensors outputs 
        (updatedCache, generator) = Connection.generateAll genome.sensors genome.outputs cache
    in
    generator |> Random.map (\connections -> (Genome { genome | connections = connections }, updatedCache) )


distance : Genome -> Genome -> Float
distance (Genome a) (Genome b) =
    Connection.distance 1 0.5 a.connections b.connections

toString : Genome -> String
toString genome =
    let
        c2s = 
            IntDict.values
            >> List.map Connection.toString
            >> String.join ", "
    in
    case genome of
        Genome {sensors, outputs, hidden, connections} ->
            [s sensors, s outputs, s hidden, c2s connections] |> format "Genome {0} {1} {2} [{3}]"
