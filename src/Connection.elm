module Connection exposing (Connection, Cache, create, toString, fromInnovation, generateAll, createCache, distance)

import Dict exposing (Dict)
import Utils exposing (format, s)
import Random exposing (Generator)
import Random.Extra exposing (combine)
import List.Extra exposing (lift2)
import IntDict exposing (IntDict)

weightSpan : Float
weightSpan = 3.0

type Connection = Connection
    { innovationId : Int
    , from : Int
    , to : Int
    , weight : Float
    , enabled : Bool
    }

type alias Cache = 
    { nextInnovation : Int
    , cache : InnovationDict
    }

type alias InnovationDict = Dict Edge Int

type alias Innovation =
    { id : Int
    , edge : Edge
    }

type alias Edge = ( Int, Int )


--------------- CONNECTION ------------------

create : Int -> Int -> Int -> Float -> Bool -> Connection
create innovationId from to weight enabled =
    Connection 
        { innovationId = innovationId 
        , from = from
        , to = to
        , weight = weight
        , enabled = enabled
        }

fromInnovation : Innovation -> Generator Connection
fromInnovation {id, edge} =
    let
        (from, to) = edge
    in
    Random.map 
        (\weight -> create id from to weight True) 
        (Random.float -weightSpan weightSpan)

generateAll : List Int -> List Int -> Cache -> (Cache, Generator (IntDict Connection))
generateAll inputs outputs cache =
    let
        edges = calculateUnconnected inputs outputs []
        (updatedCache, innovations) = createInnovations cache edges
        generator : Generator (IntDict Connection)
        generator = 
            innovations |> IntDict.values |> List.map fromInnovation |> combine 
            |> Random.map (\list -> list |> List.map (\(Connection c) -> (c.innovationId, Connection c)) |> IntDict.fromList)
    in
    ( updatedCache
    , generator
    )


distance : Float -> Float -> IntDict Connection -> IntDict Connection -> Float
distance c12 c3 x y = 
    let
        excess_or_disjoint = IntDict.diff (IntDict.union x y) (IntDict.intersect x y) |> IntDict.size |> toFloat
        getCommonWeights : IntDict Connection -> IntDict Connection -> List Float
        getCommonWeights alpha beta = IntDict.intersect alpha beta |> IntDict.values |> List.map (\(Connection c) -> c.weight)
        x_weights = getCommonWeights x y
        y_weights = getCommonWeights y x
        weightDiff = List.map2 ((-)) x_weights y_weights |> List.sum
    in
    c12 * excess_or_disjoint + c3 * weightDiff


toString : Connection -> String
toString (Connection {innovationId, from, to, weight}) = 
    [s innovationId, s from, s to, s weight] |> format "{0}:{1}->{2}={3}"


--------------- INNOVATION ---------------------

createCache : Int -> Cache
createCache nextInnovation =
    Cache nextInnovation Dict.empty


createInnovations : Cache -> List Edge -> (Cache, IntDict Innovation)
createInnovations incomingCache edges =
    let
        f : Edge -> (Cache, IntDict Innovation) -> (Cache, IntDict Innovation)
        f edge (cache, list) =
            let
                (updatedCache, innovation) = createInnovation cache edge
            in
            ( updatedCache
            , list |> IntDict.insert innovation.id innovation
            )
    in
    List.foldl f (incomingCache, IntDict.empty) edges

createInnovation : Cache -> Edge -> (Cache, Innovation)
createInnovation {nextInnovation, cache} edge =
    case cache |> Dict.get edge of
        Just id -> 
            ( Cache nextInnovation cache
            , Innovation id edge
            )
        Nothing ->
            ( Cache (nextInnovation + 1) (cache |> Dict.insert edge nextInnovation)
            , Innovation nextInnovation edge
            )

calculateUnconnected : List Int -> List Int -> List Edge -> List Edge
calculateUnconnected inputs outputs connected =
    let
        toDict = 
            List.map (\edge -> (edge, True)) 
            >> Dict.fromList
        fromDict = Dict.keys
        diff a b = Dict.diff (toDict a) (toDict b) |> fromDict
        all = lift2 (\i o -> (i, o)) inputs outputs 
        unconnected = diff all connected
    in
    unconnected
