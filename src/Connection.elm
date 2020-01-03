module Connection exposing (Connection, Cache, create, toString, fromInnovation, generateAll, createCache)

import Dict exposing (Dict)
import Utils exposing (format, s)
import Random exposing (Generator)
import Random.Extra exposing (combine)
import List.Extra exposing (lift2)

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
    , cache : Dict Edge Int
    }

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

generateAll : List Int -> List Int -> Cache -> (Cache, Generator (List Connection))
generateAll inputs outputs cache =
    let
        edges = lift2 (\from to -> (from, to)) inputs outputs
        (updatedCache, innovations) = createInnovations cache edges
        generator = innovations |> List.map fromInnovation |> combine
    in
    ( updatedCache
    , generator
    )


toString : Connection -> String
toString (Connection {innovationId, from, to, weight}) = 
    [s innovationId, s from, s to, s weight] |> format "{0}:{1}->{2}={3}"


--------------- INNOVATION ---------------------

createCache : Int -> Cache
createCache nextInnovation =
    Cache nextInnovation Dict.empty


createInnovations : Cache -> List Edge -> (Cache, List Innovation)
createInnovations incomingCache edges =
    --foldl : (a -> b -> b) -> b -> List a -> b
    --foldl : (Edge -> (Cache, List Innovation) -> (Cache, List Innovation)) -> (Cache, List Innovation) -> List Edge -> (Cache, List Innovation)
    let
        f : Edge -> (Cache, List Innovation) -> (Cache, List Innovation)
        f edge (cache, list) =
            let
                (updatedCache, innovation) = createInnovation cache edge
            in
            ( updatedCache
            , list ++ [innovation]
            )
    in
    List.foldl f (incomingCache, []) edges

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
