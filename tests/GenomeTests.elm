module GenomeTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Genome exposing (Genome)
import Connection exposing (Cache)
import Random exposing (Generator)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Genome module"
        [ describe "a Genome"
            [ test "can be created unconnected" <|
                \_ ->
                    let 
                        sensors = 2
                        outputs = 1 
                    in
                    Genome.createUnconnected sensors outputs 
                    |> Genome.toString 
                    |> Expect.equal "Genome [0,1,2] [3] [] []"
            , test "can be created fully connected" <|
                \_ -> 
                    let 
                        sensors = 2
                        outputs = 1
                        nextInnovation = 1
                        innovationCache = Connection.createCache nextInnovation
                        seed = Random.initialSeed 0

                        sut = Genome.createConnected 
                            sensors 
                            outputs 
                            innovationCache
                        ((actual, _), _) = Random.step sut seed
                    in
                    actual
                    |> Genome.toString
                    |> Expect.equal "Genome [0,1,2] [3] [] [1:0->3=-2.4657153169026884, 2:1->3=-1.91318726038014, 3:2->3=0.5688122444798633]"
            ]
        , describe "multiple Genomes can be created"
            [ test "unconnected" <|
                \_ ->
                    let
                        sensors = 1
                        outputs = 1
                        count = 2
                        expectedGenome = "Genome [0,1] [2] [] []"
                    in
                    Genome.createManyUnconnected sensors outputs count
                    |> List.map Genome.toString 
                    |> Expect.equal [expectedGenome, expectedGenome]
            , test "fully connected" <|
                \_ ->
                    let
                        sensors = 0
                        outputs = 1
                        count = 2
                        nextInnovation = 1
                        innovationCache = Connection.createCache nextInnovation
                        seed = Random.initialSeed 0
                        sut = Genome.createManyConnected sensors outputs count innovationCache
                        ((actual, _), _) = Random.step sut seed
                    in
                    actual
                    |> List.map Genome.toString 
                    |> Expect.equal 
                        [ "Genome [0] [1] [] [1:0->1=-2.4657153169026884]"
                        , "Genome [0] [1] [] [1:0->1=-1.91318726038014]"
                        ]
            ]
        , describe "the distance between two genomes can be calculated"
            [ test "when there are no innovations at all" <|
                \_ ->
                    let
                        sensors = 1
                        outputs = 1
                        a = Genome.createUnconnected sensors outputs
                        b = Genome.createUnconnected sensors outputs

                    in
                    Genome.distance a b |> Expect.equal 0.0
            , test "when they are fully connected" <|
                \_ ->
                    let
                        sensors = 1
                        outputs = 1
                        nextInnovation = 1
                        innovationCache = Connection.createCache nextInnovation
                        seed = Random.initialSeed 0
                        generator : Cache -> Generator (Genome, Cache)
                        generator = Genome.createConnected sensors outputs

                        ((first, updatedCache), updatedSeed) = Random.step (generator innovationCache) seed
                        ((second, _), _) = Random.step (generator updatedCache) updatedSeed
                    in
                    Genome.distance first second |> Expect.within error -2.7896
            ]
        ]
error : FloatingPointTolerance
error = Absolute 0.0001