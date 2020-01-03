module GenomeTests exposing (suite)

import Expect
import Genome
import Connection
import Random
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
        ]