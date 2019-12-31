module GenomeTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Genome


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
                    |> Expect.equal "Genome [0,1,2] [3] []"
            ]
        ]