module Genome exposing (Genome, createUnconnected, toString)

import List

type Genome = Genome 
    { sensors : List Int
    , outputs : List Int
    , hidden : List Int
    }

createUnconnected : Int -> Int -> Genome
createUnconnected sensors outputs =
    Genome 
    { sensors = List.range 0 sensors
    , outputs = List.range (sensors + 1) (sensors + outputs)
    , hidden = []
    }

toString : Genome -> String
toString genome =
    case genome of
        Genome {sensors, outputs, hidden} ->
            [sensors, outputs, hidden] |> format "Genome {0} {1} {2}"
     

format : String -> List a -> String
format template values =
    let
        format_rec index vals templ =
            case vals of
                [] -> templ
                head::tail -> 
                    templ 
                    |> String.replace ("{" ++ (index |> String.fromInt) ++ "}") (head |> Debug.toString)
                    |> format_rec (index + 1) tail
    in
    format_rec 0 values template 