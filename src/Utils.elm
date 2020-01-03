module Utils exposing (format, s)

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