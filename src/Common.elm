module Common exposing (mapIf, until)


mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf p f list =
    List.map (until p f) list


until : (a -> Bool) -> (a -> a) -> a -> a
until p f a =
    if p a then
        f a

    else
        a

