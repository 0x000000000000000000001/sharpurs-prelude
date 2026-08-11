let showIntImpl (x: obj) : obj = box (string (unbox<int> x))
let showNumberImpl (x: obj) : obj = box (string (unbox<float> x))
let showStringImpl (x: obj) : obj = box ("\"" + unbox<string> x + "\"")
let showCharImpl c = string (unbox<char> c)
let showArrayImpl (f: obj) (xs: obj) =
    let xs' = unbox<obj[]> xs
    let mutable res = "["
    for i = 0 to xs'.Length - 1 do
        res <- res + unbox<string> (Sharpurs_Prelude.sharpurs_apply f xs'.[i])
        if i < xs'.Length - 1 then
            res <- res + ","
    res <- res + "]"
    box res
