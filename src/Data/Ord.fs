let ordIntImpl lt eq gt x y = let x' = unbox<int> x in let y' = unbox<int> y in if x' < y' then lt else if x' = y' then eq else gt
let ordNumberImpl lt eq gt x y = let x' = unbox<float> x in let y' = unbox<float> y in if x' < y' then lt else if x' = y' then eq else gt
let ordStringImpl lt eq gt x y = let x' = unbox<string> x in let y' = unbox<string> y in if x' < y' then lt else if x' = y' then eq else gt
let ordCharImpl lt eq gt x y = let x' = unbox<char> x in let y' = unbox<char> y in if x' < y' then lt else if x' = y' then eq else gt
let ordBooleanImpl lt eq gt x y = let x' = unbox<bool> x in let y' = unbox<bool> y in if x' < y' then lt else if x' = y' then eq else gt

// Mirrors Data/Ord.js: compare element by element with the delta function,
// then compare lengths (a longer array sorts before a shorter one).
let ordArrayImpl (f: obj) (xs: obj) (ys: obj) : obj =
    let xarr = unbox<obj[]> xs
    let yarr = unbox<obj[]> ys
    let mutable index = 0
    let mutable result = 0

    while result = 0 && index < xarr.Length && index < yarr.Length do
        let delta = unbox<int> (sharpurs_apply (sharpurs_apply f (xarr.[index])) (yarr.[index]))
        if delta <> 0 then result <- delta
        index <- index + 1

    if result <> 0 then box result
    elif xarr.Length = yarr.Length then box 0
    elif xarr.Length > yarr.Length then box -1
    else box 1
