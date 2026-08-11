let eqIntImpl (a: obj) (b: obj) = unbox<int> a = unbox<int> b
let eqNumberImpl (a: obj) (b: obj) = unbox<float> a = unbox<float> b
let eqStringImpl (a: obj) (b: obj) = unbox<string> a = unbox<string> b
let eqCharImpl (a: obj) (b: obj) = unbox<char> a = unbox<char> b
let eqBooleanImpl (a: obj) (b: obj) = unbox<bool> a = unbox<bool> b
let eqArrayImpl (f: obj) (xs: obj) (ys: obj) =
    let xs' = unbox<obj[]> xs
    let ys' = unbox<obj[]> ys
    printfn "eqArrayImpl: xs.Length=%d, ys.Length=%d" xs'.Length ys'.Length
    if xs'.Length <> ys'.Length then box false
    else
        let mutable eq = true
        for i = 0 to xs'.Length - 1 do
            let isEq = unbox<bool> (sharpurs_apply (sharpurs_apply f xs'.[i]) ys'.[i])
            printfn "eqArrayImpl: xs[%d]=%A, ys[%d]=%A, isEq=%b" i xs'.[i] i ys'.[i] isEq
            if not isEq then
                eq <- false
        printfn "eqArrayImpl: eq=%b" eq
        box eq
