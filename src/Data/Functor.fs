let arrayMap = box (fun (f: obj) -> box (fun (arr: obj) ->
    let a = unbox<obj[]> arr
    let l = a.Length
    let result = Array.zeroCreate<obj> l
    for i = 0 to l - 1 do
        result.[i] <- sharpurs_apply f a.[i]
    box result
))
