let arrayApply (fs: obj) (xs: obj) : obj =
    let fsArr = unbox<obj[]> fs
    let xsArr = unbox<obj[]> xs
    let lenF = fsArr.Length
    let lenX = xsArr.Length
    let result = Array.zeroCreate (lenF * lenX)
    let mutable n = 0
    for i = 0 to lenF - 1 do
        let f = fsArr.[i]
        for j = 0 to lenX - 1 do
            result.[n] <- sharpurs_apply f xsArr.[j]
            n <- n + 1
    result :> obj
