let concatString a b = (unbox<string> a) + (unbox<string> b)
let concatArray (xs: obj) (ys: obj) =
    let arrX = unbox<obj[]> xs
    let arrY = unbox<obj[]> ys
    if arrX.Length = 0 then ys
    elif arrY.Length = 0 then xs
    else
        let res = Array.zeroCreate (arrX.Length + arrY.Length)
        Array.Copy(arrX, 0, res, 0, arrX.Length)
        Array.Copy(arrY, 0, res, arrX.Length, arrY.Length)
        box res
