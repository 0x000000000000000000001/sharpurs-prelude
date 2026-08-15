let absInt (n: int) =
    if n = System.Int32.MinValue then System.Int32.MaxValue
    else System.Math.Abs(n)

let intDiv a b = 
    let x = unbox<int> a
    let y = unbox<int> b
    if y = 0 then 0
    else
        let yy = absInt y
        let m = ((x % yy) + yy) % yy
        if y = -1 && x = System.Int32.MinValue then System.Int32.MinValue
        else ((x - m) / y)
        
let intDegree a = absInt (unbox<int> a)
let numDiv a b = (unbox<float> a) / (unbox<float> b)
let intMod a b =
    let x = unbox<int> a
    let y = unbox<int> b
    if y = 0 then 0
    else
        let yy = absInt y
        ((x % yy) + yy) % yy
