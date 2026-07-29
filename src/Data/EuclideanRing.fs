let intDiv a b = (unbox<int> a) / (unbox<int> b)
let intDegree a = System.Math.Abs(unbox<int> a)
let numDiv a b = (unbox<float> a) / (unbox<float> b)
let intMod a b =
    let x = unbox<int> a
    let y = unbox<int> b
    if y = 0 then 0
    else
        let yy = System.Math.Abs(y)
        ((x % yy) + yy) % yy
