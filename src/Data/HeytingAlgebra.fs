let boolConj (a: obj) (b: obj) = box ((unbox<bool> a) && (unbox<bool> b))
let boolDisj (a: obj) (b: obj) = box ((unbox<bool> a) || (unbox<bool> b))
let boolNot (a: obj) = box (not (unbox<bool> a))
