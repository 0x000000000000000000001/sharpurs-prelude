let showIntImpl (x: obj) : obj = box (string (unbox<int> x))
let showNumberImpl (x: obj) : obj = box (string (unbox<float> x))
let showStringImpl (x: obj) : obj = box ("\"" + unbox<string> x + "\"")
let showCharImpl c = string (unbox<char> c)
let showArrayImpl a = undefined
