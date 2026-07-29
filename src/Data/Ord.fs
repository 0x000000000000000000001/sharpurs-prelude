let ordIntImpl lt eq gt x y = let x' = unbox<int> x in let y' = unbox<int> y in if x' < y' then lt else if x' = y' then eq else gt
let ordNumberImpl lt eq gt x y = let x' = unbox<float> x in let y' = unbox<float> y in if x' < y' then lt else if x' = y' then eq else gt
let ordStringImpl lt eq gt x y = let x' = unbox<string> x in let y' = unbox<string> y in if x' < y' then lt else if x' = y' then eq else gt
let ordCharImpl lt eq gt x y = let x' = unbox<char> x in let y' = unbox<char> y in if x' < y' then lt else if x' = y' then eq else gt
let ordBooleanImpl lt eq gt x y = let x' = unbox<bool> x in let y' = unbox<bool> y in if x' < y' then lt else if x' = y' then eq else gt
let ordArrayImpl = undefined
