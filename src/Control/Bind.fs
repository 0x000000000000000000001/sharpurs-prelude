let arrayBind =
    fun (xs: obj) -> fun (f: obj) ->
        let arr = unbox<obj[]> xs
        let result = System.Collections.Generic.List<obj>()
        for x in arr do
            let res = unbox<obj[]> (sharpurs_apply f x)
            result.AddRange(res)
        result.ToArray() :> obj
