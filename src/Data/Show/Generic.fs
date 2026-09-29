let intercalate (separator: obj) (values: obj) : obj =
    let parts = unbox<obj[]> values |> Array.map unbox<string>
    box (System.String.Join(unbox<string> separator, parts))
