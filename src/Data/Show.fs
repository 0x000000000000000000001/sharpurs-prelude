let showIntImpl (x: obj) : obj = box (string (unbox<int> x))

// Mirrors the ECMAScript Number::toString layout: shortest round-trip digits
// with plain notation for 10^-6 < |x| < 10^21 and exponent notation outside
// that range. .NET's "R" gives the same digits but a different layout.
let jsNumberToString (n: float) : string =
    if System.Double.IsNaN n then "NaN"
    elif System.Double.IsPositiveInfinity n then "Infinity"
    elif System.Double.IsNegativeInfinity n then "-Infinity"
    elif n = 0.0 then "0"
    else
        let r = n.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
        let sign, body = if r.StartsWith("-") then "-", r.Substring(1) else "", r
        let mantissa, expPart =
            let idx = body.IndexOfAny([| 'E'; 'e' |])
            if idx >= 0 then body.Substring(0, idx), body.Substring(idx + 1) else body, "0"
        let e = System.Int32.Parse(expPart, System.Globalization.CultureInfo.InvariantCulture)
        let pointIdx =
            let idx = mantissa.IndexOf('.')
            if idx >= 0 then idx else mantissa.Length
        let allDigits = mantissa.Replace(".", "")
        let firstNonZero = allDigits |> Seq.tryFindIndex (fun c -> c <> '0') |> Option.defaultValue 0
        let stripped = allDigits.Substring(firstNonZero).TrimEnd('0')
        let digits = if stripped = "" then "0" else stripped
        let k = digits.Length
        let nPos = (pointIdx - firstNonZero) + e
        let rendered =
            if k <= nPos && nPos <= 21 then
                digits + String.replicate (nPos - k) "0"
            elif 0 < nPos && nPos <= 21 then
                digits.Substring(0, nPos) + "." + digits.Substring(nPos)
            elif -6 < nPos && nPos <= 0 then
                "0." + String.replicate (-nPos) "0" + digits
            else
                let mant = if k = 1 then digits else digits.Substring(0, 1) + "." + digits.Substring(1)
                let exp = nPos - 1
                let expText = if exp >= 0 then "+" + string exp else string exp
                mant + "e" + expText
        sign + rendered

// Mirrors the JS backend: integral values keep one decimal ("4.0"), other
// values use the round-trip representation.
let showNumberImpl (x: obj) : obj =
    let str = jsNumberToString (unbox<float> x)
    let candidate = str + ".0"
    let parsed = fst (System.Double.TryParse(candidate, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture))
    box (if parsed then candidate else str)

// Mirrors the JS backend: single quotes and the same escapes.
let showCharImpl (x: obj) : obj =
    let c = unbox<char> x
    let code = int c
    let body =
        if code < 0x20 || code = 0x7F then
            match c with
            | '\a' -> "\\a"
            | '\b' -> "\\b"
            | '\f' -> "\\f"
            | '\n' -> "\\n"
            | '\r' -> "\\r"
            | '\t' -> "\\t"
            | '\v' -> "\\v"
            | _ -> "\\" + code.ToString(System.Globalization.CultureInfo.InvariantCulture)
        elif c = '\'' || c = '\\' then "\\" + string c
        else string c
    box ("'" + body + "'")

// Mirrors the JS backend escaping for control characters, quotes and
// backslashes, including the "\&" disambiguation before a digit.
let showStringImpl (x: obj) : obj =
    let s = unbox<string> x
    let sb = System.Text.StringBuilder()
    sb.Append('"') |> ignore
    let mutable i = 0
    while i < s.Length do
        let c = s.[i]
        let code = int c
        if code < 0x20 || code = 0x7F || c = '"' || c = '\\' then
            match c with
            | '"' | '\\' -> sb.Append('\\').Append(c) |> ignore
            | '\a' -> sb.Append("\\a") |> ignore
            | '\b' -> sb.Append("\\b") |> ignore
            | '\f' -> sb.Append("\\f") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | '\v' -> sb.Append("\\v") |> ignore
            | _ ->
                sb.Append('\\').Append(code.ToString(System.Globalization.CultureInfo.InvariantCulture)) |> ignore
                let next = i + 1
                if next < s.Length && s.[next] >= '0' && s.[next] <= '9' then sb.Append("\\&") |> ignore
        else
            sb.Append(c) |> ignore
        i <- i + 1
    sb.Append('"') |> ignore
    box (sb.ToString())

let showArrayImpl (f: obj) (xs: obj) =
    let xs' = unbox<obj[]> xs
    let mutable res = "["
    for i = 0 to xs'.Length - 1 do
        res <- res + unbox<string> (Sharpurs_Prelude.sharpurs_apply f xs'.[i])
        if i < xs'.Length - 1 then
            res <- res + ","
    res <- res + "]"
    box res
