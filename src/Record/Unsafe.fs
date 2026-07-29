let unsafeHas = box (fun (k: obj) -> box (fun (map: obj) -> box (Map.containsKey (unbox<string> k) (unbox<Map<string, obj>> map))))
let unsafeGet = box (fun (k: obj) -> box (fun (map: obj) -> Map.find (unbox<string> k) (unbox<Map<string, obj>> map)))
let unsafeSet = box (fun (k: obj) -> box (fun (v: obj) -> box (fun (map: obj) -> box (Map.add (unbox<string> k) v (unbox<Map<string, obj>> map)))))
let unsafeDelete = box (fun (k: obj) -> box (fun (map: obj) -> box (Map.remove (unbox<string> k) (unbox<Map<string, obj>> map))))
