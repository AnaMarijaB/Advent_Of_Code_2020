let zmnozi x y = x * y

let uredi sez =
    List.sort compare sez

let uredi_po'' f sez =
    sez
    |> List.mapi (fun i x -> (f x, i, x))
    |> uredi
    |> List.map (fun (_, _, x) -> x)

let rec fakulteta n = 
    if n = 0 then 1 else n * fakulteta (n-1)

let rec fakulteta' n = 
    let rec aux acc n =
        if n = 0 then acc else aux (n*acc) (n-1)
    in 
    aux 1 n