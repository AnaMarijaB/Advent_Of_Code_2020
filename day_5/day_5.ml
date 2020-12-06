let explode s = List.init (String.length s) (String.get s) 

let id_sedeza podatek = 
    let na_kose = explode podatek in
    let rec aux row_min row_max col_min col_max = function
        |[]-> row_min, col_max
        |x::xs -> 
            match x with
            |'F' -> aux row_min (row_max - ((row_max - row_min) / 2)-1) col_min col_max xs
            |'B' -> aux (row_min + ((row_max - row_min) / 2)+1) row_max col_min col_max xs
            |'R' -> aux row_min row_max (col_min + ((col_max-col_min)/2)+1) col_max xs
            |'L' -> aux row_min row_max col_min (col_max - ((col_max-col_min)/2)-1) xs
            |_ -> failwith "neveljavna koda"
    in
    let (vrsta, kolona) = aux 0 127 0 7 na_kose in
    vrsta * 8 + kolona 

let max_element list = 
    let rec aux acc = function
        |[] -> acc
        |x::xs -> if x >= acc then aux x xs else aux acc xs
    in
    aux 0 list

let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n' |> List.map id_sedeza |> max_element in
    string_of_int (input_list)

let rec manjkajoci_idji vsi_id st_vrstic manjkajo =  
    match st_vrstic with
    | 0 -> manjkajo
    | _ -> let rec aux sez row col man = match col with
        | (-1) -> manjkajoci_idji vsi_id (row-1) man
        | _ -> if (List.mem (row * 8 + col) sez) 
            then aux sez row (col - 1) man
            else aux sez row (col - 1) ((row * 8 + col)::man)
        in
        aux vsi_id st_vrstic 7 manjkajo

let rec moj_id vsi_id = function
    |[] -> failwith "noben ne manjka"
    |x::xs -> if (List.mem (x+1) vsi_id) && (List.mem (x-1) vsi_id) then x else moj_id vsi_id xs

let naloga2 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n' |> List.map id_sedeza  in
    let manjkajoci = manjkajoci_idji input_list 127 [] in
    let moj_sedez = moj_id input_list manjkajoci in
    string_of_int (moj_sedez)

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_5/day_5.in" in
    let odgovor1 = naloga1 vsebina_datoteke in
    let odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_5/day_5_1.out" odgovor1;
    izpisi_datoteko "day_5/day_5_2.out" odgovor2