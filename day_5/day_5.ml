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

let seznam_vseh_id = 
    let range a b = List.init (b - a) ((+) a) in
    let vrste = range 0 128 in 
    let kolone = range 0 8 in
    let rec aux acc v k = 
        match v with
        |[] -> acc
        |x::xs -> match k with 
            |[] -> aux xs k 
            |y::ys -> ((x*8+y)::acc) xs ys 
    in 
    aux [] vrste kolone 

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
    let odgovor1 = naloga1 vsebina_datoteke
    in
    izpisi_datoteko "day_5/day_5_1.out" odgovor1;