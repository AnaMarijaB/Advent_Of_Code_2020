let naloga1 vsebina_datoteke =
    "10"

let naloga2 vsebina_datoteke =
    string_of_int (String.length vsebina_datoteke)

let nal vhod = 
    

    let min_max_meja meja = 
        let seznam = String.split_on_char '-' meja in 
        match seznam with 
        |min::max::[] -> int_of_string min, int_of_string max
        |_-> failwith "meja ni veljavna"
    in 

    let vrstica_v_slovar vrstica = 
        match String.split_on_char ' ' vrstica with
        | meja :: crka :: geslo :: [] -> (min_max_meja meja, crka.[0], geslo)
        |_ -> failwith "vrstica ni veljavna"
    in

    vrstica_v_slovar vhod

let prestej_crke_v_seznamu crka seznam = 
    let rec aux crka acc = function
        |x::xs ->  if x = crka then aux crka (acc+1) xs else aux crka acc xs
        |[] -> acc
    in
    aux crka 0 seznam
    

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
    let vsebina_datoteke = preberi_datoteko "proba_day_0/day_0.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "proba_day_0/day_0_1.out" odgovor1;
    izpisi_datoteko "proba_day_0/day_0_2.out" odgovor2