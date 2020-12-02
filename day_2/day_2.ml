let min_max_meja meja = 
    let seznam = String.split_on_char '-' meja in 
    match seznam with 
    |min::max::[] -> int_of_string min, int_of_string max
    |_-> failwith "meja ni veljavna"

let vrstica_v_slovar vrstica = 
     match String.split_on_char ' ' vrstica with
    | meja :: crka :: geslo :: [] -> (min_max_meja meja, crka.[0], geslo)
    |_ -> failwith "vrstica ni veljavna"

let explode s = List.init (String.length s) (String.get s) 

let rec prestej_crke_v_seznamu crka seznam = 
     let rec aux crka acc = function
         |x::xs ->  if x = crka then aux crka (acc+1) xs else aux crka acc xs
         |[] -> acc
     in
    aux crka 0 seznam

let geslo_je_pravilno vrstica = 
    let (meja, crka, geslo) = vrstica_v_slovar vrstica in 
    let najmanjsi, najvecji = meja in 
    let stevilo_crk = prestej_crke_v_seznamu crka (explode geslo) in
    (najmanjsi <= stevilo_crk) && (stevilo_crk <= najvecji)

let rec stevilo_pravilnih_gesel seznam = 
     let rec aux acc = function
     |[] -> acc
     |x::xs -> if geslo_je_pravilno x then aux (acc+1) xs else aux acc xs
     in
     aux 0 seznam

let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n'in
    string_of_int (stevilo_pravilnih_gesel input_list)

let geslo_je_pravilno2 vrstica = 
    let (meja, crka, geslo) = vrstica_v_slovar vrstica in 
    let najmanjsi, najvecji = meja in 
    let min = (geslo.[najmanjsi-1]=crka) in 
    let max = (geslo.[najvecji -1]=crka) in 
    (min || max) && (not min || not max)

let rec stevilo_pravilnih_gesel2 seznam = 
     let rec aux acc = function
     |[] -> acc
     |x::xs -> if geslo_je_pravilno2 x then aux (acc+1) xs else aux acc xs
     in
     aux 0 seznam
    
let naloga2 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n'in
    string_of_int (stevilo_pravilnih_gesel2 input_list)

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
    let vsebina_datoteke = preberi_datoteko "day_2/day_2.in" in
    let odgovor1 = naloga1 vsebina_datoteke in
    let odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_2/day_2_1.out" odgovor1;
    izpisi_datoteko "day_2/day_2_2.out" odgovor2