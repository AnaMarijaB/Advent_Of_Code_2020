let stevilo_dreves seznam zamik dolzina_vrstice= 
    let rec aux acc d = function
        |[]-> acc
        |x::xs -> 
            if x.[d] = '#' then aux (acc+1) ((d + zamik) mod dolzina_vrstice) xs 
            else aux (acc) ((d + zamik) mod dolzina_vrstice) xs
    in
    aux 0 0 seznam 

let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n'in
    string_of_int (stevilo_dreves input_list 3 31)

let rec lihi_elementi_seznama = function (*če začnemo šteti elemente od 1 naprej*)
    |[] -> []
    |x::y::xs -> x::(lihi_elementi_seznama xs)
    |x::[] -> x::[]

let naloga2 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n'in
    let ena_ena = stevilo_dreves input_list 1 31 in
    let tri_ena = stevilo_dreves input_list 3 31 in
    let pet_ena = stevilo_dreves input_list 5 31 in
    let sedem_ena = stevilo_dreves input_list 7 31 in
    let ena_dva = stevilo_dreves (lihi_elementi_seznama input_list) 1 31 in
    string_of_int (ena_ena*tri_ena*pet_ena*sedem_ena*ena_dva)

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
    let vsebina_datoteke = preberi_datoteko "day_3/day_3.in" in
    let odgovor1 = naloga1 vsebina_datoteke in
    let odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_3/day_3_1.out" odgovor1;
    izpisi_datoteko "day_3/day_3_2.out" odgovor2