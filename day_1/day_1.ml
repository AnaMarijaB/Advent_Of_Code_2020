let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n' |> List.filter (fun s -> s <> "") in
    let stevila = List.map int_of_string input_list in

    let rec vsota_dveh_2020 = function
        |[]-> failwith "seznam je prazen"
        |x::xs -> if List.mem (2020-x) xs then x * (2020-x) else vsota_dveh_2020 xs
    in 

    string_of_int (vsota_dveh_2020 stevila)

let naloga2 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n' |> List.filter (fun s -> s <> "") in
    let stevila = List.map int_of_string input_list in

    let rec vsota_treh_2020 vhodni_seznam = 
        let rec aux acc = function 
            |[] -> 0
            | x::xs -> if List.mem (acc-x) xs then x * (acc-x) else aux acc xs
        in
        match vhodni_seznam with
        |[]-> failwith "seznam je prazen"
        |x::xs -> 
            let z = aux (2020-x) xs in
            if z = 0 then vsota_treh_2020 xs else z * x
    in 

    string_of_int (vsota_treh_2020 stevila)

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
    let vsebina_datoteke = preberi_datoteko "day_1/day_1.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_1/day_1_1.out" odgovor1;
    izpisi_datoteko "day_1/day_1_2.out" odgovor2