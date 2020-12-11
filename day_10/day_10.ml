let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n' |> List.map int_of_string |> List.sort compare  in

    let rec aux diff1 diff3 = function
        |[]|_::[] -> diff1 * (diff3 + 1)
        |x::y::xs -> match (y-x) with
            |1 -> aux (diff1 + 1) diff3 (y::xs)
            |3 -> aux diff1 (diff3 + 1) (y::xs)
            |_ -> failwith "Nekaj ni vredu."
    in

    string_of_int (aux 0 0 (0::input_list))

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
    let vsebina_datoteke = preberi_datoteko "day_10/day_10.in" in
    let odgovor1 = naloga1 vsebina_datoteke 
    in
    izpisi_datoteko "day_10/day_10_1.out" odgovor1;