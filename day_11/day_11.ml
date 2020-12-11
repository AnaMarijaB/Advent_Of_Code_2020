let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n'in
    input_list

 

(* let _ =
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
    let vsebina_datoteke = preberi_datoteko "day_6/day_6.in" in
    let odgovor1 = naloga1 vsebina_datoteke 
    in
    izpisi_datoteko "day_6/day_6_1.out" odgovor1; *)