let remove_x list = 
    let rec aux acc = function
        |[] ->acc
        |x::xs -> if x = "x" then aux acc xs else aux (x::acc) xs
    in
    aux [] list

let nasl_od_mojega moj_cas st_busa = 
    let rec aux moj bus stevec = 
        if stevec < moj then aux moj bus (stevec+bus) else stevec 
    in
    (aux moj_cas st_busa 0, st_busa)

let min_of_list list = 
    let rec aux (acc, bus) = function
        |[] -> acc, bus
        |(x, y)::xs -> if x < acc then aux (x, y) xs else aux (acc, bus) xs
    in 
    aux (List.hd list) list

let naloga1 vsebina = 
    let input_list = vsebina |> String.split_on_char '\n'in
    let head = int_of_string (List.hd input_list) in 
    let avtobusi = input_list |> List.tl |> List.hd |> String.split_on_char ',' |> remove_x |> List.map int_of_string in 
    let moji_odhodi = List.map (nasl_od_mojega head) avtobusi |> min_of_list in 
    string_of_int((fst moji_odhodi - head)*(snd moji_odhodi))



let test = "7,13,x,x,59,x,31,19"


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
    let vsebina_datoteke = preberi_datoteko "day_13/day_13.in" in
    let odgovor1 = naloga1 vsebina_datoteke 
    in
    izpisi_datoteko "day_13/day_13_1.out" odgovor1;