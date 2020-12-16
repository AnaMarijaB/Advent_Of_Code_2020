#use "topfind"
#require "str"
open Str

let ena_omejitev sez = 
    let rec aux prazen = function
        |[x; y] -> if x > y then prazen else aux  (x::prazen) [x+1; y]
        |_ -> failwith "pogoj ni ustrezen"
    in
    aux [] sez

let seznam_ustreznih_stevil sez = 
    let stevila = sez |> List.map (String.split_on_char '-') |> List.map (List.map int_of_string) |> List.map (ena_omejitev) in
    let rec aux prazen = function
        |[] -> prazen 
        |x::xs -> match x with 
            |y::ys -> aux (y::prazen) (ys::xs) 
            |[]-> aux prazen xs 
    in 
    aux [] stevila

let seznam_st_na_vstopnicah sez = 
    let rec aux prazen = function
        |[] -> prazen 
        |x::xs -> match x with 
            |y::ys -> aux (y::prazen) (ys::xs)
            |[] -> aux prazen xs
    in
    aux [] sez 

let sez_neustreznih_st pogoji moje_st = 
    let rec aux stevilo pog = function
        |x::xs -> if List.mem x pog then aux stevilo pog xs else aux (stevilo+x) pog xs
        |[] -> stevilo
    in
    aux 0 pogoji moje_st

let naloga1 vsebina =
    let r = Str.regexp "\n\n" in
    let input = vsebina |> Str.split r in
    
    let st = Str.regexp ".*: \\([0-9]+-[0-9]+\\) or \\([0-9]+-[0-9]+\\)" in
    let omejitve = input |> List.hd |> String.split_on_char '\n' in 
    let prve_st = List.map (Str.replace_first st "\\1") omejitve in 
    let druge_st = List.map (Str.replace_first st "\\2" ) omejitve in
    let pogoji = prve_st @ druge_st in
    let valid = seznam_ustreznih_stevil pogoji in

    let zadnji_del = List.nth input 2 in
    let tickets = zadnji_del |> String.split_on_char '\n' |> List.tl |> List.map (String.split_on_char ',') |> 
        List.map (List.map int_of_string) |> seznam_st_na_vstopnicah in
    
    string_of_int (sez_neustreznih_st valid tickets)

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
    let vsebina_datoteke = preberi_datoteko "day_16/day_16.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    in
    izpisi_datoteko "day_16/day_16_1.out" odgovor1;