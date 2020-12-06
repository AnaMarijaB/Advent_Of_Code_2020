#use "topfind"
#require "str"
open Str

let explode s = List.init (String.length s) (String.get s) 

let stevilo_vprasanj_posameznega str = 
    let input = str |> Str.global_replace (Str.regexp "\n") "" |> explode in
    let rec aux acc prazen = function
        |[] -> acc
        |x::xs ->  if List.mem x prazen then aux acc prazen xs else aux (acc+1) (x::prazen) xs
    in
    aux 0 [] input

let st_vseh_vpr input = 
    let rec aux acc = function
        |[] -> acc
        |x::xs -> aux (acc + stevilo_vprasanj_posameznega x) xs
    in
    aux 0 input 

let naloga1 vsebina = 
    let r = Str.regexp "\n\n" in
    let input_list = vsebina |> Str.split r |> st_vseh_vpr in
    string_of_int (input_list)

let remove p list = 
    let rec aux acc = function
        |[] -> acc
        |x::xs -> if x = p then aux acc xs else aux (x::acc) xs
    in
    aux [] list

let charlist_to_stringlist list = List.map (String.make 1) list

let stevilo_vprasanj_grupe sez = 
    let input = sez |> List.map explode |> List.map charlist_to_stringlist in
    let rec aux acc prazen = function
        |[] -> acc
        |x::xs ->  match x with
            |[] -> aux acc prazen xs
            |y::ys ->  if List.mem y prazen then aux acc prazen ys else aux (acc-1) (remove y prazen) ys
    in 
    aux (List.length (List.hd input)) (List.hd input) input
 

let naloga2 vsebina = 
    let r = Str.regexp "\n\n" in
    let input_list = vsebina |> Str.split r |> List.map (String.split_on_char '\n') in
    input_list


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
    let vsebina_datoteke = preberi_datoteko "day_6/day_6.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    in
    izpisi_datoteko "day_6/day_6_1.out" odgovor1;