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

(* let remove p list = 
    let rec aux acc = function
        |[] -> acc
        |x::xs -> if x = p then aux acc xs else aux (x::acc) xs
    in
    aux [] list

let charlist_to_stringlist list = List.map (String.make 1) list

let my_max = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs

let podsez_max_dolzina seznam = 
    let rec aux acc = function
        |[] -> acc
        |x::xs -> if List.length x > List.length acc then aux (x) xs else aux acc xs
    in
    aux (List.hd seznam) seznam

let stevilo_vprasanj_grupe sez = 
    let input = sez |> List.map explode |> List.map charlist_to_stringlist in
    let rec aux acc prazen = function
        |[] -> acc
        |x::xs ->  match x with
            |[] -> aux acc prazen xs
            |y::ys ->  if List.mem y prazen then aux acc prazen (ys::xs) else aux (acc-1) (remove y prazen) (ys::xs)
    in 
    aux (my_max (List.map String.length sez)) (podsez_max_dolzina input) input

let rec vsi_true = function
    |[] -> true 
    |x::xs -> if x then vsi_true xs else false

let stevilo_vprasanj_grupe2 sez = 
    let input = sez |> List.map explode |> List.map charlist_to_stringlist in
    let rec aux acc prazen = function
        |[] -> acc
        |x::xs ->  match x with
            |[] -> aux acc prazen xs
            |y::ys ->  if List.mem y prazen then aux acc prazen (ys::xs) else
                if vsi_true (List.map (List.mem y) xs) then aux acc prazen (ys::xs) else aux (acc-1) (remove y prazen) (ys::xs)
    in 
    aux (my_max (List.map String.length sez)) (podsez_max_dolzina input) input
 
let naloga2 vsebina = 
    let r = Str.regexp "\n\n" in
    let input_list = vsebina |> Str.split r |> List.map (String.split_on_char '\n') in
    let rec aux acc = function
        |[] -> acc
        |x::xs -> aux (acc+ stevilo_vprasanj_grupe2 x) xs 
    in
    string_of_int (aux 0 input_list) *)


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