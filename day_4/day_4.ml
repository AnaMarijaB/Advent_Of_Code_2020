#use "topfind"
#require "str"
open Str

(*https://stackoverflow.com/questions/8373460/substring-check-in-ocaml*)
let contains string vzorec =
    let re = Str.regexp_string vzorec in
        try ignore (Str.search_forward re string 0); true
        with Not_found -> false

let veljaven_id string = 
    let pogoji = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] in 
    let rec aux str = function
        |[] -> true
        |x::xs -> if contains str x then aux str xs else false
    in
    aux string pogoji

let stevec_veljavnih_id seznam = 
    let rec aux acc = function
        |[]-> acc
        |x::xs -> if veljaven_id x then aux (acc+1) xs else aux acc xs
    in
    aux 0 seznam 

let naloga1 vsebina = 
    let r = Str.regexp "\n\n" in
    let input_list = vsebina |> Str.split r in
    string_of_int (stevec_veljavnih_id input_list)
 
let v_pare geslo =  (*geslo je zapisano kot "byr:1937\neyr:2030 pid:154364481\nhgt:158cm iyr:2015 ecl:brn hcl:#c0946f cid:155"*)
    let p = Str.regexp "[\n \t :]+" in
    let vnos = Str.split p geslo in
    let rec v_dvojice = function
        |[] -> []
        | x::y::xs -> (x, y) :: v_dvojice xs
        |_ -> failwith "vrstica ni veljavna" 
    in
    v_dvojice vnos 

let veljaven_hgt podatek = 
    let konec = Str.last_chars podatek 2 in
    match konec with 
    |"cm" -> let st = int_of_string (List.hd (Str.split (Str.regexp "cm") podatek)) in 
        if st >= 150 && st <= 193 then true else false
    |"in" -> let st = int_of_string (List.hd (Str.split (Str.regexp "in") podatek)) in 
        if st >= 59 && st <= 76 then true else false
    |_-> false

let rec veljaven_id_2 = function
    |[] -> true
    | x :: xs -> let (pogoj, podatek) = x in
        match pogoj with
        |"byr" -> let num = (int_of_string podatek) in 
            if (num >= 1920 && num <= 2002) then veljaven_id_2 xs else false

        |"iyr" -> let num = (int_of_string podatek) in 
            if (num >= 2010 && num <= 2020) then veljaven_id_2 xs else false

        |"eyr" -> let num = (int_of_string podatek) in 
            if (num >= 2020 && num <= 2030) then veljaven_id_2 xs else false

        |"hgt" -> if veljaven_hgt podatek then veljaven_id_2 xs else false

        |"hcl" -> let reghcl = Str.regexp "^#[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]$" in 
            if (Str.string_match reghcl podatek 0) then veljaven_id_2 xs else false

        |"ecl" -> let colors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] in
            if (List.mem podatek colors) then veljaven_id_2 xs else false

        |"pid" -> let regpid = Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" in
            if (Str.string_match regpid podatek 0) then veljaven_id_2 xs else false
        
        |"cid" -> veljaven_id_2 xs

        |_ -> false

let stevec_veljavnih_id2 seznam = 
    let rec aux acc = function
        |[]-> acc
        |x::xs -> let vnos = v_pare x in
            if veljaven_id x then 
                if veljaven_id_2 vnos then aux (acc+1) xs else aux acc xs
            else aux acc xs
    in
    aux 0 seznam

let naloga2 vsebina = 
    let r = Str.regexp "\n\n" in
    let input_list = vsebina |> Str.split r in
    string_of_int (stevec_veljavnih_id2 input_list)

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
    let vsebina_datoteke = preberi_datoteko "day_4/day_4.in" in
    let odgovor1 = naloga1 vsebina_datoteke in
    let odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_4/day_4_1.out" odgovor1;
    izpisi_datoteko "day_4/day_4_2.out" odgovor2