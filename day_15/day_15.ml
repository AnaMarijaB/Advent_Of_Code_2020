let seznam_dvojic stevilo = 
    let rec aux acc seznam  = 
        if acc > stevilo then seznam else aux (acc+1) ((acc, 0)::seznam)
    in 
    List.rev (aux 1 [])

(*TAKO NI ŠLO....pomagala sem si z rešitvami iz ostalih spletnih strani
https://topaz.github.io/paste/#XQAAAQCEAgAAAAAAAAA2GUrqJWbwoQ9CFebuH/MnTUmk6LKG1iy1QPrmX8BOHQarhtbB6/4dOcxi07PeHfHbyZFoKijNljg404yiVVW+eAVwTudCIPxy96/BZWYEOK8EEvtvajho6fuNYxOnYJQVZkdHp+q9Hy2Xe9jzjC0+QSL8FuFOUu4RA54Sev0BhtLR23osbbfE+NFSEL6qVFZWVFa54rS9ydTeMhxrqPKlSkgZSYAOB5DA/Gc4BogO9C5Nea7GXiQuZcGM6igC/BvMtpJWozO7aYJFWBl+u6478ySYykmeD97swtPQcGs+evyOBABxJRB+JXgEorQ7lPqWauzfFfKBxQOdRQDVP/E1NGVk9mMVHJyG30IIBLxrQuA3He+yK3tcu0991Q0DBHlF3pGvsRV8WiFBmKUrDR5Fms5ughtdZtBZ4J8TcnRJvqo5cF+yC0cY9RO9zJVs/8sUDeA=
https://www.reddit.com/r/adventofcode/comments/kdf85p/2020_day_15_solutions/gfwu7ti/?utm_source=reddit&utm_medium=web2x&context=3*)

let n_ti_element n sez =
  let sez_brez_zadnjega = sez |> List.rev |> List.tl |> List.rev in 
  let zadnji = sez |> List.rev |> List.hd in 
  let prazen = Array.make n 0 in 
  List.iteri (fun i n -> prazen.(n) <- i + 1) sez_brez_zadnjega;   (*to morem ugotovit zakaj je tle *)
  let rec krog poteza prejsni = if poteza = n + 1 then prejsni else 
    let naslednji = match prazen.(prejsni) with
      |0 -> 0 
      |i -> poteza - i -1 
    in
    prazen.(prejsni) <- poteza -1;
    krog (poteza + 1) naslednji
  in
  krog ((List.length sez) +1) zadnji
 
let podatki = "6,4,12,1,20,0,16"

let naloga1 vsebina = 
    let input = vsebina |> String.split_on_char ',' |> List.map (int_of_string) in
    string_of_int (n_ti_element 2020 input)  (*mi javi napako int_of_string ma če dam posebi v konzolo pa dela*)


let naloga2 vsebina = 
    let input = vsebina |> String.split_on_char ',' |> List.map (int_of_string) in
    string_of_int (n_ti_element 30000000 input)  (*mi javi napako int_of_string ma če dam posebi v konzolo pa dela*)
    
let _ =
    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let odgovor1 = naloga1 podatki in
    let odgovor2 = naloga2 podatki
    in
    izpisi_datoteko "day_15/day_15_1.out" odgovor1;
    izpisi_datoteko "day_15/day_15_2.out" odgovor2