let rec somaLinhas acumulador arquivo =
    try
        let linha = input_line arquivo in
        match linha with
        | "" -> acumulador
        | valor -> somaLinhas (int_of_string valor + acumulador) arquivo
    with
    | End_of_file -> acumulador


let rec encontraMaior maior arquivo =
    match somaLinhas 0 arquivo with
    | 0 -> maior
    | x -> encontraMaior (if maior > x then maior else x) arquivo

let sort x (a, b, c) =
    if x > a then
        (x, a, b)
    else if x > b then
        (a, x, b)
    else if x > c then
        (a, b, x)
    else (a, b, c)

let soma (a, b, c) =
    a + b + c

let rec encontraTresMaiores (a, b, c) arquivo =
    match somaLinhas 0 arquivo with
    | 0 -> (a, b, c)
    | x -> encontraTresMaiores (sort x (a, b, c)) arquivo

let () =
    let arquivo = open_in "input" in 
    Printf.printf "Resultado parte 1: %i\n" (encontraMaior 0 arquivo);
    close_in arquivo;
    let arquivo = open_in "input" in 
    let resposta2 = soma (encontraTresMaiores (0, 0, 0) arquivo) in
    Printf.printf "Resultado parte 2: %i\n" resposta2;
    close_in arquivo
