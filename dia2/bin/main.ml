let valor jogada =
    match jogada with
    | [] -> 0
    | jogada_oponente :: _ :: jogada_minha :: _ ->
            if jogada_oponente = 'A' then
                    if jogada_minha = 'X' then 3 + 1
                    else if jogada_minha = 'Y' then 6 + 2
                    else 0 + 3
            else if jogada_oponente = 'B' then
                    if jogada_minha = 'X' then 0 + 1
                    else if jogada_minha = 'Y' then 3 + 2
                    else 6 + 3
            else
                    if jogada_minha = 'X' then 6 + 1
                    else if jogada_minha = 'Y' then 0 + 2
                    else 3 + 3
    | _ -> 0

let valor2 jogada =
    match jogada with
    | [] -> 0
    | jogada_oponente :: _ :: jogada_minha :: _ ->
            if jogada_oponente = 'A' then
                    if jogada_minha = 'X' then 0 + 3
                    else if jogada_minha = 'Y' then 3 + 1
                    else 6 + 2
            else if jogada_oponente = 'B' then
                    if jogada_minha = 'X' then 0 + 1
                    else if jogada_minha = 'Y' then 3 + 2
                    else 6 + 3
            else
                    if jogada_minha = 'X' then 0 + 2
                    else if jogada_minha = 'Y' then 3 + 3
                    else 6 + 1
    | _ -> 0

let stringParaListaChar string =
    string |> String.to_seq |> List.of_seq

let rec joga canal pontos =
    try
        let linha = input_line canal in 
        let jogada = stringParaListaChar linha in
        match linha with
        | "" -> pontos
        | _ -> joga canal (pontos + valor jogada)
    with
    | End_of_file -> pontos

let rec joga2 canal pontos =
    try
        let linha = input_line canal in 
        let jogada = stringParaListaChar linha in
        match linha with
        | "" -> pontos
        | _ -> joga2 canal (pontos + valor2 jogada)
    with
    | End_of_file -> pontos

let () = 
    let canal = open_in "input" in 
    let resultado = joga canal 0 in
    Printf.printf "Resultado 1: %i\n" resultado;

    seek_in canal 0;
    let resultado2 = joga2 canal 0 in
    Printf.printf "Resultado 2: %i\n" resultado2
