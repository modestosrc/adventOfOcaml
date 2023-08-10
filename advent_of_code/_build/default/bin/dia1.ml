let arquivo = "input/dia1"

let somaLinhas canal =
    let rec aux acc =
        try
            match input_line canal with
            | "" -> acc
            | st -> aux ((int_of_string st) + acc)
        with
        |End_of_file -> acc
    in aux 0
;;

let resposta1 = 
    let canal = open_in arquivo in
    let rec aux r =
        match somaLinhas canal with
        | 0 -> r
        | x -> aux (max x r)
    in     
    let result = aux 0 in
    close_in canal;
    result
;;

let resposta2 =
    let canal = open_in arquivo in
    let rec aux (a, b, c) =
        match somaLinhas canal with
        | 0 -> (a, b, c)
        | x -> aux (if x > a then (x, a, b) else
                    if x > b then (a, x, b) else
                    if x > c then (a, b, x) else
                    (a, b, c)) 
    in 
    let result = (fun (a, b, c) -> a + b + c) (aux (0, 0, 0)) in
    close_in canal;
    result
;;

let main =
    print_endline ("Resposta 1: " ^ (resposta1 |> string_of_int ));

    print_endline ("Resposta 2: " ^ (resposta2 |> string_of_int ));
