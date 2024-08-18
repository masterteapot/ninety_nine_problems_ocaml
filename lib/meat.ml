type 'a node =
  | One of 'a
  | Many of 'a node list

type 'a rle =
  | Modone of 'a
  | Modmany of int * 'a

let rec last = function
  | [] -> None
  | x :: [] -> Some x
  | _ :: tl -> last tl
;;

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl
;;

let rec nth ls idx =
  match ls with
  | [] -> raise (Failure "nth")
  | hd :: tl -> if idx == 0 then hd else nth tl (idx - 1)
;;

let length ls =
  let rec aux counter = function
    | [] -> counter
    | _ :: tl -> aux (counter + 1) tl
  in
  aux 0 ls
;;

let rev ls =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] ls
;;

let flatten ls =
  let rec aux acc = function
    | [] -> acc
    | One hd :: tl -> aux (hd :: acc) tl
    | Many hd :: tl ->
      let new_acc = aux acc hd in
      aux new_acc tl
  in
  aux [] ls |> rev
;;

let palindrone ls =
  let sl = rev ls in
  let rec aux ls sl =
    match ls, sl with
    | [], [] -> true
    | hd :: tl, dh :: lt when hd = dh -> aux tl lt
    | _, _ -> false
  in
  aux ls sl
;;

let compress ls =
  let rec aux acc = function
    | [] -> rev acc
    | a :: b :: tl when a = b -> aux acc (a :: tl)
    | a :: tl -> aux (a :: acc) tl
  in
  aux [] ls
;;

let pack ls =
  let rec aux inner outer = function
    | [] -> rev outer
    | a :: b :: tl when a = b -> aux (a :: inner) outer (b :: tl)
    | a :: tl -> aux [] ((a :: inner) :: outer) tl
  in
  aux [] [] ls
;;

let encode ls =
  let rec aux counter acc = function
    | [] -> []
    | [ x ] -> rev ((counter + 1, x) :: acc)
    | hd :: md :: tl when hd = md -> aux (counter + 1) acc (md :: tl)
    | hd :: tl -> aux 0 ((counter + 1, hd) :: acc) tl
  in
  aux 0 [] ls
;;

let mod_encode ls =
  let rec aux counter acc = function
    | [] -> []
    | [ x ] when counter = 0 -> rev (Modone x :: acc)
    | [ x ] -> rev (Modmany (counter + 1, x) :: acc)
    | hd :: md :: tl when hd = md -> aux (counter + 1) acc (md :: tl)
    | hd :: tl when counter = 0 -> aux 0 (Modone hd :: acc) tl
    | hd :: tl -> aux 0 (Modmany (counter + 1, hd) :: acc) tl
  in
  aux 0 [] ls
;;

let decode ls =
  let rec add_many num_needed item idx acc =
    if num_needed = idx then acc else add_many num_needed item (idx + 1) (item :: acc)
  in
  let rec aux acc = function
    | [] -> rev acc
    | Modone x :: tl -> aux (x :: acc) tl
    | Modmany (n, x) :: tl -> aux (add_many n x 0 acc) tl
  in
  aux [] ls
;;

let duplicate ls =
  let rec aux acc = function
    | [] -> rev acc
    | hd :: tl -> aux (hd :: hd :: acc) tl
  in
  aux [] ls
;;

let replicate ls num =
  let rec add_many n x acc = if n = 0 then acc else add_many (n - 1) x (x :: acc) in
  let rec aux acc = function
    | [] -> rev acc
    | hd :: tl -> aux (add_many num hd [] @ acc) tl
  in
  aux [] ls
;;

let drop ls n =
  let rec aux idx acc = function
    | [] -> rev acc
    | _ :: tl when idx = 1 -> aux n acc tl
    | hd :: tl -> aux (idx - 1) (hd :: acc) tl
  in
  aux n [] ls
;;

let split ls n =
  let rec aux idx acc = function
    | [] -> rev acc, []
    | ls when idx = 0 -> rev acc, ls
    | hd :: tl -> aux (idx - 1) (hd :: acc) tl
  in
  aux n [] ls
;;

let slice ls i k =
  let rec aux counter acc = function
    | [] -> rev acc
    | _ when counter > k -> rev acc
    | hd :: tl ->
      if counter >= i then aux (counter + 1) (hd :: acc) tl else aux (counter + 1) acc tl
  in
  aux 0 [] ls
;;

let rotate ls i =
  let rec aux counter acc = function
    | [] -> rev acc
    | hd :: tl when counter = i -> tl @ rev (hd :: acc)
    | hd :: tl -> aux (counter + 1) (hd :: acc) tl
  in
  aux 1 [] ls
;;

let remove_at i ls =
  let rec aux counter acc = function
    | [] -> rev acc
    | _ :: tl when counter = i -> rev acc @ tl
    | hd :: tl -> aux (counter + 1) (hd :: acc) tl
  in
  aux 0 [] ls
;;

let insert_at el i ls =
  let rec aux counter acc = function
    | [] -> rev (el :: acc)
    | hd :: tl when counter = i -> rev (el :: acc) @ (hd :: tl)
    | hd :: tl -> aux (counter + 1) (hd :: acc) tl
  in
  aux 0 [] ls
;;

let range i k =
  let operator = if i <= k then ( + ) else ( - ) in
  let rec aux counter acc =
    if counter = k then rev (counter :: acc) else aux (operator counter 1) (counter :: acc)
  in
  aux i []
;;

let rand_select ls num =
  let rec get_rand_indicies counter acc =
    if counter = num
    then acc
    else get_rand_indicies (counter + 1) (Random.int (length ls) :: acc)
  in
  let rand_ls = get_rand_indicies 0 [] in
  List.map (fun x -> nth ls x) rand_ls
;;

let lotto_select i k =
  let rec get_rand_indicies counter acc =
    if counter = 0
    then acc
    else get_rand_indicies (counter - 1) ((Random.int k + 1) :: acc)
  in
  get_rand_indicies i []
;;

let extract i ls =
  let rec chopper item acc = function
    | [] -> acc
    | ls when length ls < i - 1 -> acc
    | hd :: tl -> chopper item ((item :: slice (hd :: tl) 0 (i - 2)) :: acc) tl
  in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (chopper hd [] tl :: acc) tl
  in
  if i = 1
  then List.map (fun x -> [ x ]) ls
  else if i <= 0
  then [ [] ]
  else aux [] ls |> List.flatten |> rev
;;

let length_sort ls =
  let rec aux acc ls item =
    match ls with
    | [] -> rev (item :: acc)
    | hd :: tl when length item < length hd -> List.rev acc @ (item :: hd :: tl)
    | hd :: tl -> aux (hd :: acc) tl item
  in
  List.fold_left (aux []) [] ls
;;

let frequency_sort ls =
  let rec aux acc ls item =
    match ls with
    | [] -> rev (item :: acc)
    | hd :: tl when fst item < fst hd -> List.rev acc @ (item :: hd :: tl)
    | hd :: tl -> aux (hd :: acc) tl item
  in
  let mapped = List.map (fun x -> length x, x) ls in
  List.fold_left (aux []) [] mapped
;;

let full_words num =
  let nums =
    [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
  in
  let rec reducinator acc = function
    | 0 -> acc
    | x -> reducinator (Int.rem x 10 :: acc) (x / 10)
  in
  let rec aux acc = function
    | [] -> acc
    | [ hd ] -> acc ^ nums.(hd)
    | hd :: tl -> aux (acc ^ nums.(hd) ^ "-") tl
  in
  num |> reducinator [] |> aux ""
;;

let () = ()
