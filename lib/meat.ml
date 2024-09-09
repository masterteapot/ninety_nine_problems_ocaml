type 'a node =
  | One of 'a
  | Many of 'a node list

type 'a rle =
  | Modone of 'a
  | Modmany of int * 'a

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let example_tree =
  Node
    ( 'a'
    , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
    , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) )
;;

type demo_tree =
  | Dleaf
  | Dbranch of int * demo_tree * demo_tree

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec num_branches = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + num_branches l + num_branches r
;;

type direction =
  | Left
  | Right

(* (* (And (Var "a", Or (Var "a", Var "b"))) *) *)
let table2 vara varb expr =
  let rec aux ab bb = function
    | Var x when x = vara -> ab
    | Var x when x = varb -> bb
    | Var _ -> failwith "what is this var?"
    | Not x -> Bool.not @@ aux ab bb x
    | And (x, y) -> aux ab bb x && aux ab bb y
    | Or (x, y) -> aux ab bb x || aux ab bb y
  in
  let poss = [ true, true; true, false; false, true; false, false ] in
  List.map (fun x -> fst x, snd x, aux (fst x) (snd x) expr) poss
;;

let longest_branch t =
  let rec aux counter = function
    | Empty -> counter
    | Node (_, l, r) -> Int.max (aux (counter + 1) l) (aux (counter + 1) r)
  in
  aux 0 t
;;

let gray n =
  let rec mirror lacc racc = function
    | [] -> List.rev lacc @ racc
    | hd :: tl -> mirror (("0" ^ hd) :: lacc) (("1" ^ hd) :: racc) tl
  in
  let rec aux counter ls =
    if counter = n then ls else aux (counter + 1) (mirror [] [] ls)
  in
  aux 1 [ "0"; "1" ]
;;

let table vlist expr =
  let all_truthies ls =
    let rec aux iacc = function
      | [] -> [ List.rev iacc ]
      | hd :: tl -> aux ((hd, true) :: iacc) tl @ aux ((hd, false) :: iacc) tl
    in
    aux [] ls
  in
  let rec aux ls = function
    | Var x -> List.assoc x ls
    | Not x -> Bool.not @@ aux ls x
    | And (x, y) -> aux ls x && aux ls y
    | Or (x, y) -> aux ls x || aux ls y
  in
  let poss = all_truthies vlist in
  List.map (fun x -> x, aux x expr) poss
;;

let shortest_branch t =
  let rec aux counter = function
    | Empty -> counter
    | Node (_, l, r) -> Int.min (aux (counter + 1) l) (aux (counter + 1) r)
  in
  aux 0 t
;;

let construct_balanced_tree num x =
  let rec add_node = function
    | Empty -> failwith "Shouldn't be constructing empty nodes"
    | Node (v, Empty, r) -> Node (v, Node (x, Empty, Empty), r)
    | Node (v, l, Empty) -> Node (v, l, Node (x, Empty, Empty))
    | Node (v, l, r) ->
      if shortest_branch l <= shortest_branch r
      then Node (v, add_node l, r)
      else Node (v, l, add_node r)
  in
  let rec aux counter acc =
    if counter = num then acc else aux (1 + counter) (add_node acc)
  in
  aux 1 (Node (x, Empty, Empty))
;;

(* Do I need to have two internal functions, one for constructing nodes and one for constructing a list of nodes? *)
(* How can I have something that goes in both directions but also maintains balance? *)
let cbal num x =
  let rec aux counter = function
    | v when counter = num -> v
    | Empty -> aux (counter + 1) (Node (x, Empty, Empty))
    | Node (v, l, r) -> Node (v, aux (counter + 1) l, r)
  in
  aux 1 (Node (x, Empty, Empty))
;;

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

let is_prime num =
  let num = abs num in
  let rec aux counter is_true =
    if (not is_true) || num < 2
    then false
    else if counter = num || counter * counter > num
    then true
    else aux (counter + 1) (num mod counter <> 0)
  in
  if num < 2 then false else aux 2 true
;;

let gcd n1 n2 =
  let big = if n1 >= n2 then n1 else n2 in
  let small = if n1 < n2 then n1 else n2 in
  let rec aux small big =
    if big mod small = 0 then small else aux (big mod small) small
  in
  aux small big
;;

let all_primes low high =
  assert (low <= high);
  assert (low > 1);
  let rec aux num acc =
    let v = is_prime num in
    if num > high
    then List.rev acc
    else if v = true && num > 2
    then aux (num + 2) (num :: acc)
    else if v = true
    then aux (num + 1) (num :: acc)
    else aux (num + 2) acc
  in
  aux low []
;;

let coprime n1 n2 = gcd n1 n2 = 1

let goldbach num =
  let primes = all_primes 2 num in
  let rec is_match x = function
    | [] -> None
    | hd :: tl -> if x + hd = num then Some (x, hd) else is_match x tl
  in
  let rec aux = function
    | [] -> failwith "no 2 primes found to equal value"
    | hd :: tl ->
      (match is_match hd primes with
       | Some (x, y) -> x, y
       | None -> aux tl)
  in
  aux primes
;;

let goldbach_v2 num =
  let rec aux small =
    if is_prime small && is_prime (num - small)
    then small, num - small
    else aux (small + 1)
  in
  aux 2
;;

let goldbach_list low high =
  let rec get_even_ints lval rval =
    if lval > rval
    then []
    else if lval mod 2 = 1
    then get_even_ints (lval + 1) rval
    else lval :: get_even_ints (lval + 2) rval
  in
  let evens = get_even_ints low high in
  List.map (fun x -> x, goldbach_v2 x) evens
;;

let goldbach_limit low high limit =
  List.filter (fun (_, (x, y)) -> x > limit && y > limit) (goldbach_list low high)
;;

let phi num =
  let rec aux counter =
    if counter = num
    then 0
    else if coprime counter num
    then 1 + aux (counter + 1)
    else aux (counter + 1)
  in
  aux 1
;;

let factors num =
  let rec aux counter num acc =
    if counter = num
    then rev (num :: acc)
    else if num mod counter = 0
    then aux counter (num / counter) (counter :: acc)
    else aux (counter + 1) num acc
  in
  aux 2 num []
;;

let factors_v2 num =
  let rec aux counter num = function
    | (x, c) :: tl as ls when counter = num ->
      if x = num then rev ((x, c + 1) :: tl) else rev ((num, 1) :: ls)
    | _ when counter = num -> [ num, 1 ]
    | (x, c) :: tl as ls when num mod counter = 0 ->
      if counter = x
      then aux counter (num / counter) ((x, c + 1) :: tl)
      else aux counter (num / counter) ((counter, 1) :: ls)
    | ls when num mod counter = 0 -> aux counter (num / counter) ((counter, 1) :: ls)
    | x -> aux (counter + 1) num x
  in
  aux 2 num []
;;

let rec pow x = function
  | 0 -> 1
  | 1 -> x
  | n ->
    let b = pow x (n / 2) in
    b * b * if n mod 2 = 0 then 1 else x
;;

let phi_improved num =
  let f_nums = factors_v2 num in
  List.fold_left (fun acc (p, m) -> acc * (p - 1) * pow p (m - 1)) 1 f_nums
;;

let () = ()
