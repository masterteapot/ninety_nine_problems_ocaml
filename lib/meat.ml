type 'a node =
  | One of 'a
  | Many of 'a node list

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
  let rec aux counter item acc = function
    | [] when counter = 0 -> []
    | [] -> rev ((counter, item) :: acc)
    | hd :: tl when counter = 0 -> aux 1 hd acc tl
    | hd :: tl when hd = item -> aux (counter + 1) item acc tl
    | hd :: tl -> aux 1 hd ((counter, item) :: acc) tl
  in
  aux 0 "" [] ls
;;

let () = ()
