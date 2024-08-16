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

let () = ()
