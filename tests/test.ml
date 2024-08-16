open Meat
open OUnit2
(* open Batteries *)

let tests =
  "test suite for stats"
  >::: [ ("last item in string"
          >:: fun _ -> assert_equal (Some "d") (last [ "a"; "b"; "c"; "d" ]))
       ; ("last two items in string"
          >:: fun _ -> assert_equal (Some ("c", "d")) (last_two [ "a"; "b"; "c"; "d" ]))
       ; ("nth item in string"
          >:: fun _ -> assert_equal "c" (nth [ "a"; "b"; "c"; "d"; "e" ] 2))
       ; ("nth item in string"
          >:: fun _ -> assert_raises (Failure "nth") (fun () -> nth [ "a" ] 2))
         (* "!! 4" >:: (fun _ -> assert_equal 24 (!! 4) ~cmp:Int.equal ~printer:string_of_int); *)
         (* "!! -4" >:: (fun _ -> assert_raises (Failure "not a positive int") (fun () -> (!! (-4)))); *)
         (* "4 or more events of 80% out of 5" >:: (fun _ -> assert_equal 0.73728 (culm_probs 4 5 0.8) ~cmp:( Float.approx_equal ) ~printer:string_of_float); *)
       ]
;;

let _ = run_test_tt_main tests
