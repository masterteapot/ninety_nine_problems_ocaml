open Meat
open OUnit2
(* open Batteries *)

let tests =
  "test suite for stats"
  >::: [ ("last item in string"
          >:: fun _ -> assert_equal (Some "d") (last [ "a"; "b"; "c"; "d" ]))
       ; ("last two items in string"
          >:: fun _ -> assert_equal (Some ("c", "d")) (last_two [ "a"; "b"; "c"; "d" ]))
       ; ("nth item in list"
          >:: fun _ -> assert_equal "c" (nth [ "a"; "b"; "c"; "d"; "e" ] 2))
       ; ("nth item in list"
          >:: fun _ -> assert_raises (Failure "nth") (fun () -> nth [ "a" ] 2))
       ; ("length of a list" >:: fun _ -> assert_equal 3 (length [ "a"; "b"; "c" ]))
       ; ("reverse a list" >:: fun _ -> assert_equal ["c"; "b"; "a"] (rev [ "a"; "b"; "c" ]))
       ; ("palindrone" >:: fun _ -> assert_equal true (palindrone ["x"; "a"; "m"; "a"; "x"]))
       ; ("not palindrone" >:: fun _ -> assert_equal false (palindrone ["x"; "a"]))
       ; ("flat list" >:: fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]))
       ; ("dedups consecutive" >:: fun _ -> assert_equal ["a"; "b"; "c"; "a"; "d"; "e"] (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
       ; ("packing dups" >:: fun _ -> assert_equal [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]))
       ; ("run-length encoding" >:: fun _ -> assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
       ; ("modified run-length encoding" >:: fun _ -> assert_equal [Modmany (4, "a"); Modone "b"; Modmany (2, "c"); Modmany (2, "a"); Modone "d";
 Modmany (4, "e")] (mod_encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
       ; ("modified run-length decoding" >:: fun _ -> assert_equal ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] (decode [Modmany (4, "a"); Modone "b"; Modmany (2, "c"); Modmany (2, "a"); Modone "d"; Modmany (4, "e")]))
       ; ("duplicate list" >:: fun _ -> assert_equal ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] (duplicate ["a"; "b"; "c"; "c"; "d"]))
       ; ("duplicate list n times" >:: fun _ -> assert_equal ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] (replicate ["a"; "b"; "c"] 3))
       ; ("drop every nth item" >:: fun _ -> assert_equal ["a"; "b"; "d"; "e"; "g"; "h"; "j"] (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3))
       ; ("split at n" >:: fun _ -> assert_equal (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3))
       ; ("splice at i and k both inclusive" >:: fun _ -> assert_equal (["c"; "d"; "e"; "f"; "g"]) (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6))
         (* "!! 4" >:: (fun _ -> assert_equal 24 (!! 4) ~cmp:Int.equal ~printer:string_of_int); *)
         (* "!! -4" >:: (fun _ -> assert_raises (Failure "not a positive int") (fun () -> (!! (-4)))); *)
         (* "4 or more events of 80% out of 5" >:: (fun _ -> assert_equal 0.73728 (culm_probs 4 5 0.8) ~cmp:( Float.approx_equal ) ~printer:string_of_float); *)
       ]
;;

let _ = run_test_tt_main tests
