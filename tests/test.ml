open Meat
open OUnit2
open Batteries

let tests = "test suite for stats" >::: [
  "!! 4" >:: (fun _ -> assert_equal 24 (!! 4) ~cmp:Int.equal ~printer:string_of_int);
  "!! 1" >:: (fun _ -> assert_equal 1 (!! 1) ~cmp:Int.equal ~printer:string_of_int);
  "!! 0" >:: (fun _ -> assert_equal 1 (!! 0) ~cmp:Int.equal ~printer:string_of_int);
  "!! -4" >:: (fun _ -> assert_raises (Failure "not a positive int") (fun () -> (!! (-4))));
  "ksn 4 5" >:: (fun _ -> assert_equal 5 (ksn 4 5) ~cmp:Int.equal ~printer:string_of_int);
  "ksn 3 5" >:: (fun _ -> assert_equal 10 (ksn 3 5) ~cmp:Int.equal ~printer:string_of_int);
  "5 equal events of 10%" >:: (fun _ -> assert_equal 0.01 (probs 2 2 0.1) ~cmp:( Float.approx_equal ) ~printer:string_of_float);
  "3 equal events of 99%" >:: (fun _ -> assert_equal 0.9703 (probs 3 3 0.99) ~cmp:( Float.approx_equal ) ~printer:string_of_float);
  "4 events of 80% out of 5" >:: (fun _ -> assert_equal 0.4096 (probs 4 5 0.8) ~cmp:( Float.approx_equal ) ~printer:string_of_float);
  "3 events of 80% out of 5" >:: (fun _ -> assert_equal 0.2048 (probs 3 5 0.8) ~cmp:( Float.approx_equal ) ~printer:string_of_float);
  "3 or more events of 80% out of 5" >:: (fun _ -> assert_equal 0.94208 (culm_probs 3 5 0.8) ~cmp:( Float.approx_equal ) ~printer:string_of_float);
  "4 or more events of 80% out of 5" >:: (fun _ -> assert_equal 0.73728 (culm_probs 4 5 0.8) ~cmp:( Float.approx_equal ) ~printer:string_of_float);
]

let _ = run_test_tt_main tests;;
