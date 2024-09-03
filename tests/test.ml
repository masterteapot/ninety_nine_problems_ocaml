open Meat
open OUnit2
(* open Batteries *)

let example_balanced_tree =
  Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
;;

let example_unbalanced_tree =
  Node
    ( 'x'
    , Node ('x', Node ('x', Empty, Empty), Empty)
    , Node ('x', Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Empty), Empty) )
;;

let example_unbalanced_tree_v2 =
  Node
    ( 'x'
    , Node ('x', Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Empty), Empty)
    , Node ('x', Node ('x', Empty, Empty), Empty) )
;;

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
       ; ("reverse a list"
          >:: fun _ -> assert_equal [ "c"; "b"; "a" ] (rev [ "a"; "b"; "c" ]))
       ; ("palindrone"
          >:: fun _ -> assert_equal true (palindrone [ "x"; "a"; "m"; "a"; "x" ]))
       ; ("not palindrone" >:: fun _ -> assert_equal false (palindrone [ "x"; "a" ]))
       ; ("flat list"
          >:: fun _ ->
          assert_equal
            [ "a"; "b"; "c"; "d"; "e" ]
            (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]))
       ; ("dedups consecutive"
          >:: fun _ ->
          assert_equal
            [ "a"; "b"; "c"; "a"; "d"; "e" ]
            (compress
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ; ("packing dups"
          >:: fun _ ->
          assert_equal
            [ [ "a"; "a"; "a"; "a" ]
            ; [ "b" ]
            ; [ "c"; "c" ]
            ; [ "a"; "a" ]
            ; [ "d"; "d" ]
            ; [ "e"; "e"; "e"; "e" ]
            ]
            (pack
               [ "a"
               ; "a"
               ; "a"
               ; "a"
               ; "b"
               ; "c"
               ; "c"
               ; "a"
               ; "a"
               ; "d"
               ; "d"
               ; "e"
               ; "e"
               ; "e"
               ; "e"
               ]))
       ; ("run-length encoding"
          >:: fun _ ->
          assert_equal
            [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
            (encode
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ; ("modified run-length encoding"
          >:: fun _ ->
          assert_equal
            [ Modmany (4, "a")
            ; Modone "b"
            ; Modmany (2, "c")
            ; Modmany (2, "a")
            ; Modone "d"
            ; Modmany (4, "e")
            ]
            (mod_encode
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ; ("modified run-length decoding"
          >:: fun _ ->
          assert_equal
            [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
            (decode
               [ Modmany (4, "a")
               ; Modone "b"
               ; Modmany (2, "c")
               ; Modmany (2, "a")
               ; Modone "d"
               ; Modmany (4, "e")
               ]))
       ; ("duplicate list"
          >:: fun _ ->
          assert_equal
            [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
            (duplicate [ "a"; "b"; "c"; "c"; "d" ]))
       ; ("duplicate list n times"
          >:: fun _ ->
          assert_equal
            [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
            (replicate [ "a"; "b"; "c" ] 3))
       ; ("drop every nth item"
          >:: fun _ ->
          assert_equal
            [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
            (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3))
       ; ("split at n"
          >:: fun _ ->
          assert_equal
            ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
            (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3))
       ; ("splice at i and k both inclusive"
          >:: fun _ ->
          assert_equal
            [ "c"; "d"; "e"; "f"; "g" ]
            (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6))
       ; ("rotate a list at nth element"
          >:: fun _ ->
          assert_equal
            [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
            (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3))
       ; ("remove element from list at i"
          >:: fun _ -> assert_equal [ "a"; "c"; "d" ] (remove_at 1 [ "a"; "b"; "c"; "d" ])
         )
       ; ("insert element into list at i"
          >:: fun _ ->
          assert_equal
            [ "a"; "alfa"; "b"; "c"; "d" ]
            (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ]))
       ; ("create list of integers within range"
          >:: fun _ -> assert_equal [ 4; 5; 6; 7; 8; 9 ] (range 4 9))
       ; ("create list of integers within descending range"
          >:: fun _ -> assert_equal [ 9; 8; 7 ] (range 9 7))
         (* ; ("return a list of random items of length x" >:: fun _ -> assert_equal (["e"; "c"; "g"]) (rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3)) *)
         (*        ; ("return a list of i number of random items from 1 to k" >:: fun _ -> assert_equal ([20; 28; 45; 16; 24; 38] *)
         (* ) (lotto_select 6 49)) *)
       ; ("create a list of all possible permutations"
          >:: fun _ ->
          assert_equal
            [ [ "a"; "b" ]
            ; [ "a"; "c" ]
            ; [ "a"; "d" ]
            ; [ "b"; "c" ]
            ; [ "b"; "d" ]
            ; [ "c"; "d" ]
            ]
            (extract 2 [ "a"; "b"; "c"; "d" ]))
       ; ("sort list by inner list lengths"
          >:: fun _ ->
          assert_equal
            [ [ "o" ]
            ; [ "d"; "e" ]
            ; [ "d"; "e" ]
            ; [ "m"; "n" ]
            ; [ "a"; "b"; "c" ]
            ; [ "f"; "g"; "h" ]
            ; [ "i"; "j"; "k"; "l" ]
            ]
            (length_sort
               [ [ "a"; "b"; "c" ]
               ; [ "d"; "e" ]
               ; [ "f"; "g"; "h" ]
               ; [ "d"; "e" ]
               ; [ "i"; "j"; "k"; "l" ]
               ; [ "m"; "n" ]
               ; [ "o" ]
               ]))
         (* TODO       (* ; ("sort list by their list length frequency" >:: fun _ ->  *) *)
         (*      assert_equal ([["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]]) (frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]])) *)
       ; ("convert numbers into strings"
          >:: fun _ -> assert_equal "one-seven-five" (full_words 175))
       ; ("checks if a number is prime" >:: fun _ -> assert_equal true (is_prime 7))
       ; ("checks if a number is prime" >:: fun _ -> assert_equal false (is_prime 12))
       ; ("checks if a number is prime" >:: fun _ -> assert_equal false (is_prime 117))
       ; ("checks if a number is prime" >:: fun _ -> assert_equal true (is_prime 97))
       ; ("checks if two numbers are coprime"
          >:: fun _ -> assert_equal true (coprime 13 27))
       ; ("checks if two numbers are coprime"
          >:: fun _ -> assert_equal false (coprime 20536 7826))
       ; ("finds the greatest common divisor between 2 numbers"
          >:: fun _ -> assert_equal 1 (gcd 13 27))
       ; ("finds the greatest common divisor between 2 numbers"
          >:: fun _ -> assert_equal 2 (gcd 20536 7826))
       ; ("Euler's totient function to find number of coprime numbers"
          >:: fun _ -> assert_equal 4 (phi 10))
       ; ("Find lowest factors of a number"
          >:: fun _ -> assert_equal [ 3; 3; 5; 7 ] (factors 315))
       ; ("Find number of nodes"
          >:: fun _ -> assert_equal 4 (num_branches example_balanced_tree))
       ; ("Find longest branch of tree"
          >:: fun _ -> assert_equal 5 (longest_branch example_unbalanced_tree))
       ; ("Find shortest branch of tree"
          >:: fun _ -> assert_equal 2 (shortest_branch example_unbalanced_tree))
       ; ("Find shortest branch of tree"
          >:: fun _ -> assert_equal 2 (shortest_branch example_unbalanced_tree_v2))
       ; ("Construct a balanced tree"
          >:: fun _ ->
          assert_equal
            (Node
               ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty)))
            (construct_balanced_tree 4 'x'))
         ; ("Find all balanced trees"
            >:: fun _ ->
            assert_equal
              [ Node
                  ( 'x'
                  , Node ('x', Empty, Empty)
                  , Node ('x', Node ('x', Empty, Empty), Empty) )
              ; Node
                  ( 'x'
                  , Node ('x', Empty, Empty)
                  , Node ('x', Empty, Node ('x', Empty, Empty)) )
              ; Node
                  ( 'x'
                  , Node ('x', Node ('x', Empty, Empty), Empty)
                  , Node ('x', Empty, Empty) )
              ; Node
                  ( 'x'
                  , Node ('x', Empty, Node ('x', Empty, Empty))
                  , Node ('x', Empty, Empty) )
              ]
              (cbal 4 'x'))
       ]
;;

let _ = run_test_tt_main tests
