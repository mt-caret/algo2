open Core_kernel
open Core_bench
open Lib

let run_tests () =
  let is_sorted l =
    let rec go = function
      | false, _ -> false
      | _, [] -> true
      | _, [_] -> true
      | _, x1 :: x2 :: xs -> go (x1 <= x2, x2 :: xs)
    in
    go (true, l)
  in
  let test_sort ~sort_name ~f =
    QCheck.Test.make ~count:100 ~name:(sort_name ^ "_actually_sorts")
      QCheck.(list small_nat)
      (fun l -> f l |> is_sorted)
  in
  let open QCheck_runner in
  let
    tests = "tests" >::: 
       [ test_sort ~sort_name:"quicksort" ~f:quicksort_l
       ; test_sort ~sort_name:"insertionsort" ~f:insertionsort_l
       ]
  in
  OUnit.run_test_tt_main tests

let canonicalize str =
  let is_alpha ch = let n = Char.to_int ch in 96 < n && n < 123 in
  let f = String.filter ~f:is_alpha << String.lowercase in
  String.split_on_chars ~on:[' '; '\n'] str
    |> List.map ~f
    |> List.filter ~f:(Fn.non String.is_empty)

(* approx. 100000 words *)
let get_input_list () =
  List.map ~f:canonicalize (In_channel.(input_lines stdin))
    |> List.concat

let run_sort () =
  let
    input = get_input_list () |> Array.of_list
  in
  (*insertionsort input*)
  quicksort input
  (*radixsort input |> Array.of_list*)

let run_benchmarks () =
  let input_list = get_input_list () in
  let
    shim ~sort ~sort_name n =
      let input = List.take input_list n |> Array.of_list in
      let name = sort_name ^ ": " ^ (string_of_int n) in
      Bench.Test.create ~name (fun () -> sort input)
  in
  let
    (*set ~f = List.map ~f:(fun x -> f (x * 1000)) (List.range 10 100)*)
    set ~f = List.map ~f:(fun x -> f (x * 5000)) (List.range 1 21)
  in
  let
    benchmarks = List.concat
      [ set ~f:(shim ~sort:(quicksort ~hybrid:true) ~sort_name:"quicksort") 
      ; set ~f:(shim ~sort:insertionsort ~sort_name:"insertionsort")
      ; set ~f:(shim ~sort:radixsort ~sort_name:"radixsort")
      ]
  in
  Bench.bench benchmarks

let show =
  printf "%s\n" << String.concat ~sep:", " << List.map ~f:string_of_int << Array.to_list

let run () =
  In_channel.(iter_lines stdin) ~f:(fun line ->
    let
      vec =
        String.split ~on:' ' line
        |> List.map ~f:Int.of_string
        |> Array.of_list
    in
    show vec;
    quicksort vec;
    show vec;
  )

let show_input () =
  get_input_list ()
  |> String.concat ~sep:"\n" 
  |> printf "%s\n"

let _ = run_benchmarks ()
