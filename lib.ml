open Core_kernel

let (<<) f g x = g x |> f
let (>>) f g x = f x |> g
let undef () = failwith "undefined"

let conv_sort : ('a array -> unit) -> 'a list -> 'a list = fun f xs ->
  let vec = Array.of_list xs in f vec; Array.to_list vec;;

let quicksort ?(hybrid=false) (xs : 'a array) : unit =
  let n = Array.length xs in
  if n > 1 then begin
    let swap i j = let t = xs.(j) in xs.(j) <- xs.(i); xs.(i) <- t in
    let rec go l r =
      if l < r then
        if hybrid && r - l <= 1024 then
          for i = (l+1) to r do
            let w = ref xs.(i) in
            let j = ref (i - 1) in
            while !j >= 0 && !w < xs.(!j) do
              xs.(!j + 1) <- xs.(!j);
              j := !j - 1
            done;
            xs.(!j + 1) <- !w;
          done
        else begin
          let pi = (l + r) / 2 in
          let pivot = xs.(pi) in
          let i = ref l in
          let j = ref r in
          while !i <= !j do
            while xs.(!i) < pivot do i := !i + 1 done;
            while xs.(!j) > pivot do j := !j - 1 done;
            if !i <= !j then begin
              swap !i !j;
              i := !i + 1;
              j := !j - 1;
            end
          done;
          go l !j;
          go !i r;
        end
    in
    go 0 (n - 1)
  end;;

let quicksort_l (xs : 'a list) : 'a list = conv_sort quicksort xs

let insertionsort (xs : 'a array) : unit =
  let n = Array.length xs in
  if n > 1 then
    for i = 1 to (n - 1) do
      let w = ref xs.(i) in
      let j = ref (i - 1) in
      while !j >= 0 && !w < xs.(!j) do
        xs.(!j + 1) <- xs.(!j);
        j := !j - 1
      done;
      xs.(!j + 1) <- !w;
    done;;

let insertionsort_l (xs : 'a list) : 'a list = conv_sort insertionsort xs

let rev_concat (xs : 'a list array) : 'a list =
  let i = ref ((Array.length xs) - 1) in
  let ret = ref [] in
  while !i >= 0 do
    ret := List.rev_append xs.(!i) !ret;
    i := !i - 1;
  done;
  !ret;;

let radixsort (xs : string array) : unit =
  let n = Array.length xs in
  let index = ref ((Array.map ~f:String.length xs |> Array.fold ~init:1 ~f:max) - 1) in
  let ret = ref (List.range 0 n) in
  while !index >= 0 do
    let buckets = Array.create 256 [] in
    List.iter !ret (fun i ->
      let bucket_index = 
        if !index < String.length xs.(i)
        then String.get xs.(i) !index |> Char.to_int
        else 0
      in buckets.(bucket_index) <- i :: buckets.(bucket_index);
    );
    ret := Array.to_list buckets |> List.concat;
    index := !index - 1;
  done;
  let i = ref 0 in
  List.iter (List.map ~f:(fun i -> xs.(i)) !ret) (fun x ->
    xs.(!i) <- x;
    i := !i + 1;
  );;

let radixsort_l (xs : 'a list) : 'a list = conv_sort radixsort xs
