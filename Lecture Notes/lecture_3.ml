(* Comp302 -- Lecture 3*)

(* Function that take a list and reverse all the elements in the list - use pattern matcing *)
(* Linked list, runtime is O(n^2)*)
let rec reverse l =
  match l with
  | [] -> []
  | x::xs -> (reverse xs) @ [x] ;;

(* Tail recursion version of a function that reverse a list, runtime is O(n) *)
  let reverse l =
    let rec helper(l, acc) =
      match l with
      | [] -> acc
      | x::xs -> helper(xs, x::acc)
      in helper(l, []) ;;

(* Insertion Sort *)
(* insert (3, [1;2;3;5]) -> - : int list = [1; 2; 3; 4; 5] *)
let rec insert(n, l) =
  match l with
  | [] -> [n]
  | x::xs ->
    if (n < x) then n::(x::xs)
    else x::(insert(n, xs)) ;;      

(* val insertion_sort : 'a list -> 'a list = <fun> *)  
(* insertion_sort [8;2;5;1;3;2;6;9;7] ;; --> - : int list = [1;2;2;3;5;6;7;8;9] *)  
let rec insertion_sort l =
  match l with
  | [] -> []
  | x::xs -> insert(x, insertion_sort(xs)) ;;

(* concatenate a bunch of lists *)  
(* concat ;; --> - : 'a list list -> 'a list = <fun> *)
let foo = cancat [[1;2;3];[4;5;6];[7;8;9]] ;;
(* val foo : int list = [1;2;3;4;5;6;7;8;9] *)
nth foo 7 ;; (* - : int = 8 *)

(* higer-order function: take as input or output of function or both*)
map ;; (* - : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
map (fun n -> n * n) foo ;; (* - : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81] *)

let sqrd x = x * x ;;    
sqrd 4 ;; (* - : int = 16 *)
map sqrd foo ;; (* - : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81] *)

map String.length ["Hello"; "world"] ;; (* - : int list = [5; 5] *)

fold_left (fun n m -> n + m) 0 foo ;; (* - : int = 45 *)
fold_left (fun n m -> n + m) 5 foo ;; (* - : int = 50 *)

let freverse 1 = fold_left (fun lst item -> item :: lst) [] l ;;
(* val freverse : 'a list -> 'a list = <fun> *)
freverse foo ;; (* - : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1] *)
