
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
(* Inert a number in an ordered list of numbers *)
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

(* fold_left is higer-order-function which combines elements inthe list from left to right *)
fold_left ;;
- : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
fold_left (fun n m -> n + m) 0 foo ;; (* - : int = 45 *)
fold_left (fun n m -> n + m) 5 foo ;; (* - : int = 50 *)

(* Use fold to reverse a list *)
let freverse l = fold_left (fun lst item -> item :: lst) [] l ;;
(* val freverse : 'a list -> 'a list = <fun> *)
let freverse l = fold_left (fun lst -> fun item -> item :: lst) [] l ;;
(* val freverse : 'a list -> 'a list = <fun> *)
freverse foo ;; (* - : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1] *)

(* A function that takes a string and splits into a list of characters *)
let explode s =
  let rec exp i l = 
    if i < 0 then l else exp (i-1) (s.[i] :: l)
  in exp (String.length s-1) [] ;;
(* val explode : string -> char list = <fun> *)
explode "hello world" ;;
- : char list = ['h'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd']

let count_vowels (str:string) = 
  let char_list = explode str
  in 
  let acc_fun (a, e, i, o, u) letter =
    match letter with  (* pattern matching *)
    | 'a' -> (a+1, e, i, o, u)
    | 'e' -> (a, e+1, i, o, u)
    | 'i' -> (a, e, i+1, o, u)
    | 'o' -> (a, e, i, o+1, u)
    | 'u' -> (a, e, i, o, u+1)
    | _ -> (a,e,i,o,u)
  in List.fold_left acc_fun (0,0,0,0,0) char_list ;;
val count_vowels : string -> int * int * int * int * int = <fun>

count_vowels "Twas brilling and the slithy toves did gyre and gimble in the wabe." ;;
- : int * int * int * int * int = (4, 6, 6, 1, 0)

(* break and reconstruct a list; tuple is a packaging of information taken as a unit*)

filter ;;
- : ('a -> bool) -> 'a list -> 'a list = <fun>

(*first filter everything out, then can apply map, fold_left......*)

let positive lst = filter (fun x -> x > 0) lst ;;
(* val positive : int list -> int list = <fun> *)
positive [1;2;-3;4;-5;10] ;;
- : int list = [1; 2; 4; 10]

let is_positive x = x > 0 ;;
val is_posotive : int -> bool = <fun>

let positive lst = filter is_posotive lst ;;

fold_right ;;
- :  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>
fold_left ;;
- : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>

let find_min lst = fold_right (fun item m -> min item m) lst max_int ;;
val find_min : int list -> int = <fun>
find_min [4;5;6;3;5] ;;    
(* - : int = 3 *)
