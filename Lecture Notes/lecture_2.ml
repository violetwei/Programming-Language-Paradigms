(* COMP302 Winter2019 Lecture 2(Jan. 9th) notes written in OCaml *)

(* The tail recursive version*)
let fact n =
  let rec helper n acc =
    if n = 0 then acc
    else
      helper (n-1, n*acc)
  in 
  helper n 1

(* The recursive version *)  
let rec fac n =
  if n = 0 then 1
  else n * fact (n-1)

let rec fib n =
  if n = 1 then 1
  else if n = 2 then 1
  else fib (n - 1) + fib (n-2)

let fib n = 
  let rec helper n a b =
    if n = 1 then a
    else if n = 2 then b
    else helper (n - 1) b (a + b)
  in
  helper n 1 1


let add_pair pair = fst pair + snd pair;;

(* pattern matching*) 
let add_pair pair =
  match pair with
  | (1, 1) -> -1
  | (x, y) -> x + y 
;;   

let add_pair (x, y) = x + y ;;

let f pair =
  match pair with
  | x -> x 
;;
  
(* [] emplity list is a list*)
(* if n is an integer and l is a list, then n :: l is a list *)
1 :: [] ;; (* [1] *)
2 :: (1 :: []) ;; (* [2; 1] *)
[1; 2] @ [3; 4] ;; (* [1; 2; 3; 4] *)

let vowels = ['a'; 'e'; 'i'; 'o'; 'u'] ;;
vowels @ ['y'] ;; (* char list = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'] *)
'y' :: vowels ;; (* char list = ['y'; 'a'; 'e'; 'i'; 'o'; 'u'] *)

(* take list apart *)
List.hd vowels ;; (* 'a' *)
List.tl vowels ;; (* ['e'; 'i'; 'o'; 'u'] *)
(List.hd vowels) :: (List.tl vowels) ;; (* char list = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'] *))

(* l1 = [1; 2; 3; 4]  l2 = [11; 12; 13; 14] --> [1; 11; 2; 12; 3; 13; 4; 14] *)
let rec badzip li l2 =
  if l1 = [] then l2
  else
    (List.hd li) :: badzip l2 (List.tl li)
