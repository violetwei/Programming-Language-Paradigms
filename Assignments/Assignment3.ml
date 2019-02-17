(* Assignment 3 *)

exception NotImplemented;;

let rec memberof (n, l) =
  match l with
  | [] -> false
  | x :: xs -> if x = n then true else memberof (n, xs)
;;

let remove (item, lst) =
  List.filter (fun u -> not (u = item)) lst
  
(* Q1: Write a function common that takes a pair of lists and forms a new list containing a unique copy of each element 
       that occurs in both lists. Here is the type, as echoed by the interpreter, and an example.
       
       val common : 'a list * 'a list -> 'a list = <fun>

       # let l1 = [1; 3; 2; 4; 1; 5; 6; 3];;
       val l1 : int list = [1; 3; 2; 4; 1; 5; 6; 3]
       # let l2 = [3; 9; 8; 2; 11; 21; 3];;
       val l2 : int list = [3; 9; 8; 2; 11; 21; 3]
       # common (l1,l2);;
       - : int list = [3; 2]
*)

(* My solution to Question 1 *)

let rec common twolists = 
  match fst twolists with
  | [] -> if snd twolists = [] then [] else common (snd twolists, fst twolists)
  | h::t ->
      if memberof (h, snd twolists) then
        let b' = remove (h, snd twolists) in
        h::(common (t, b'))
      else
        common (t, snd twolists)     
;; 

(* The following three questions are about merge sort. 
   The mergesort algorithm is a recursive algorithm for sorting lists which runs in time O(nlogn). 
   The items in the list must have an order relation defined on them, otherwise sorting does not make sense of course.

   The idea is as follows: the given list l is split into two equal 
   (if the length of l is odd then one of the "halves" is one item longer thannthe other) lists l1 and l2. 
   These lists are sorted recursively and then the results are merged back to give a single sorted list. 
   Code this in OCaml. Your algorithm can use < as a comparison operator. 
   Your code must have a function split that produces a pair of lists, 
   a function merge that merges sorted lists and a function mergesort that implements the overall algorithm.   
*)  

(* Q2: In this question you will implement split with the following type:

   val split : 'a list -> 'a list * 'a list = <fun>

   # split [1; 3; 2; 4; 5; 6; 9; 11; 17; 13; 12];;
   - : int list * int list = ([1; 2; 5; 9; 17; 12], [3; 4; 6; 11; 13])
*)   

(* My solution to Question 2. Mergesort requires that you use recursion.  
   Using List.sort or some other sort defeats the whole purpose.  
   This question is for the implementation of split.*)

let rec split l =
  match l with
  | x::y::tail ->
      let a,b = split tail in
      x::a, y::b
  | x::[] -> [x],[]
  | [] -> [],[]
;;

(* Q3: you will implement the merge algorithm on ordered lists.

   val merge : 'a list * 'a list -> 'a list = 
   # let l3 = [1; 3; 5; 7; 9];;
   val l3 : int list = [1; 3; 5; 7; 9]
   # let l4 = [2; 4; 6; 8];;
   val l4 : int list = [2; 4; 6; 8]
   # merge (l3,l4);;
   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
*)

(* My solution to Question 3 *)

let rec merge twolists =
  match fst twolists, snd twolists with
  | [], _ -> snd twolists
  | _, [] -> fst twolists
  | hx :: txs, hy :: tys ->
      if hx < hy then hx :: merge (txs, snd twolists) else hy :: merge (fst twolists, tys)
;;

(* Q4: complete the mergesort algorithm. Here is the type and an example.

   val mergesort : 'a list -> 'a list = 
   # mergesort [10;2;8;5;1;4;3;9;7;6];;
   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   # mergesort [1;3;2;4;1;2;5;3];;
   - : int list = [1; 1; 2; 2; 3; 3; 4; 5]
*)

(* My solution to Question 4 *)

let rec mergesort l = 
  match l with
  | ([] | _::[]) -> l
  | _ -> let (pri,seg) = split l
      in (merge ((mergesort pri), (mergesort seg))) 
;;
