(* Topic: List manipulation *)

(* Q1: Write a program that takes two lists of the same length and returns a single list with the items paired together. 
   This will be used in later questions in this assignment 
   # let rec pairlists twolists = ...
   val pairlists : 'a list * 'b list -> ('a * 'b) list = <fun>
   We will forbid the use of the List.combine library function that does the same thing. 
   Test inputs are two int lists of the same length and we will check that you have handled the case of empty input lists. 
  
   Here is an example showing the right way and the wrong way to use pairlists.  
   # let l1 = [1; 2; 3];;
   val l1 : int list = [1; 2; 3]
   # let l2 = ['a'; 'b'; 'c'];;
   val l2 : char list = ['a'; 'b'; 'c']
   # let p1 = pairlists (l1, l2);;
   val p1 : (int * char) list = [(1, 'a'); (2, 'b'); (3, 'c')]
   # let e1 = pairlists l1 l2;;
   Characters 9-18:
   let e1 = pairlists l1 l2;;
            ^^^^^^^^^
   Error: This function has type 'a list * 'b list -> ('a * 'b) list
   It is applied to too many arguments; maybe you forgot a ';'.   
*)
   

(* Part A: My answer *)
let rec pairlists (l1, l2) = 
  match l1, l2 with
  | [],_ -> []
  | _,[] -> []
  | (x :: xs),(y::ys) -> (x,y) :: pairlists (xs, ys) 
;;

(* Q1 Part B: If (wi) is a sequence of weights and (vi) is a sequence of values we define the weighted mean 
   or average by the formula *)
   
(* Part B: My answer *)   
let wmean weights data =
  sumlist (List.map (fun (x,y) -> x *. y) (pairlists (weights, data) )) /. sumlist weights
;;
