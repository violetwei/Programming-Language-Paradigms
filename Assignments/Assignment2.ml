(* Topic: List manipulation *)

(* Question 1 *)

(* Q1 Part A: Write a program that takes two lists of the same length and returns a single list with the items paired together. 
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

(* Q1 PartA Solution given by professor *)
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: (pairlists (xs,ys));;
    
    

(* Q1 Part B: If (wi) is a sequence of weights and (vi) is a sequence of values we define the weighted mean 
   or average by the formula mean=∑i wi⋅vi / ∑i wi   
   In this question you are given two lists of floats as input. 
   The first list is a list of weights and the second is the list of values. 
   Implement a function wmean which calculates the weighted mean of the values with the given weights.
   
   Here is the declaration and the type that we expect and an example of using it.
   # let wmean weights data = ....
   val wmean : float list -> float list -> float = <fun>
   # wmean [1.0; 1.5; 2.5; 0.5; 1.5] [10.3; 11.7; 2.0; 5.0; 6.5];;
   - : float = 6.44285714285714217
   
   In order to do this you are required to use your pairlists function from Q1(a) and the sumlist function that we have provided in the prelude. 
   In this exercise we want you to learn to use the List.map function from the library; 
   you are not allowed to use let rec in this exercise.    
*)
   
(* Part B: My answer *)   
let wmean weights data =
  sumlist (List.map (fun (x,y) -> x *. y) (pairlists (weights, data) )) /. sumlist weights
;;

(* Q1 Part B: solution given by professor *)

let rec sumlist l =
  match l with
  | [] -> 0.0 
  | (x::xs) -> x +. sumlist(xs);;
  
let w_mean weights data =
  let denom = sumlist weights in
  let pairs = pairlists (weights, data) in
  (sumlist (List.map (fun (x,y) -> x *. y) pairs))/.denom;;
  

(* Question 2
   This question involves two simple exercises in list manipulation. 
   In the first one you are given a pair: the first member of the pair is an item 
   and the second member is a list of items of the same type as the first member. 
   The output is a boolean that says whether the item is in the list or not. 
   The second function takes a pair of an item and a list and returns a list with the item removed. 
   If the item is not in the list then the same list is returned. Here are the declarations and types.
   
   # let rec memberof (n, l) = ...
   val memberof : 'a * 'a list -> bool = <fun>
   # let rec remove (item, lst) = ...
   val remove : 'a * 'a list -> 'a list = <fun>
   # remove (3, [1; 6; 3; 2; 6; 1; 7; 2; 3; 5]);;
   - : int list = [1; 6; 2; 6; 1; 7; 2; 5]
   
   The lists may contain duplicates; all copies should be removed. 
   Make sure that you deal with empty lists. 
   Here is an important style remark: never write if foo then true else false; this should just be foo     
 *)
 
(* My solution to Q2 *)

let rec memberof (n, l) =
  List.mem n l
;;

let rec remove (item, lst) =
  List.filter (fun x -> x != item) lst
;;  


(* Q2 solution given by professor *)

let rec memberof pair =
  match pair with
  | (n,[]) -> false
  | (n,(x::xs)) -> if (x = n) then true else memberof(n,xs);;

let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | (x::xs) -> if (x = item) then remove(item, xs) else x::(remove(item, xs));;
  
  
(* Question 3 
   In this question we want you to find the largest item in a list of integers. 
   You can assume that the lists are nonempty. 
   The top level declaration is nonrecursive because there is an internal recursive function.
   
   # let find_max l = ...
   val find_max : 'a list -> 'a = <fun>
   # find_max [1; 6; 3; 2; 6; 1; 7; 2; 3; 5];;
   - : int = 7
 *)
  
(* My solution to Question 3. *) 

let find_max l =
  match l with
    [] -> failwith "None"
  |h::t ->  let rec helper (seen,rest) =
              match rest with 
                [] -> seen
              |h'::t' -> let seen' = if h' > seen then h' else seen in 
                  let rest' = t'
                  in helper (seen',rest')
      in helper (h,t)   
;;

(* Question 3 solution given by prof. *)

let find_max l =
  let rec helper(l,m) =
    match l with
      | [] -> m
      | (x::xs) ->
         if (m < x) then helper(xs,x) else helper(xs,m)
  in
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x);;

   
(* Q4: use 'find_max' and 'remove' to implement a selection sort algorithm. 
   You can assume that the list contains no duplicates.
   # let rec selsort l = ...
   val selsort : 'a list -> 'a list = <fun>
   # selsort [1; 4; 2; 5; 3; 9; 6; 8; 7];;
   - : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1]
   You are not allowed to use built-in sorting functions.   
*)   

(* My solution to Question 4 *)

let rec selsort l =
  match l with
    [] -> []
  | _ -> let m = find_max l in m :: (selsort(remove(m, l)))
;;

(* Question 4 solution given by prof. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | _ -> let m = (find_max l) in
         m::(selsort(remove(m,l)));;
