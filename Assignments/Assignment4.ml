(* Assignment 4 *)
exception NotImplemented;;

type 'a tree =
  Empty | Node of 'a tree * 'a * 'a tree
;;

let deriv((f: float -> float), (dx: float)) =
  fun (x:float) -> (((f (x +. dx)) -. (f x))/.dx)
;;

let iterSum(f, (lo:float), (hi:float), inc) =
  let rec helper((x:float), (result:float)) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0)
;;

let integral((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  let delta (x:float) = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta)
;;

(* Q1: you will use the pre-defined polymorphic binary tree type in the prelude.

Write the function map_tree : ('a -> 'b) -> 'a tree -> 'b tree. It takes two arguments, a function f and a tree t. For each non-empty node of the tree, the function should be applied to the data and the resulting tree returned. 
Hint: this should not be any longer than 3-4 lines

Here is an example of use:

# let t1 = Node(Empty, 0, (Node(Empty, 1, Empty)));;
let t2 = Node(Node(Empty,5,Empty),6,Empty);;
let t3 = Node(Node(t2,2,Node(Empty,3,Empty)),4,t1);;
# t3;;
- : int tree =
Node (
  Node 
    (Node (Node (Empty, 5, Empty), 6, Empty), 
    2, 
    Node (Empty, 3, Empty)),
  4, 
  Node (Empty, 0, Node (Empty, 1, Empty))
)
# mapTree (fun x -> x + 1) t3;;
- : int tree =
Node (
  Node 
    (Node (Node (Empty, 6, Empty), 7, Empty), 
    3, 
    Node (Empty, 4, Empty)),
  5, 
  Node (Empty, 1, Node (Empty, 2, Empty))
)
*)

(* My solution to Question 1 *)

let rec mapTree f (t: 'a tree) =
  match t with 
  | Empty -> Empty 
  | Node (l, m, r) -> Node(mapTree f l, f m, mapTree f r)
;;

(* Question 1 given by prof. *)
let rec mapTree f (t: 'a tree) =
  match t with
  | Empty -> Empty
  | Node(l,v,r) -> Node((mapTree f l), (f v), (mapTree f r));;

(* Q2: Suppose that f is a continuous function from the real numbers to the real numbers. 
       Suppose further that there is an interval [a,b] in which the function is known to have exactly one root 
       (a value r such that f(r) = 0). We will assume that f(a) < 0 and f(b) > 0 and the root r is somewhere in between. 
       There is an algorithm that is reminiscent of binary search that finds (a good approximation of) the root. 
       The half-interval method for finding the root of a real-valued function works as follows. 
       The idea is to search for the root by first guessing that the root is the midpoint of the interval. 
       If the midpoint is the root we are done; if not, we check whether the value of f at the midpoint is positive or negative. 
       If it is positive then the root must be between a and the midpoint; if it is negative, the root must be between the midpoint and b. 
       In either case we recursively call the search algorithm.

       Code this algorithm in OCaml by writing the function halfint (f, epsilon) (negValue, posValue). 
       It takes a float -> float function f, two float values negValue, posValue corresponding to a and b respectively, and a float precision factor epsilon. 
       Hint: you can use the native function abs_float.
*)

(* My solution to Question 2. *)

let rec halfint (f, (epsilon : float)) ((negValue : float), (posValue : float)) =
  let mid:float = ((posValue +. negValue)/.2.0) in 
  if f(mid) = 0.0 then mid 
  else if f(mid) > 0.0 then halfint(f,epsilon)(negValue, mid) 
  else halfint(f,epsilon)(mid,posValue) 
;;

(* Question 2 solution given by prof. *)

(* Floating-point version of absolute value. *)
let absFl(x:float) = if (x < 0.0) then -.x else x;;

let rec halfint((f: float -> float),(posValue:float),(negValue:float),(epsilon:float)) =
  let mid = (posValue +. negValue)/.2.0 in
  if ((absFl(f mid)) < epsilon)
  then
    mid
  else
    if (f mid) < 0.0
    then
      halfint(f,posValue,mid,epsilon)
    else
    halfint(f,mid,negValue,epsilon);;
    

(* Q3: The Newton-Raphson method is a classic higher-order function in action. 
       The main point is that we can attempt to find a root of a function using the function and its derivative. 
       This is an important approach that could be - and is - used in rocket science.
       Implement a function newton (f, epsilon, dx) guess using the provided template that attempts to 
       find a root of the function f near the guess value, by using the iterative formula
       xn+1=xn−f(xn)/f′(xn)
       where guess is the iterative xn. 
       Hint: Use the function and dx as well as the helper method deriv to create a derivative function.
*)

(* My solution to Question 3. *)

let rec newton (f, (epsilon:float), (dx:float)) (guess:float) =
  let close x y = abs_float (x -. y) < epsilon in
  let improve (guess:float) = guess -. (f guess /. (deriv(f, dx)) guess) in
  if close (f guess) 0.0 then guess
  else newton (f, epsilon, dx) (improve (guess))
;; 

(* Question 3. *)
let deriv((f: float -> float), (dx: float)) = fun (x:float) -> (((f (x +. dx)) -. (f x))/.dx);;

let rec newton(f, (guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = absFl(x-.y) < epsilon in
  let improve((guess:float),f,(dx:float)) = guess -. (f guess)/.((deriv(f,dx)) guess) in
  if close((f guess), 0.0, epsilon)
  then
    guess
  else
    newton(f,improve(guess,f,dx),epsilon,dx);;
    
(* For testing *)
let make_cubic((a:float),(b:float),(c:float)) = fun x -> (x*.x*.x +. a *. x*.x +. b*.x +. c)
let test1 = newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)
(* Should get -3.079599812; don't worry if your last couple of digits are off. *)
let test2 = newton(sin,5.0,0.0001,0.0001)
(* Should get 9.424.... *)

(* Q4: Implement the function indIntegral (f, dx) : float -> float that returns another function, 
       which would compute an indefinite integral. The returned function should be equivalent to
       indIntegral(f,dx)=∫x0f(y)dy
       with dx as the approximated rectangle width. You may use any previous functions or the ones in the prelude.
*)

(* My solution to Question 4. *)

let indIntegral (f, (dx:float)) =
  fun x -> integral (f, 0.0, x, dx)
;;

(* Question 4 solution given by prof. *)
(* The first two functions below should be preloaded into the environment. *)
                  
  let iterSum(f, (lo:float), (hi:float), inc) =
    let rec helper((x:float), (result:float)) =
      if (x > hi) then result
      else helper((inc x), (f x) +. result)
    in
    helper(lo,0.0);;

let integral((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  let delta (x:float) = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

(* This is all they have to write. *)

let indIntegral(f,(dx:float)) = fun (x:float) -> integral(f,0.0,x,dx);;
