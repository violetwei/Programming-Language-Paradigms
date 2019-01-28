(* Binary Search Trees coded from scratch.  *)
type 'key bstree = Empty | Node of 'key * 'key bstree * 'key bstree

let rec find less x (t : 'key bstree) =
  match t with
    | Empty -> false
    | Node(y,left,right) -> 
        if (less(x,y)) then find less x left
        else
          if (less(y,x)) then find less x right
        else (* x = y *) true;;

let rec insert less x (t: 'key bstree) =
  match t with
    | Empty -> Node(x,Empty,Empty)
    | (Node(y,left,right)) as nn ->
        if (less(x,y)) then Node(y,(insert less x left),right)
        else
          if (less(y,x)) then Node(y,left,(insert less x right))
        else (* x = y *) nn;;

exception EmptyTree

let rec deletemin (t: 'key bstree) =
  match t with
    | Empty -> raise EmptyTree
    | Node(y,Empty,right) -> (y,right)
    | Node(y,left,right) -> 
        let (z,ll) = deletemin(left) in
        (z,Node(y,ll,right));;


let rec delete less x (t: 'key bstree) =
  match t with
    | Empty -> raise EmptyTree
    | Node(y,left,right) ->
        if (less(x,y)) then Node(y,(delete less x left),right)
        else
          if (less(y,x)) then Node(y,left,(delete less x right))
        else (* x = y *)
          match (left,right) with
            |  (Empty,r) -> r 
            |  (l,Empty) -> l 
            |  (l,r) ->
               let (z,r1) = deletemin(r) in Node(z,l,r1)


let strLess((s1:string), s2) = s1 < s2;;

let tr1 = insert strLess "monkey" Empty;;
let tr2 = insert strLess "donkey" tr1;;
let tr3 = insert strLess "elephant" tr2 
let tr4 = insert strLess "chimpanzee" tr3 
let tr5 = insert strLess "aadvaark" tr4 
let tr6 = insert strLess "buffalo" tr5 
let tr7 = insert strLess "giraffe" tr6 
let tr8 = insert strLess "rhinocerous" tr7 
let tr9 = insert strLess "okapi" tr8 
let tr10 = insert strLess "zebra" tr9 
let tr11 = insert strLess "tiger" tr10
