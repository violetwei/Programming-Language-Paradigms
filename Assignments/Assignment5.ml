(* Q1: For this question, you need to implement 6 functions covering the logic of the full algorithm. 

1. val areNeighbours : country -> country -> chart -> bool = <fun> should return true iff the two countries ct1 and ct2 given are in the chart, whether as (ct1, ct2) or (ct2, ct1).
2. val canBeExtBy : colour -> country -> chart -> bool = <fun> should return true iff the given country can be added to the given colour.
3. val extColouring : chart -> colouring -> country -> colouring = <fun> should return an extended version of the given colouring by inserting the given country into the first valid colour.
4. val removeDuplicates : ’a list -> ’a list = <fun> is pretty self-explanatory. However, make sure to keep the first instance of any duplicates.
5. val countriesInChart : chart -> country list = <fun> returns the list of countries involved in a chart with no duplicates.
6. val colourTheCountries : chart -> colouring = <fun> uses all previous functions to actually create a colouring! 

In this question you will implement a simple map colouring problem. 
We will represent a map showing countries by describing which countries share a border. 
A country is just a string. A chart is simply a map (in the normal sense of the word) of some countries. 
It is represented as a list of pairs of countries. If (a, b) is in the list it means that the countries a and b share a border. 
This relation is symmetric but we will not put both (a, b) and (b, a) in the list.

We want to colour the chart so that two countries that share a border are not given the same colour. 
We will not name the colours; we simply view a colour as a set of countries that share the same colour; 
such a set is represented as a list of countries. A colouring is a set of colours; hence a set of sets of countries: 
this is represented as a list of lists of countries. 
The algorithm takes a chart, and an initially empty colouring and then tries to extend the colouring 
by adding countries from the chart. It works by naively checking if a country can be added to a given colour by 
making sure that it is not a neighbour of any of the countries already with that colour.
*)

(* Question 1.1 *)

let areNeighbours ct1 ct2 (cht : chart) =
  List.mem (ct1,ct2) cht || List.mem (ct2,ct1) cht 
;;

(* Question 1.2 *) 
let canBeExtBy (col:colour) (ct: country) (ch : chart) = 
  if ((List.map (fun x -> (if(areNeighbours ct x ch) then ct else x)) col) = col) then true 
  else false 
    
;;

(* Question 1.3 *)

let rec extColouring (cht: chart) (colours : colouring) (cntry : country) = 
  match colours with
  | [] -> [[cntry]]
  | x::xs -> if canBeExtBy x cntry cht
      then ((([cntry] @ x) :: xs):colouring)
      else (([x]:colouring) @ (extColouring cht xs cntry)) 
;;

(* Question 1.4 *) 

let rec removeDuplicates lst =
  match	lst	with	
  |	[]	->	[]	
  |	[hd]	->	[hd]	
  |	h::t -> h::(removeDuplicates(List.filter(fun x -> x<>h)t))
  (*List.filter (fun x -> if (List.exists x lst) then x else [] ) lst*)  
;;

(* Question 1.5 *) 
let rec apptr l k =
  let ln = List.rev l in
  let rec app ln k acc = match ln with
    | [] -> acc
    | h::t -> app t k (h::acc) in
  app ln k k
;;
  
let countriesInChart (cht: chart) =
  removeDuplicates ((fun (x,y) -> apptr x y) (List.split cht))
;;

(* Quesiton 1.6 *)

let colourTheCountries (cht : chart) =
  List.fold_left (fun col2 x -> extColouring cht col2 x) [] (countriesInChart cht) ;;
  
  
(* Q1 solution given by prof. *)

type country = string;;
type chart = (country * country) list;;
type colour = country list;;
type colouring = colour list;;

let areNeighbours ct1 ct2 (cht : chart) =
   (List.mem (ct1,ct2) cht) || (List.mem (ct2,ct1) cht);;

let canBeExtBy (col:colour) (ct: country) (ch : chart) =
  List.for_all (fun c -> not (areNeighbours c ct ch)) col;;

let rec extColouring (cht: chart) (colours : colouring) (cntry : country) =
  match colours with
  | [] -> [[cntry]]
  | col :: rest -> 
     if (canBeExtBy col cntry cht)
    then
      (cntry :: col) :: rest
    else
      col :: (extColouring cht rest cntry);;

let rec removeDuplicates lst =
  match lst with
  | [] -> []
  | x :: xs -> x :: (List.filter (fun u -> not (u = x)) (removeDuplicates xs));;

let countriesInChart (cht: chart) =
  removeDuplicates (List.fold_left (fun lst (c1,c2) -> c1:: (c2 :: lst)) [] cht);;

let myWorld:chart = [("Andorra","Benin");("Andorra","Canada");("Andorra","Denmark");("Benin","Canada"); ("Benin","Denmark");("Canada","Denmark");("Estonia","Canada");("Estonia","Denmark");("Estonia","Finland");("Finland","Greece");("Finland","Benin");("Greece","Benin");("Greece","Denmark");("Greece","Estonia")];;

let colourTheCountries (cht : chart) =
  List.fold_left (extColouring cht) [[]] (countriesInChart cht);;
