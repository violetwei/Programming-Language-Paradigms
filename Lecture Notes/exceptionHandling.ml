exception Empty_list;;

let gethead (l : int list) =
  match l with
    | [] -> raise Empty_list
    | x :: xs -> x;;

let solve(a,b,c) =
  let disc = (b *. b -. 4.0 *. a *. c) in
  if  disc < 0.0 || a = 0.0 then
    failwith "The discriminant is negative or a was zero."
  else
    ((-.b +. sqrt(disc))/.(2.0*.a),(-.b -. sqrt(disc))/.(2.0*.a));;

exception Complex;;
exception Linear;;

let solve_quad(a,b,c) =
  let disc = (b *. b -. 4.0 *. a *. c) in
  if  a = 0.0 then
    raise Linear
  else
    if disc < 0.0
    then raise Complex
    else
      ((-.b +. sqrt(disc))/.(2.0*.a),(-.b -. sqrt(disc))/.(2.0*.a));;
    

let solve_text(a,b,c) = let (r1,r2) = solve_quad(a,b,c) in
                        "Roots are: " ^ (Float.to_string r1)^" and "^(Float.to_string r2);;

let solve_catch(a,b,c) = 
  try
    solve_text(a,b,c)
  with
  | Linear -> "Degenerate linear equation"
  | Complex -> "The roots are complex";;

(* This produces a type error. 
let bad_solve(a,b,c) =
  try
    solve_quad(a,b,c)
  with
  | Complex -> "No real solutions";;
*)

let solve_robust(a,b,c) =
  try
    let disc = (b *. b -. 4.0 *. a *. c) in
    if  a = 0.0 then
      raise Linear
    else
      if disc < 0.0 then
      raise Complex
      else
        ((-.b +. sqrt(disc))/.(2.0*.a),(-.b -. sqrt(disc))/.(2.0*.a))
  with
  | Linear -> (print_string "This is not a quadratic you idiot!");
               (-. c /. b, -. c /. b)
  | Complex -> (print_string "The roots are complex.  The real and imaginary parts are:");
               let disc =  (b *. b -. 4.0 *. a *. c) in
               let realpart = -. b/.(2.0 *. a) in
               let imagpart = (sqrt(-.disc))/.(2.0 *. a) in
               (realpart,imagpart);;

(* Built-in exceptions *)

let basic n = if n = 0 then 0 else failwith "N was not 0"

let fact n =
  let rec helper (n,m) =
    if (n = 0) then m else helper(n-1,n*m)
  in
  if n < 0 then invalid_arg "n cannot be negative"
  else helper(n,1);;


(* Using exceptions for backtracking. *)

exception Change;;

let makeChange coinTypes amt = 
  let rec helper((coins: int list), (amt:int)) : int list =
    match (coins, amt) with
    | (_,0) -> []
    | ([],_) -> raise Change
    | (coin::rest,amt) -> 
       try
         if 
           (coin > amt) 
         then 
           helper(rest,amt) 
         else 
           coin::(helper(coins, amt - coin))
       with 
         | Change -> helper(rest,amt)
  in
  try
    let ch = helper(coinTypes,amt) in
    ("Return the following coins: "
     ^ (List.fold_left (fun s -> fun n -> (string_of_int n) ^ " " ^ s) " " ch) ^ "\n")
      
    with
      | Change -> "Sorry, I cannot make change.\n"
