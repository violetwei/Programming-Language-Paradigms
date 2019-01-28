(* Q1: Here is a way to compute the square root of a positive real number say x. 
   We make a wild guess that the square root is some number g. 
   Then we check if g2 is close to x. If it is we return g as our answer; if not we refine our guess using the formula 
   g′=(g+x/g)/2.0  where g′ is the new guess. 
   We keep repeating until we are close enough. Write an OCaml program to implement this idea. 
   Since we are working with floating point numbers we cannot check for equality; we need to check if the numbers are close enough. 
   I have written a function called close for you as well as a function called square. 
   In OCaml floating point operations need special symbols: you need to put a dot after the operation. 
   Thus you need to write, for example, 2.0 +. 3.0. 
   All the basic arithmetic operations have dotted versions: +., ∗., /. and must be used with floating point numbers. 
   You cannot write expressions like 2 +. 3.5 or 2.0 + 3.0. We will not test your program on negative inputs so we are not requiring you to deal with the possibility that you may get a negative answer. 
 *)
 
 (* My solution to Q1 *)
 let mysqrt (x:float) = 
  let rec helpers ((a:float), (b:float)) = 
    if close(a, square b) then b 
    else helpers (a, (b +. (a/.b)) /. 2.0)
  in
  helpers (x, 1.0);;
  
  
(* Question 1 Square root using Newton's method. -- Solution given by Professor *)
open Float;;

let close((x:float), (y:float)) = (abs(x -. y) < 0.0001);;

let square(x:float) = x *. x;;

let rec mysqrt((x:float), (guess:float)) =
    if close(square(guess), x) then guess
    else mysqrt(x, (guess +. x/.guess)/.2.0);;
    

(* Q2: This is similar to the question above except that we are computing cube roots. 
   The formula to refine the guess is g′=(2.0⋅g+x/g^2)/3.0
   where I am using mathematical notation with arithmetic operation symbols overloaded. 
   In your code you must use the floating-point versions of the arithmetic operations.
*)


(* My solution to Q2 *)
let cube_root (x:float) = 
  let rec helperc ((a:float), (b:float)) = 
    if close(a, cube b) then b 
    else helperc (a, ((2.0 *.b) +. (a/.square b)) /. 3.0)
  in
  helperc (x, 1.0);;
  

(* Question 2 Cube root using Newton's method. Solution given by professor *)

let cube(x:float) = x*. x*. x;;

let rec cube_root((x:float), (guess:float)) =
  if close(cube(guess),x) then guess
  else cube_root(x, ((2.0 *. guess) +. x/.(square(guess)))/.3.0);;
  
  
  
