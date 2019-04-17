(* Q1. Parsing
In this question you will implement a parser for the language of algebraic expressions described in class on the 11th of March. 

We use the following type definition to capture expression trees:

type exptree =
  | Var of char
  | Expr of char * exptree * exptree
This definition is given in the prelude.

The input will be just plain strings and the output has to be an exptree. 
The input strings consist of simple algebraic expressions with + and * and one-character symbols that are just lower-case letters. 
It is also possible to use parentheses in the input expressions. 
Blanks are not allowed in the input1. 
The parser itself will consist of three mutually recursive functions as explained in class; see the given template. *)

(* Solution to Q1. Parser *)
type exptree = Var of char | Expr of char * exptree * exptree;;

let charSet = 
 let rec aux i acc =
  if i < 97 then acc else aux (i-1) ((Char.chr i)::acc)
 in aux 122 [];;

let isin (x: char) lst = List.exists (fun y -> x = y) lst;;

let parse (inputexp: string): exptree = 
  let sym = ref inputexp.[0] in
  let cursor = ref 0 in 

  let getsym () =
    (cursor := !cursor + 1);
    (sym := inputexp.[!cursor])
  in

  let rec expr (): exptree =
    let result = term () in
    if !sym = '+' then
      Expr('+',result, (getsym();expr()))
    else
      result
  and term (): exptree =
    let result = primary() in
    if !sym = '*' then
      Expr('*',result, (getsym();term()))
    else
      result
  and primary (): exptree = 
    if !sym = '('
    then
      begin
        getsym();
        let result = expr () in
        if not (!sym = ')') then 
          failwith "Mismatched parens"
        else 
          if (!cursor = String.length(inputexp) - 1) 
          then 
            result
          else 
            ((getsym ()); result)
      end
    else
      if (isin !sym charSet)
      then 
        if (!cursor = String.length(inputexp) - 1) 
        then 
          (Var !sym) 
        else 
          let result = Var !sym in (getsym(); result)
      else
        failwith "In primary"
        (* Printf.printf "sym is : %c." !sym *)
  in
  expr();;

let example = "(a+(b+(c*d)+e)*f)*g";;
let example2 = "(a+b)*c";;
let example3 = "((a))+b";;
let example4 = "a+b+c+d";;
let example5 = "a+(b+(c+d))";;
let example6 = "((a+b)*(c+d)+e+f*g+h)";;
let badex1 = "a++b";;
let badex2 = "a+(b+c)*2";;
let badex3 = "a+)";;
let badex4 = "a+(b+";;

(*
# parse example;;
- : exptree =
Expr ('*',
 Expr ('+', Var 'a',
  Expr ('*',
   Expr ('+', Var 'b', Expr ('+', Expr ('*', Var 'c', Var 'd'), Var 'e')),
   Var 'f')),
 Var 'g')
# parse example2;;
- : exptree = Expr ('*', Expr ('+', Var 'a', Var 'b'), Var 'c')
# parse example3;;
- : exptree = Expr ('+', Var 'a', Var 'b')
# parse example4;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example5;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example6;;
- : exptree =
Expr ('+',
 Expr ('*', Expr ('+', Var 'a', Var 'b'), Expr ('+', Var 'c', Var 'd')),
 Expr ('+', Var 'e', Expr ('+', Expr ('*', Var 'f', Var 'g'), Var 'h')))

# let badex1 = "a++b";;
val badex1 : string = "a++b"
# parse badex1;;
Exception: Failure "In primary".
# let badex2 = "a+(b+c)*2";;
val badex2 : string = "a+(b+c)*2"
# parse badex2;;
Exception: Failure "In primary".
# let badex3 = "a+)";;
val badex3 : string = "a+)"
# parse badex3;;
Exception: Failure "In primary".
# let badex4 = "a+(b+";;
val badex4 : string = "a+(b+"
# parse badex4;;
Exception: Invalid_argument "index out of bounds".
# parse "a + b";;
- : exptree = Var 'a'
*)


(* Q2. Code Generation

In this question you will implement a toy code generator for the very simple stack machine as explained in class on March 13th. 
Use the output from the parser as input to the code generator. The code generator should just print the "machine instructions". 

Use statements like:

Printf.printf "LOAD  %c\n" c
Printf.printf "ADD  %c\n" c
Printf.printf "MUL  %c\n" c
Printf.printf "STORE %i\n" tempstore
Printf.printf "ADD %i\n" tempstore

within your program to get this effect.*)

let tempstore = ref 0;;
  
type exptree = Var of char | Expr of char * exptree * exptree;;

(* solution to q2. code generation *)
let codegen (e: exptree) = 
  let rec helper((e: exptree), (tag: char)) =
    match e with
      | Var c ->
        if (tag = '=') then
          Printf.printf "LOAD  %c\n" c
        else
          if (tag = '+')
          then
            Printf.printf "ADD  %c\n" c
          else (* tag = '*' *)
            Printf.printf "MUL  %c\n" c
      | Expr(op,l,r) -> 
         if (tag = '=') then
           (helper (l,'=');
           helper (r, op))
         else
           begin
             tempstore := !tempstore + 1;
             Printf.printf "STORE %i\n" !tempstore;
             helper(l,'=');
             helper(r,op);
             (if (tag = '+')
             then
               Printf.printf "ADD %i\n" !tempstore
             else
               Printf.printf "MUL %i\n" !tempstore);
             tempstore := !tempstore - 1
           end
  in
  helper(e,'=');;

(*
         
let exptree = Expr ('+',Var 'a',Expr ('*',Var 'b',Expr ('+',Var 'c',Var 'd')));;
# codegen exptree;;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
ADD  d
MUL 2
ADD 1
- : unit = ()

*)
