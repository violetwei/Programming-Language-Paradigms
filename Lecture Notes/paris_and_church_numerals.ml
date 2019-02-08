(* Data structures with pure function. *)

let tt = fun x -> fun y -> x;;
let ff = fun x -> fun y -> y;;
let makePair = fun x -> fun y -> fun z -> z x y;;
let fst p = p tt;;
let snd p = p ff;;

(* Arithmetic with Church numerals *)
let zero = fun f -> (fun x -> x)

let one = fun f -> (fun x -> (f x))

let two = fun f -> (fun x -> (f (f x)))

let three = fun f -> (fun x -> (f (f (f x))));;                 

(* This is just for display purposes. *)
let showcn cn = (cn (fun n -> n + 1)) 0

let r1 = showcn one

let r2 = showcn two

let succ cn =  (fun f -> (fun x -> f ((cn f) x)))

let r3 = showcn (succ two)

let add n m = fun f -> (fun x -> ((n f) ((m f) x)))

let times n m = fun f -> (fun x -> (n (m f) x))

let exp n m = fun f -> (fun x -> (m n) f x)
