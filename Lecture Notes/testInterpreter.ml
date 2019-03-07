let p = Let ( [("b", Num 2);("d",Num 4)], Apply(Lambda ("c", Plus(Plus(Var "c", Var "b"), Var "d")), Num 3));; 

let q = Let ( [("b", Num 2)], Apply(Lambda ("c",Plus(Var "c", Var
"b")),Num 3));; 

let x = Lambda ("y",Plus (Var "y", Var "y"));;

let z = Apply (x, Num 2);;

let l1 = [(Var "a",Int 2);(Var "b",Int 3)];;

let l2 = [(Var "c", Int 3);(Var "d", Int 4)];;

let e1 =Env ( [Layer l1;Layer l2]);;


let d = Apply (Lambda ("e",Plus (p, Var "e")), Num 5);;

let e = Let ([("f",q)], Apply (Lambda("s", Plus(Plus(Var "s",q), 
                                                       Var "f")), Num 3));; 

(* This returns a function as a closure. *)
let f = Apply(Lambda ("x", Var "x"), Lambda ("u", Var "u"));;

let k1 = Let ([("x", Num 1)], Let ([("f", Lambda ("u", Plus(Var "u", Var "x")))], Let ([("x", Num 2)], Apply(Var "f", Var "x"))));;

let k2 = Apply (Lambda ("x", Var "x"), (Lambda ("y", Plus (Var "y", Num 1))));;

(* Use this with e1 *)
let k3 = Apply (Lambda ("x", Plus (Var "x", Var "a")), Var "c");;

let fresh = Env [ (Layer []) ]

let d1 = Apply(Lambda ("f", Lambda ("x", Apply (Var "f", Var "x"))),
               Lambda ("n", Plus(Var "n", Num 1)))

let d2 = Apply(d1, Num 3)
