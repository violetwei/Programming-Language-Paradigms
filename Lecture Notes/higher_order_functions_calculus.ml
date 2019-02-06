(* Doubler and self-application. *)
let twice f = fun x -> f (f x);;

let inc n = n + 1;;

let fourtimes f = (twice twice) f;;

let compose(f,g) = fun x -> g (f x);;

(* Some examples from calculus. *)
let deriv((f: float -> float), (dx:float)) = fun (x:float) -> ((f(x +. dx) -. f(x))/.dx);;

let absFl(x:float) = if (x < 0.0) then -.x else x;; 

let iterSum(f, (lo:float), (hi:float), inc) =
  let rec helper((x:float), (result:float)) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0);;

let integral((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  let delta (x:float) = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

let r_sq (x:float):float = x *. x;;

integral(r_sq,0.0,1.0,0.001);;

integral(sin,0.0, 3.14159, 0.001);;
