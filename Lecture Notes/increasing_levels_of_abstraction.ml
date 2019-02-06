let rec sumInts(a,b) = if (a > b) then 0 else a + sumInts(a+1,b);;

let rec sumSquares(a,b) = if (a > b) then 0 else (a*a) + sumSquares(a+1,b);;
let rec sumCubes(a,b) = if (a > b) then 0 else (a*a*a) + sumCubes(a+1,b);;

let rec sum(f,lo,hi) =
    if (lo > hi) then 0
    else (f lo) + sum(f,lo+1,hi);;

let square n = n * n;;
let cube n = n * n * n;;

let rec sumInc(f,lo,hi,inc) =
    if (lo > hi) then 0
    else (f lo) + sumInc(f, (inc lo), hi, inc);;

let byTwo n = n + 2;;

let rec product(f,lo,hi,inc) =
    if (lo > hi) then 1
    else (f lo) * product(f, (inc lo), hi, inc);;

let id x = x;;
let inc n = n + 1;;
product(id, 1, 5, inc);;

let acc(comb,f,lo,hi,inc,unit) =
  let rec helper(a, acc) =
    if (a > hi) then acc
    else
      helper((inc a), comb(acc, (f a)))
    in
        helper(lo, unit);;

(* The following computes sum of the squares of numbers from 1 to 5 *).

acc((fun (n,m) -> n + m), (fun x -> x * x), 1, 5, (fun n -> n + 1), 0);;
