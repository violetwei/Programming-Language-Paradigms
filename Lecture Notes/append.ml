(* Regular, non-tail recursive append - OCaml *)
let rec append (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
  | [] -> l2
  | x :: xs ->
     let r = append xs l2 in
     x :: r

let rec append_k (l1 : 'a list) (l2 : 'a list) (k : 'a list -> 'r) : 'r =
  match l1 with
  | [] -> k l2
  | x :: xs ->
     append_k xs l2 (fun ys -> k (x :: ys))
