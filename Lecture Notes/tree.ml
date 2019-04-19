type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let rec find_k (p : 'a -> bool) (t : 'a tree) (sc : 'a -> 'r) (fc : unit -> 'r) : 'r =
  match t with
  | Empty -> fc ()
  | Node (l, x, r) ->
     if p x
     then sc x
     else find_k p l sc (fun () -> find_k p r sc fc)
