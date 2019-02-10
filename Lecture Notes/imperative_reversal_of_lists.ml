type cell = { data : int; next : rlist}
and rlist = cell option ref;;

let c1 = {data = 1; next = ref None};;
let c2 = {data = 2; next = ref (Some c1)};;
let c3 = {data = 3; next = ref (Some c2)};;
let c5 = {data = 5; next = ref (Some c3)};;

(* This converts an rlist to an ordinary list. *)
let rec displayList (c : rlist) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l);;

let cell2rlist (c:cell):rlist = ref (Some c);;

let reverse (lst: rlist) =
  let rec helper ((l: rlist), (acc: rlist)) =
    match !l with 
      | None -> acc
      | Some c when !(c.next) = None -> (c.next := !acc; acc := (Some c); acc)
      | Some c ->
         (l := !(c.next); c.next := !acc; acc := (Some c); helper(l,acc))
  in
  (helper(lst, ref None));;
