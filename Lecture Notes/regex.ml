type regex =
  | Epsilon
  | Empty
  | Single of char
  | Cat of regex * regex
  | Alt of regex * regex
  | Star of regex

let rec accept (cs : char list) (r : regex) (sc : char list -> 'r) (fc : unit -> 'r) : 'r =
  match r with
  | Epsilon -> sc cs
  | Empty -> fc ()
  | Single c' ->
     begin
       match cs with
       | [] -> fc ()
       | c :: cs ->
          if c = c'
          then sc cs
          else fc ()
     end
  | Cat (r1, r2) ->
     accept cs r1
       (fun cs -> accept cs r2 sc fc)
       fc
  | Alt (r1, r2) ->
     accept cs r1
       sc
       (fun () -> accept cs r2 sc fc)
  | Star r ->
     accept cs r
       (fun cs -> accept cs (Star r) sc fc)
       (fun () -> sc cs)

let accept' (cs : char list) (r : regex) : bool =
  accept cs r (fun cs -> cs = []) (fun () -> false)

let explode (s : string) : char list =
  List.init (String.length s) (String.get s)

let literally (s : string) : regex =
  let rec go = function
    | [] -> Epsilon
    | c :: cs -> Cat (Single c, go cs)
  in
  go (explode s)

let accept'' (s : string) (r : regex) : bool =
  accept' (explode s) r

let r1 = Single 'a'

let r2 = Cat (literally "ba", Star (literally "na"))
