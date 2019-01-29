let x = 3 in x + 2;;

let x = 3 in
  let y = x + 1 in
  let x = 2 in
  x + y;;

let x = 3 in
    let f = fun u -> x + u in
    let x = 2 in
    f x;;

let x = 3 in
    let f u = x + u in
    let x = 2 in
    f x;;

let x = 1 in
    let y = x in
    let x = 2 in
    let f = fun n -> n + x in
    let x = 3 in
    f (x + y);;
