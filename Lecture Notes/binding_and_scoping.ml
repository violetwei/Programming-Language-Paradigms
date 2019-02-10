let result = 
  let x = 3 in
  let f = 
    let x = x + 1 in
    fun y -> x + y in
  f x;;

let result =
  let  x = 1 in
  let  y = 2 in
  let  x = 3 in
  let  f = fun u ->  u + x in
  let  y = 4 in
  f y;;
