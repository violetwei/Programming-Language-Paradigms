(* Imperative Programming *)
let x = ref 1;;
!x;;
x := !x + 1;;
!x;;

let update v = (v := !v + 1);;
update x;;
!x;;

let ar = Array.make 5 0;;

Array.set ar 2 1729;;

let ar2 = Array.make 10 " ";;

Array.set ar2 0 "The";;
Array.set ar2 1 "quick";;
Array.set ar2 2 "brown";;
Array.set ar2 3 "fox";;
Array.set ar2 4 "jumps";;
Array.set ar2 5 "over";;
Array.set ar2 6 "the";;
Array.set ar2 7 "lazy";;
Array.set ar2 8 "dog.";;

for i = 0 to ((Array.length ar2) - 1) do
  (print_string (Array.get ar2 i); print_string " ")
done;;
