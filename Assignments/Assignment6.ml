(* In class, we have shown you a program which mimics transactions done on a bank account. 
   We will now develop an extended version of this example. For this we have first defined a data-type for transactions:

   type transaction = Withdraw of int | Deposit of int | CheckBalance | ChangePassword of string | Close.

   In class, we defined a function make-account which generates a bank account when given an opening balance.
   In this exercise, you are asked to modify this code and generate a password-protected bank account. 
   Any transaction on the bank account should only be possible, if one provides the right password. 
   For this, implement the function makeProtectedAccount with the arguments and types shown below.

   val makeProtectedAccount : int * string -> string * transaction -> unit = <fun>
   This function takes in the opening balance as a first argument and the password as a second, 
   and will return a function which when given the correct password and a transaction will perform the transaction. 
   One crucial difference to be noted right away is that in the new code I want you to print the balance on the screen instead of returning it as a value.
*)

(* Examples *)  
  # let zoe = makeProtectedAccount(1000, "BiologyRocks");;
  val zoe : string * transaction -> unit = <fun>
  # let elisa = makeProtectedAccount(500, "ArtsStudentsArePoor");;
  val elisa : string * transaction -> unit = <fun>
  # let alison = makeProtectedAccount(2500, "MathIsTheBest");;
  val alison : string * transaction -> unit = <fun>
  # elisa("ArtsStudentsArePoor", Withdraw 100);;
  The new balance is: 400.
  - : unit = ()
  # alison("MathIsTheBest", Deposit 200);;
  The new balance is: 2700.
  - : unit = ()
  # zoe("BiologyRocks", CheckBalance);;
  The balance is: 1000.
  - : unit = ()
  # zoe("BiologySucks", Withdraw 100);;
  Incorrect password.
  - : unit = ()
  # zoe("BiologyRocks", Withdraw 1500);;
  Insufficient funds.
  - : unit = ()
  # elisa("ArtsStudentsArePoor", ChangePassword "CSwillMakeMeRich");;
  Password changed.
  - : unit = ()
  # elisa("ArtsStudentsArePoor", Deposit 100);;
  Incorrect password.
  - : unit = ()
  # elisa("CSwillMakeMeRich", Deposit 200);;
  The new balance is: 600.
  - : unit = ()
  # alison("MathIsTheBest", Close);;
  Account successfully closed.
  - : unit = ()
  # alison("MathIsTheBest", CheckBalance);;
  Account closed.
  - : unit = ()
  
  
(* Solution to Question 1 *)

let makeProtectedAccount((openingBalance: int), (password: string)) =
    let isOpen = ref true in 
    let balance = ref openingBalance in 
    let passwd = ref password in 
    fun ((passkey:string), (t : transaction)) ->
      if not !isOpen 
      then 
        print_string "Account closed."
      else
        if not (passkey = !passwd) then
          print_string "Incorrect password."
        else
          match t with
          | Withdraw(x) ->
             if (x < !balance)
             then
               (balance := !balance - x;
               Printf.printf "The new balance is: %i." !balance)
            else
              print_string "Insufficient funds."
          | Deposit(x) ->
              balance := !balance + x;
              Printf.printf "The new balance is: %i." !balance
          | CheckBalance ->
              Printf.printf  "The balance is: %i." !balance
          | ChangePassword newPwd -> (passwd := newPwd; print_string "Password changed.")
          | Close -> isOpen := false; print_string "Account successfully closed.";;  
