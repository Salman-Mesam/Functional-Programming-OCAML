(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  let count= ref 0 in
  let currBalance= ref 0 in 
  let passkey= ref p in
  let checkPassValidity passwd = match passwd with
    |value->if value= !passkey then true else false 
          
  in 
  let checkBalanceSufficiency amt = match amt with
    |amount-> if (amount > !currBalance) then false else true
  in
  {
    update_passwd = (fun oldP newP-> if (checkPassValidity oldP) then
                        (count:=0;passkey:=newP)
                      else (count := !count+1;raise wrong_pass)
                    );
    retrieve = (fun passw amount->
        if (!count<3) then 
          (if (checkPassValidity passw) then 
             (count:=0;
              if (checkBalanceSufficiency amount) then currBalance := ((!currBalance)-amount)
              else  raise no_money
             )
           else(count:= (!count)+1;raise wrong_pass)) 
        else raise too_many_attempts
          
      );
    
    deposit= (fun passw amount->
        if (!count<3) then 
          (if (checkPassValidity passw) then 
             (count:=0; currBalance := ((!currBalance)+amount) 
             )
           else(count:= (!count)+1;raise wrong_pass)) 
        else raise too_many_attempts
          
      );
    
    print_balance=(fun passw-> 
        if (!count<3) then 
          (if (checkPassValidity passw) then 
             (count:=0; !currBalance )
           else(count:= (!count)+1;raise wrong_pass)) 
          
        else raise too_many_attempts
          
      );

  }

(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  let count = ref 0 in 
  let rec fib n = if n = 0 then ( count:=!count+1 ; 0)
    else (if n = 1 then (count:=!count+1 ; 1) else (count:=!count+1 ; fib (n-2) + fib (n-1); ))
  in
  {
    num_rec=(!count);
    result=(fib n);
  }
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  let rec fib n =
    match Hashtbl.find_opt store n with
    |Some value->value
    |None-> 
        let value= if n = 0 then 0
          else (if n = 1 then 1 else fib (n-2) + fib (n-1)) 
        in
        Hashtbl.add store n value ; value
  in
  fib n
;;

(* Q 2.3 : General memoization function *)

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  let hash = Hashtbl.create 1000 in
  let rec g x = match Hashtbl.find_opt hash x with
    |Some value -> stats.lkp := !(stats.lkp)+1; value
    |None-> let value = f g x in
        Hashtbl.add hash x value;stats.entries := !(stats.entries)+1;value
  in 
  g
;;

(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM = 
  let statistics = {
    entries =  ref 0;
    lkp = ref 0
  }
  in 
  let rec fib_Special = (fun f n -> if n = 0 then 0
                          else (if n = 1 then 1 else f (n-2) + f (n-1)))
  in
  
  let newfunction = (memo fib_Special statistics)
  in
  
  fun n-> ((newfunction n),statistics)
;;
