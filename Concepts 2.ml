

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([],[]);
  ([A;T;T;C;G;G;G;G],[(1,A);(2,T);(1,C);(4,G)]);
  ([A],[(1,A)]);
  ([A;T;C;G],[(1,A);(1,T);(1,C);(1,G)]);
  ([A;T],[(1,A);(1,T)]);
  ([A;A],[(2,A)])
]


  
(* TODO: Implement compress. *) 
let compress (l : nucleobase list) : (int * nucleobase) list = match l with 
  |[]->[] 
  |base::tail->
      let rec count (l : nucleobase list)(base:nucleobase)(counter : int) (listTup :(int * nucleobase) list)
        :  (int * nucleobase) list = 
        match l with 
        |[]->(listTup@[(counter,base)]) 
        |nucleobase::tail -> if nucleobase=base then count tail nucleobase (counter+1) listTup
            else count tail nucleobase 1 (listTup@[(counter,base)]) 
      in count tail base 1 []
                  

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ([],[]);
  ([(1,A);(2,T);(1,C);(4,G)],[A;T;T;C;G;G;G;G]);
  ([(1,A)],[A]);
  ([(1,A);(1,T);(1,C);(1,G)],[A;T;C;G]);
  ([(1,A);(1,T)],[A;T]);
  ([(2,A)],[A;A])
]



(* TODO: Implement decompress. *)

let rec decompress (l : (int * nucleobase) list) : nucleobase list  = 
  let lb = [] in
  match l with
  |[]->[] 
  |(frequency,base)::tail-> let rec helper (list: nucleobase list)(base:nucleobase) (counter:int): nucleobase list = 
                              match counter with 
                              |0->[]
                              |_->list@base :: helper list base (counter-1)
      in 
      helper lb base frequency @ decompress(tail)
                             

     
  


(* Q2 *)
(* TODO: Write a good set of tests for eval *)

let pi = 4.0 *. atan 1.0;;
let eval_tests = [
  (PLUS(FLOAT 5.0, FLOAT 5.0),  10.0);
  (MINUS (FLOAT 10.0, FLOAT 5.0),  5.0);
  (DIV(FLOAT 10.0,FLOAT 5.0),  2.0); 
  (SIN(FLOAT pi), 0.0);
  (COS(FLOAT pi), (-1.0));
  (EXP(FLOAT 1.0), 2.71828182846);
  (MULT(PLUS( FLOAT 5.0, FLOAT 5.0), FLOAT 5.0), 50.0);
  (MULT(PLUS( FLOAT 5.0, FLOAT 5.0), PLUS( FLOAT 5.0, FLOAT 5.0)), 100.0);
  (FLOAT(3.0),3.0)
  
  
  

]

(* TODO: Implement eval. *)
let rec eval e : float = 
  match e with
  |FLOAT(v1)->v1
  |PLUS(e1,e2)-> (eval e1) +. (eval e2)
  |MINUS(e1,e2)-> (eval e1) -. (eval e2)
  |MULT(e1,e2)->(eval e1) *. (eval e2)
  |DIV(e1,e2)-> (eval e1) /. (eval e2) 
  |SIN(e1)->sin (eval e1)
  |COS(e1)->cos (eval e1)
  |EXP(e1)-> exp (eval e1)
  
                     
                                
    

let pi = 4.0 *. atan 1.0;;
    (* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (PLUS(FLOAT 5.0, FLOAT 5.0),[Float 5.0;Float 5.0; Plus]) ;
  (FLOAT 5.0,[Float 5.0]);
  (SIN(FLOAT pi), [Float  pi; Sin]);
  (COS(FLOAT pi), [Float  pi; Cos]);
  (EXP(FLOAT 1.0), [Float  1.0; Exp])
  
  
  
]



(* TODO: Implement to_instr. *)
let rec to_instr e =
  let list : instruction list  = [] 
  in
  match e with 
  |FLOAT v1 ->list@[Float v1]
  |PLUS(e1,e2)->list@(to_instr e1)@(to_instr e2)@[Plus]
  |MINUS(e1,e2)->list@(to_instr e1)@(to_instr e2)@[Minus]
  |MULT(e1,e2)->list@(to_instr e1)@(to_instr e2)@[Mult]
  |DIV(e1,e2)->list@(to_instr e1)@(to_instr e2)@[Div]
  |SIN(e1)->list@(to_instr e1)@[Sin]
  |COS(e1)->list@(to_instr e1)@[Cos]
  |EXP(e1)->list@(to_instr e1)@[Exp]







(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Plus,[5.0;5.0;3.0]),Some [10.0;3.0]); 
  ((Mult,[5.0;5.0;3.0]),Some [25.0;3.0]);
  ((Div,[5.0;10.0;3.0]),Some [2.0;3.0]); 
  ((Div,[10.0;30.0;3.0]),Some [3.0;3.0]); 
  ((Minus,[1.0;3.0]),Some [2.0]);
  ((Minus,[5.0;3.0]),Some [-2.0]);
  ((Plus,[1.0;3.0]),Some [4.0]);
  ((Mult,[1.0;3.0]),Some [3.0]);
  ((Div,[1.0;3.0]),Some [3.0]); 
  ((Plus,[]),None);
  ((Minus,[]),None);
  ((Mult,[]),None);
  ((Div,[]),None);
  ((Plus,[1.0]),None); 
  ((Sin,[pi;pi/.2.]),Some [0.;pi/.2.]);
  ((Cos,[pi;pi/.2.]),Some [-1.0;pi/.2.]);
  ((Cos,[]),None); 
  ((Float 4.2,[]),Some[4.2]);
  ((Float 4.2,[3.2]),Some[4.2;3.2])
  
]



(* TODO: Implement to_instr. *)               
let instr i s = 
  match s,i with 
  | _ , Float v -> Some (v :: s) 
  |[],_ -> None 
  |head::tail , Sin -> Some (sin(head)::tail)
  |head::tail , Exp -> Some (exp(head)::tail)
  |head::tail , Cos -> Some (cos(head)::tail)
  |head::head2::tail , Plus->Some ((head2+.head)::tail)
  |head::head2::tail , Minus->Some ((head2-.head)::tail)
  |head::head2::tail , Mult ->Some ((head2*.head)::tail)
  |head::head2::tail, Div ->Some ((head2/.head::tail)) 
  |head::[],_->None


(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.0 ; Float 3.0 ; Plus; Float 5.0; Mult],Some(25.0)) ;
  ([Float pi ; Sin ; Float 2.0 ; Plus],Some(2.0));
  ([],None);
  ([Float 5.0; Float 3.0 ; Minus ; Float 2.0 ; Plus ],Some(4.));
  ([Float 3.0; Float 5.0 ; Minus ; Float 2.0 ; Plus ],Some(0.0));
  ([Float 2.2; Float 3.3 ; Plus ; Float 5.0 ; Mult ],Some(27.5));
  
  
  
]


(* TODO: Implement prog. *)
let prog instrs = 
  
  let rec helper (l:instruction list) (stack:float list) = match l with
    |[]->Some(stack)
    |Float v::tail->helper tail (v::stack) 
    |head::tail->let ans = instr head stack in 
        match ans with 
        |Some a-> helper tail a
        |None-> None
  in match helper instrs [] with
  |None->None
  |Some a-> match a with 
    |head::[]->Some head
    |_->None

    


