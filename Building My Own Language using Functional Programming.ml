(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), ["x"]);
  (Apply (Var "some",[I 3;I 5;I 8]), []); 
  (Fn ([("one",Int);("two",Int)],I 1),["one";"two"]);
  (Fn ([],I 1),[]);
  (Rec ("recFunc", Arrow ([Bool;Int],Bool) ,B false),["recFunc"]);
  (Rec ("recFunc", Arrow ([Bool;Int],Bool) ,Var "recFunc"),[]);
  (Rec ("R", Arrow ([Bool;Bool],Bool) ,Var "R"),[]);
  
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused

  | Rec (x, _, e) -> if (List.mem x (free_variables e)) then (unused_vars e) else [x]@(unused_vars e) 
        
  | Fn (xs, e) ->(match xs with 
      |[]->unused_vars e
      |_-> List.fold_left (fun acc (name,_) -> 
          if (List.mem name (free_variables e)) 
          then acc 
          else acc@[name]) (unused_vars e) xs) 
                        
  |Apply (e, es) -> (match es with
      |[]-> unused_vars e
      |_-> List.fold_left (fun acc exp ->  acc @ (unused_vars exp)) (unused_vars e) es)
                       

(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1]) ));
  
  
  (((I 2, "x"), 
    Rec ("c", Int , Primop (Plus, [Var "x"; I 1]))), 
   Rec ("c", Int , Primop (Plus, [I 2; I 1])));
  
  (*special case*)
  (((Var "y", "x"), 
    Rec ("x", Int , Primop (Plus, [Var "x"; I 1]))), 
   Rec ("y", Int , Primop (Plus, [Var "y"; I 1])));
  
  
  (*special case --- special--special*)
  (((Var "x", "y"), 
    Rec ("x", Int , Primop (Plus, [Var "y"; I 1]))), 
   Rec ("n", Int , Primop (Plus, [Var "x"; I 1])));
  
  
  (((Var "x", "y"), 
    Fn ([("x",Int)],Var "y")), 
   Fn ([("n",Int);],Var "x"));

  
  (((I 1, "c"), 
    Fn ([("one",Int);("two",Int)],Var "c")), 
   Fn ([("one",Int);("two",Int)],I 1));
  
  
  
  (((Var "y", "c"), 
    Fn ([("c",Int);("two",Int)],Var "c")), 
   Fn ([("y",Int);("two",Int)],Var "y"));
  
  (((Var "y", "c"), 
    Fn ([("c",Int);("c",Int)],Var "c")), 
   Fn ([("y",Int);("y",Int)],Var "y"));
  
  
  (((Var "b", "c"), 
    Apply (Var "c",[I 3;I 5;I 8])), 
   Apply  (Var "b",[I 3;I 5;I 8]));
  
  (((Var "b", "c"), 
    Apply ( Fn ([("c",Int)],Var "c"),[I 3])), 
   Apply  ( Fn ([("b",Int)],Var "b"),[I 3]));
  
  
  
  (*Final Test Case Special Case *)
  (((I 7, "y"), 
    Apply(I 9, [B false; Var "y"])), 
   Apply(I 9, [B false; I 7]));
  
  
    
  
  
  
  
  
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if y=x then  Rec (y, t, e) else (
        let(y,e)=if List.mem y (free_variables e') 
          then rename y e 
          else (y,e) in Rec (y, t, (subst (e', x) e)))

  | Fn (xs, e) -> 
      if List.mem x (List.map fst xs) then
        exp
      else(
        let (listMajic,exp)=helper e' xs e
        in
        Fn (listMajic,(subst (e', x) exp)))
  
  | Apply (e, es) ->
      let listMajic2 = List.map (fun expression ->  subst (e',x) expression) es
      in
      Apply (subst (e',x) e, listMajic2)
  
and  helper e' xs e = match xs with
  |[]-> ([],e)
  |(name,tp)::tail->
      if List.mem name (free_variables e') 
      then (
        let (n,exprz)= (rename name e)
        in
        let (listNamestp,expression) =  helper e' tail exprz
        in
        (((n,tp)::listNamestp),expression))
      else(
        let (listNamestp,expression)= helper e' tail e
        in
        (((name,tp)::listNamestp),expression))
  
and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs


(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  
  
  (Rec ("x", Int, Primop (Minus, [I 12; I 6])), I 6);
  
  (Rec ("x", Int, Primop (Minus, [I 12; I 6])), I 6);
  
  
  (*last tc added*)
  (Apply (Rec ("m", Int, Fn( [("y",Int)] , I 7)) ,[I 7]), I 7);
  
  
  (*Test Case 4 -- Apply 0 argument function*)
  (Apply (Fn ([], Primop (Minus, [I 6; I 4])), []), I 2);
  
  
    (*Test Case 2  -- single argument function*)
  (Apply (Fn ([("a", Int)], Primop (Negate, [Var "a"])), [I 9]), I (-9));

  
  (*Test Case 3*)
  (Apply (Fn ([("a", Int); ("b", Int)], Primop (Minus, [Var "a"; Var "b"])), [I 9; I 3]), I 6);

  
 
  
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2) 

  | Rec (f, m, e) ->  eval (subst ( Rec (f, m, e) , f)  e) 
  |Apply (e, es) -> (
      match eval e with 
      |Fn(xs,exp)-> 
          let listNames = List.fold_left (fun acc (name,_) -> acc @ [name]) [] xs
          in 
          if (List.length listNames)!=(List.length es) then raise (Stuck Arity_mismatch)
          else(
            let processedList = List.combine es listNames
            in 
            eval (subst_list processedList exp)) 
      |_->raise (Stuck Apply_non_fn)
  
    )
 



(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (([("m",Bool);("d",Int);("p",Int);("strawberryNum",Int)],Fn ([("m",Bool)],B true)),Arrow([Bool],Bool));
  (([("m",Bool);("d",Int);("p",Int)],Fn ([("m",Bool)],B false)),Arrow([Bool],Bool));
  (([("m",Int);("p",Int);("strawberryNum",Int)],Fn ([],I 500)),Arrow([],Int));
  (([("m",Int);("d",Int);("p",Int);("strawberryNum",Int)],Fn ([("m",Int);("n",Int);("o",Int);("p",Int)],B false)), Arrow([Int;Int;Int;Int],Bool));
  (([("x", Int)], Var "x"), Int) ; 
  ((["a",Int;"d",Int;"j",Int],Rec("b",Int,Primop(Times,[I 10;Var "b"]))),Int);
  (([("m",Bool);("d",Int);("p",Int);("strawberryNum",Int)],Apply(Fn([],Var "m"),[])),Bool);
  (([("m",Int);("d",Int);("p",Int);("strawberryNum",Int)],Apply(Fn([("m",Int);("p",Int)],Var "m"),[I 9;I 12])),Int);
  (([("m",Bool);("d",Int);("p",Int);("strawberryNum",Int)],Apply(Fn([("m",Bool)],Var "m"),[B false])),Bool);
  
]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> 
      let new_context = extend ctx (f,t) in
      let expression_type =  infer new_context e 
      in
      if (t = expression_type) then t else raise (TypeError (Type_mismatch (t,expression_type)))

  | Fn (xs, e) -> raise NotImplemented
                    
  |Apply (e, es) -> raise NotImplemented

                    

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)

(* TODO: Implement type unification. *)
let rec unify (t1 : utp) (t2 : utp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | UInt, UInt
  | UBool, UBool -> ()
  (* For type constructors, recursively unify the parts *)
  | UArrow (t1, t1'), UArrow (t2, t2') ->
      raise NotImplemented
  | UTVar a, _ -> unifyVar a t2
  | _, UTVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t =
  raise NotImplemented
