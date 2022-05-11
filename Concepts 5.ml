(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  (({nodes = ["lahore";"karachi"];edges = [("lahore","karachi",5)]},"lahore"),[("karachi" , 5)]);
  (({nodes = [];edges = []},"lahore"),[]);
  (({nodes = ["lahore";"karachi";"islamabad"];edges = [("lahore","karachi",5);("murree","islamabad",6)]},"lahore"),[("karachi" , 5)])

]

(* TODO: Implement neighbours. *)
 (* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  (({nodes = ["lahore";"karachi"];edges = [("lahore","karachi",5)]},"lahore"),[("karachi" , 5)]);
  (({nodes = [];edges = []},"lahore"),[]);
  (({nodes = ["lahore";"karachi";"islamabad"];edges = [("lahore","karachi",5);("murree","islamabad",6)]},"lahore"),[("karachi" , 5)])

]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  let listToreturn = [] 
  in 
  List.fold_left (fun l (a,b,w) -> if vertex=a then l@[(b,w)] else l ) listToreturn g.edges
(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) = 
  let  is_SelfLoop (city: 'a) (visited : 'a list) : bool = 
    List.exists (fun y -> y = city) visited
  in 
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) = match node with
    |(city,distance)-> if (is_SelfLoop city visited)==false then
          if city=b then ([city],distance) else (
            let (list,weight) = aux_list (neighbours g city) (city::visited)
            in
            city::list, weight+distance) 
        else raise Fail
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) = match nodes with
    |[]->raise Fail
    |head::tail->try aux_node head visited with Fail -> aux_list tail visited
  in
  aux_node (a,0) []
    
  
let find_path' (g: 'a graph) (a: 'a) (b: 'a)  : ('a list * weight) = 
  let  is_SelfLoop (city: 'a) (visited : 'a list) : bool = 
    List.exists (fun y -> y = city) visited
  in 
  let rec aux_node (node: 'a * weight) (visited : 'a list) s f : ('a list * weight) = match node with
    |(city,distance)-> if (is_SelfLoop city visited)==false then
          if city=b then s ([city],distance) else  aux_list (neighbours g city) (city::visited) (fun(list, weight)-> s (city::list, weight+distance)) f 
        else f()
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) s f : ('a list * weight) = match nodes with
    |[]->f ()
    |head::tail->aux_node head visited s (fun()-> aux_list tail visited s f)
  in
  aux_node (a,0) [] (fun x -> x ) (fun() -> raise Fail)

    (* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let  is_SelfLoop (city: 'a) (visited : 'a list) : bool = 
    List.exists (fun y -> y = city) visited
  in 
  let rec aux_node (starting_node : 'a * weight) (visited : 'a list) : ('a list * weight) list = match starting_node with
    |(city,distance)-> if (is_SelfLoop city visited)==false then
          if city=b then [([city],distance)] else ( 
            let listOfTuples = aux_list (neighbours g city) (city::visited)
            in
            List.map (fun (list,weight) -> (city::list, weight+distance)) listOfTuples
          ) 
        else raise Fail
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list = match nodes with
    |[]->[]
    |head::tail->try let listOfTuples = aux_node head visited in
          listOfTuples @ aux_list tail visited with Fail -> aux_list tail visited
  in
  aux_node (a,0) []


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
  let allPaths = find_all_paths g a b 
  in
  let rec helper (pathsDetails :('a list * weight) list)  : ('a list * weight) option = match pathsDetails with 
    | (path1,w1)::(path2,w2)::tail ->
        if w1 > w2 then helper ((path2,w2)::tail) else helper ((path1,w1)::tail)
    |[]->None 
    | [(onlyPath,w)] -> Some (onlyPath,w)
  in
  helper allPaths

(* ---------- Hamming Numbers ----------- *)

let rec merge s1 s2 = {
  hd = if s1.hd < s2.hd then s1.hd else if s1.hd > s2.hd then s2.hd else s1.hd ;
  tl = Susp (fun () -> if s1.hd < s2.hd then merge (force s1.tl) s2 else if s1.hd >s2.hd then merge  s1 (force s2.tl) else merge  (force s1.tl) (force s2.tl))
  
}
  

let rec hamming_series =
  ()