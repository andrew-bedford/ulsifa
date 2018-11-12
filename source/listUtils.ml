(* Verifies if the element e is present in the list l *)
let is_member(e, l) = 
	List.exists (fun x -> x = e) l

(* Verifies that the element e is not present in the list l *)
let is_not_member(e, l) = 
	List.for_all (fun x -> x <> e) l

(* Verifies if the environment g is empty *)
let is_empty(g) = 
	g = []

(* Returns the union of the lists l1 and l2 *)
let union(l1, l2) = 
  List.fold_right (fun x l -> if (is_member(x, l1)) then l else x::l) l2 l1  
	
(* Returns the diff of the lists l1 and l2 *)    
let diff(l1, l2) =
  List.fold_right (fun x l -> if (is_member(x, l2)) then l else x::l) l1 []