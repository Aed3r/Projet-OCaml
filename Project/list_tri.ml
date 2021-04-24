let rec partitionne_bis l l1 l2 =            (* ~2.6 ms pour 100000 element, ne preserve pas l'ordre *)
	match l with
	[] -> (l1, l2)
	| [x] -> (x::l1, l2)
	| x::y::r -> partitionne_bis r (x::l1) (y::l2)
and partitionne l = partitionne_bis l [] [] ;;  

let rec fusionne comp l1 l2 =
    match l1, l2 with 
    [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | x1::r1, x2::r2 -> if comp x1 x2
        then x1::(fusionne comp r1 l2)
        else x2::(fusionne comp l1 r2)

let rec tri comp l =
    match l with
    [] -> []
    | [x] -> [x]
    | [x;y] -> fusionne comp [x] [y] (* Optionel, ameliore la vitesse *)
    | x::r -> let l1, l2 = 
        partitionne l in
            fusionne comp (tri comp l1) (tri comp l2);;

let rec min_list comp l =
    match l with
	[] -> failwith "liste vide"
	|[x] -> x
	| x::a::r -> if comp a x then min_list comp (a::r)
							else min_list comp (x::r) ;;

let rec suppr_doublons l =
	match l with
	[] -> []
	|x::r -> if (List.exists (function y -> if x = y then true else false) r) 
			 then suppr_doublons r
			 else x::(suppr_doublons r) ;;