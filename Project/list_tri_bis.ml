#load "hasard.cmo";;
open Hasard;;
init_random();;

(* === Tri Par Partition-Fusion === *)

let rec partitionne_bis l l1 l2 =            (* ~2.6 ms pour 100000 element, ne preserve pas l'ordre *)
	match l with
	[] -> (l1, l2)
	| [x] -> (x::l1, l2)
	| x::y::r -> partitionne_bis r (x::l1) (y::l2)
and partitionne l = partitionne_bis l [] [] ;;  

(* let rec partitionne l =          ~3.9 ms pour 100000 element, preserve l'ordre
	match l with
	| [] -> ([],[])
	| [_] -> (l,[])
	| x::y::r -> let l1, l2 = 
        partitionne r in 
            x::l1, y::l2 *) 

let rec fusionne comp l1 l2 =
    match l1, l2 with 
    [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | x1::r1, x2::r2 -> if comp x1 x2
        then x1::(fusionne comp r1 l2)
        else x2::(fusionne comp l1 r2)

let rec tri_partition_fusion comp l =
    match l with
    [] -> []
    | [x] -> [x]
    | x::r -> let l1, l2 = 
        partitionne l in
            fusionne comp (tri_partition_fusion comp l1) (tri_partition_fusion comp l2);;

(* === Tests Partition-Fusion === *)

partitionne [1];; (* ([1], []) *)
partitionne [1;2];; (* ([1], [2]) *) 
partitionne [1;2;3;4;5];; (* ([5; 3; 1], [4; 2]) *)

fusionne (<=) [1;3;5;7;9] [2;4;6;8];; (* [1; 2; 3; 4; 5; 6; 7; 8; 9] *)
fusionne (>=) [99;55;33;11] [666;222;44;0];; (* [666; 222; 99; 55; 44; 33; 11; 0] *)
fusionne (>=) [99;55;33;11;-9;-22;-55] [666;222;44;0;-44;-222];; (* [666; 222; 99; 55; 44; 33; 11; 0; -9; -22; -44; -55; -222] *)

tri_partition_fusion (<=) [55;33;99;77;12;0;48;96;3;2];; (* [0; 2; 3; 12; 33; 48; 55; 77; 96; 99] *)
tri_partition_fusion (>=) (random_list 100 10);;(* [85; 79; 58; 58; 49; 39; 22; 17; 11; 6] *)
tri_partition_fusion (>=) (random_list 10000 100);; (* [9937; 9892; 9665; 9663; 9579; 9565; 9548; 9523; 9250; 9083; 8927; 8904;
                                                        8834; 8772; 8570; 8557; 8546; 8515; 8418; 8392; 8138; 8069; 8050; 7940;
                                                        7910; 7894; 7809; 7598; 7578; 7433; 7241; 7120; 7085; 7071; 7054; 7042;
                                                        7014; 7001; 6992; 6778; 6722; 6721; 6646; 6553; 6515; 6290; 6192; 6177;
                                                        6149; 5875; 5818; 5581; 5560; 5515; 5484; 5415; 5396; 5257; 5054; 4818;
                                                        4811; 4673; 4454; 4364; 4131; 3987; 3883; 3873; 3782; 3659; 3608; 3589;
                                                        3586; 3498; 3474; 3414; 3133; 2995; 2812; 2696; 2603; 2493; 2379; 2044;
                                                        1841; 1839; 1808; 1716; 1619; 1489; 1373; 1344; 809; 738; 336; 302; 259;
                                                        115; 113; 106] *)
tri_partition_fusion (>=) (random_list 100 50);; (* [97; 97; 96; 94; 90; 88; 85; 81; 78; 78; 76; 74; 74; 70; 68; 62; 58; 58; 55;
                                                     54; 46; 42; 42; 40; 36; 35; 34; 31; 29; 28; 27; 27; 24; 20; 19; 19; 17; 17;
                                                     16; 14; 13; 12; 11; 10; 9; 6; 5; 3; 3; 0] *)
tri_partition_fusion (>=) (random_list 10 50);; (* [9; 9; 9; 9; 9; 9; 8; 8; 8; 8; 8; 8; 7; 7; 7; 7; 7; 6; 5; 5; 5; 5; 5; 5; 4;
                                                    4; 4; 3; 3; 3; 3; 3; 3; 3; 3; 3; 2; 2; 2; 2; 2; 2; 1; 1; 1; 0; 0; 0; 0; 0] *)

(* === Tri A Bulle === *)

let rec tri_bulle_bis comp l =
    match l with
    [] -> []
    | [x] -> [x]
    | x::y::r -> if comp x y
        then x::(tri_bulle_bis comp (y::r))
        else y::(tri_bulle_bis comp (x::r)) ;;

let rec tri_bulle comp l =
    let temp = tri_bulle_bis comp l in
        if temp = l
        then l
        else tri_bulle comp temp ;;

(* === Tests Tri A Bulle === *)

tri_bulle_bis (<=) [9;2;3;7;6;4;1;2;8;0];; (* [2; 3; 7; 6; 4; 1; 2; 8; 0; 9] *)

tri_bulle (>=) (random_list 50 10);; (* [43; 38; 36; 32; 26; 25; 25; 20; 8; 4] *)
tri_bulle (>=) (random_list 1000 100);; (* [994; 990; 965; 942; 925; 925; 922; 909; 894; 893; 869; 863; 859; 857; 850;
                                            846; 844; 810; 807; 788; 779; 741; 719; 695; 692; 674; 672; 672; 667; 652;
                                            628; 615; 605; 598; 594; 593; 588; 582; 573; 557; 546; 540; 530; 527; 504;
                                            488; 481; 481; 467; 461; 460; 458; 450; 448; 435; 418; 396; 396; 382; 380;
                                            377; 374; 372; 372; 356; 339; 338; 326; 323; 307; 294; 279; 269; 267; 247;
                                            239; 232; 213; 204; 190; 179; 151; 148; 139; 136; 128; 119; 113; 109; 106;
                                            106; 71; 62; 54; 54; 13; 10; 2; 2; 0] *)
tri_bulle (>=) (random_list 100 50);; (* [99; 96; 95; 92; 91; 88; 88; 86; 86; 83; 82; 80; 77; 74; 73; 72; 71; 69; 67;
                                          67; 58; 58; 57; 55; 54; 52; 52; 46; 46; 39; 37; 36; 36; 34; 33; 25; 25; 24;
                                          20; 20; 19; 16; 14; 12; 11; 11; 6; 5; 3; 1] *)
tri_bulle (>=) (random_list 10 50);; (* [9; 9; 9; 9; 9; 9; 8; 8; 8; 8; 8; 7; 7; 7; 6; 6; 6; 6; 6; 5; 5; 5; 5; 5; 5;
                                         5; 5; 5; 5; 4; 4; 4; 4; 4; 4; 3; 3; 2; 2; 2; 2; 2; 1; 1; 1; 1; 0; 0; 0; 0] *)

(* === tri pivot === *)

let rec partitionne_pivot_bis comp l pivot l1 l2 =
    match l with 
    [] -> (l1, l2)
    |x::y -> if comp x pivot  then  partitionne_pivot_bis comp y pivot (x::l1) l2 
                else  partitionne_pivot_bis comp y pivot l1 (x::l2) ;;

let partitionne_pivot comp l pivot = 
                partitionne_pivot_bis comp l pivot [] [];;

let rec tri_pivot comp l =
    match l with 
    [] -> []
    | [x] -> [x]
    | [x;y] -> if comp x y
        then [x;y]
        else [y;x]
    | x::r -> let (l1,l2) = partitionne_pivot comp r x in (tri_pivot comp l1) @ [x] @ (tri_pivot comp l2);;

(* === Tests Tri Pivot === *)

partitionne_pivot (>=) [8; 0; 3; 1; 7; 34; 87; 11; 4; 199] 8;; (*  ([199; 11; 87; 34; 8], [4; 7; 1; 3; 0]) *)

tri_pivot (<=) (random_list 50 10);; (* [0; 13; 16; 29; 29; 35; 37; 37; 41; 45] *)
tri_pivot (<=) (random_list 1000 100);; (* [15; 33; 46; 48; 53; 63; 81; 85; 108; 130; 144; 159; 162; 177; 223; 228; 228;
                                            238; 254; 258; 262; 264; 265; 274; 278; 307; 307; 319; 323; 328; 336; 337;
                                            337; 339; 350; 352; 435; 449; 466; 471; 472; 474; 476; 484; 505; 510; 531;
                                            535; 565; 571; 580; 580; 582; 600; 601; 619; 620; 621; 630; 631; 634; 637;
                                            638; 639; 644; 662; 669; 675; 684; 694; 694; 705; 711; 727; 727; 740; 741;
                                            759; 794; 817; 835; 843; 874; 876; 887; 888; 913; 919; 927; 934; 936; 939;
                                            955; 963; 972; 973; 987; 987; 996; 998] *)
tri_pivot (<=) (random_list 100 50);; (* [0; 2; 3; 4; 4; 5; 6; 10; 10; 12; 13; 14; 15; 17; 19; 20; 23; 27; 33; 34; 37;
                                          40; 40; 41; 49; 52; 52; 56; 57; 60; 61; 63; 63; 64; 67; 67; 73; 75; 76; 80;
                                          82; 86; 87; 87; 88; 89; 91; 94; 95; 99] *)
tri_pivot (<=) (random_list 10 50);; (* [0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 2; 2; 2; 3; 3; 4; 4; 4; 4; 4; 4; 4; 5; 5; 5;
                                         6; 6; 6; 6; 6; 6; 6; 7; 7; 7; 7; 7; 8; 8; 8; 8; 8; 9; 9; 9; 9; 9; 9; 9; 9] *)

(* === Choix d'une fonction de tri === *)

        (* Listes de tests variee *)
let longue_liste = random_list 100000 10000;;
let longue_doublon = random_list 100 10000;;
let courte_liste = random_list 50 50;;
let courte_doublon = random_list 10 50;;
let rec pgq_bis x = 
    if x = 0
    then []
    else x::(pgq_bis (x-1));;
let liste_pgq = pgq_bis 100;;
let rec ppq_bis x l = 
    if x = l
    then []
    else x::(ppq_bis (x+1) l    );;
let liste_ppq = ppq_bis 1 101;;

        (* Calcul du temps moyen de completion d'un algorithme f sur une liste l avec un comparateur comp sur n iteration *)
let rec avg_time_bis l f n i sum=
    if i = n
    then sum /. (float_of_int n)
    else avg_time_bis l f n (i+1) (sum +. 
        (let temps_debut = Sys.time () in
            let _ = f l in
                let temps_fin = Sys.time () in
                    (temps_fin -. temps_debut))) ;;
let avg_time l f n = avg_time_bis l f n 0 0.;;

        (* Test de toute les fonctions sur toutes les listes *)
let rec test_all_functions l f =
    match f with
    [] -> []
    | x::r -> (avg_time l x 1000)::(test_all_functions l r);;

let rec test_all_lists l f =
    match l with
    [] -> []
    | x::r -> (test_all_functions x f)::(test_all_lists r f);;

(* test_all_lists [longue_liste; longue_doublon; courte_liste; courte_doublon; liste_pgq] [tri_partition_fusion (>); tri_pivot (>)];; *)
(*[[0.00739357499999996302; 0.00677259499999994553]; longue_liste
   [0.00689521500000007167; 0.0176476329999999577]; longue_doublon
   [1.13559999999637277e-05; 9.5380000000062638e-06]; courte_liste
   [1.13979999999891393e-05; 1.14619999999661099e-05]; courte_doublon
   [2.37669999999461368e-05; 0.00015372999999997816] liste_pgq
  ] *)

(*avg_time courte_liste (tri_bulle (>)) 1000;; 6.39250000000117724e-05      // Tests a part pour eviter un stackoverflow
  avg_time courte_doublon (>) (tri_bulle (>)) 1000;; 5.79960000001023e-05
  avg_time liste_pgq (>) (tri_bulle (>)) 1000;; 4.55399999989936056e-06 
  avg_time longue_liste (>) (tri_bulle (>)) 1000;; 4.56299959999998883
  avg_time longue_doublon (>) (tri_bulle (>)) 1000;; 4.50347360000003 *)

(* === Fonctions Utilitaires === *)

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

let tri = tri_partition_fusion;;

(* === Tests fonctions utilitaires === *)

min_list (<) [28;6;1;-8;22;33];; (* -8 *)
suppr_doublons [1;2;3;2;4;3;5;1];; (* [2; 4; 3; 5; 1] *)

(* === Demo tris === 

courte_liste;;

tri_partition_fusion (<=) courte_liste;;
tri_pivot (<=) courte_liste;;
tri_bulle (<=) courte_liste;;

*)