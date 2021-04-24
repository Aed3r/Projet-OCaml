#load "graphics.cma";;
#load "point.cmo";;
open Point;;
#load "list_tri.cmo";;
open List_tri;;
open Graphics;;

(* === Calcul de Pa et Pb === *)

let min_point p1 p2 = p1.abs < p2.abs ;;
let max_point p1 p2 = p1.abs > p2.abs ;;
let points_depart l = ((min_list (min_point) l) , (min_list (max_point) l)) ;;

(* === Tests Calculs de Pa et Pb === *)

let p1 = {abs = 1; ord = 2} ;;
let p2 = {abs = 1; ord = 5} ;;
let p3 = {abs = 5; ord = 3} ;;
let p4 = {abs = 2; ord = 0} ;;
let p5 = {abs = 4; ord = 1} ;;
let p6 = {abs = 4; ord = 4} ;;
let p7 = {abs = 3; ord = 2} ;;
let p8 = {abs = 2; ord = 3} ;;

min_point p1 p3;; (* true *)
min_point p1 p2;; (* false *)
max_point p1 p3;; (* false *)
max_point p1 p2;; (* false *)

points_depart [p2; p1; p3];; (* ({abs = 1; ord = 2}, {abs = 5; ord = 3}) *)
points_depart [p2; p1; p3; p4];; (* ({abs = 1; ord = 5}, {abs = 5; ord = 3}) *)

(* === Ensemble de points a droite d'une droite orientee === *)

let rec points_droite l pa pb =
    match l with
    [] -> []
    | x::r -> if ((pb.abs - pa.abs) * (x.ord - pa.ord) - (pb.ord - pa.ord) * (x.abs - pa.abs)) < 0
              then x::(points_droite r pa pb)
              else points_droite r pa pb ;;

(* === Test ensemble de points a droite d'une droite orientee === *)

points_droite [p4; p2] p1 p3;; (* [{abs = 2; ord = 0}] *)
points_droite [p4; p2] p3 p1;; (* [{abs = 1; ord = 5}] *)

(* === Point le plus eloigne d’une droite === *)

let equation_droite p1 p2 =
	let a = p2.ord - p1.ord in
		let b = -(p2.abs - p1.abs) in
			let c = -(a * p1.abs) - (b * p1.ord) in
				(a, b, c) ;;

let distance_droite a b c p = (float_of_int (abs(a * p.abs + b * p.ord + c))) /. (sqrt (float_of_int ((a*a) + (b*b))));;

let distance_max p1 p2 pa pb = 
	let (a, b, c) = equation_droite p1 p2 in
	(distance_droite a b c pa) > (distance_droite a b c pb)

let rec point_eloigne p1 p2 l = min_list (distance_max p1 p2) l;;
	
(* === Test Point le plus eloigne d’une droite === *)

equation_droite p1 p3;; (* (1, -4, 7) *)
equation_droite p5 p8;; (* (2, 2, -10) *)

distance_droite 1 (-4) 7 p5;; (* 1.69774937525433089 *)
distance_droite 2 2 (-10) p1;; (* 1.41421356237309492 *)

distance_max p1 p2 p5 p6;; (* false *)
distance_max p5 p8 p6 p2;; (* true *)

point_eloigne p1 p3  [p2; p4; p5; p6];; (* {abs = 1; ord = 5} *)
point_eloigne p1 p3  [p2; p4; p5; p6];; (* {abs = 1; ord = 5} *)

(* === Ajouter un point dans une liste a sa place === *)

let points_egaux p1 p2 = p1 = p2;;

let rec ajoute_list_apres p1 p l =
	match l with 
	[] -> []
	|x::r -> if x = p1 
			then x::p::(ajoute_list_apres p1 p r)
			else x::(ajoute_list_apres p1 p r);;

(* === Tests points egaux === *)

points_egaux p1 p2;; (* false *)
points_egaux p5 {abs = 4; ord = 1};; (* true *)

ajoute_list_apres p2 p3 [p1; p2; p4; p5; p6];; (* point.point list = [{abs =1; ord =2}; {abs = 1; ord = 5}; {abs = 5; ord = 3}; {abs = 2; ord = 0}; {abs = 4; ord = 1}; {abs = 4; ord = 4}] *)
ajoute_list_apres p7 p8 [p5; p6; p7];; (* [{abs = 4; ord = 1}; {abs = 4; ord = 4}; {abs = 3; ord = 2}; {abs = 2; ord = 3}] *)

(* === Fonctions principale === *)

let rec findhull l p q enveloppe =
	match l with
	[] -> enveloppe
	| x::r -> let c = point_eloigne p q l in
				let enveloppe = ajoute_list_apres p c enveloppe in
					let enveloppe = findhull (points_droite l p c) p c enveloppe in
						findhull (points_droite l c q) c q enveloppe;;

let quickhull l =
	let l = suppr_doublons l in
		let (pa, pb) = points_depart l in
			let enveloppe = pa::(pb::[]) in
				let enveloppe = findhull (points_droite l pa pb) pa pb enveloppe in
					findhull (points_droite l pb pa) pb pa enveloppe;;

(* === Test fonctions principale === *)

quickhull [p1; p2; p3; p4; p5; p6; p7; p8];; (* [{abs = 1; ord = 2}; {abs = 2; ord = 0}; {abs = 4; ord = 1};
												 {abs = 5; ord = 3}; {abs = 4; ord = 4}; {abs = 1; ord = 5}] *) 

(* === Fonction Utilitaire === *)

let enveloppe_convexe g n =
	let l = g n in
		(vider ();
		set_color red;
		tracer_nuage l ;
		set_color blue;
		tracer_polygone(quickhull l));;

(* === Demo === *)

initialiser()
(*  
enveloppe_convexe gen_cercle 1000;; 
enveloppe_convexe gen_papillon 1000;; 
enveloppe_convexe gen_soleil 1000;;
*)