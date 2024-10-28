#load "graphics.cma";;
open Graphics;;

let test = [[0;2]; [4;2]; [2;5]; [5;7]; [5;0]; [7;0]];;

let ind_to_pos k n =
	let tau = 6.2832 in
	let a = (tau *. (float_of_int k)) /. (float_of_int n) in
	(int_of_float (250. *. (cos a)) + 300, int_of_float (250. *. (sin a)) + 300)
;;

let draw_poly n =
	open_graph "600x600";
	set_window_title ("Polygône à "^(string_of_int n)^" côtés");
	set_color black; fill_rect 0 0 600 600;
	set_color blue; set_line_width 2; moveto 550 300;
	for k = 1 to n do
		let x,y = ind_to_pos k n in
		lineto x y;
	done;
;;
draw_poly (List.length test + 3);;

let draw_tria ropes =
	set_color red; set_line_width 2;
	let n = (List.length ropes) + 3 in
	let rec sub = function
		| [a; b] :: t ->
			let xa,ya = ind_to_pos a n
			and xb,yb = ind_to_pos b n in
			moveto xa ya; lineto xb yb;
			sub t;
		| _ -> ()
	in sub ropes
;;
draw_tria test;;

type 'a set = V of 'a | S of 'a set list;;

let rec (|?|) e a = match a with
	| V _ -> failwith "type error"
	| S [] -> false
	| S (h :: t) ->
		if h |=| e
		then true
		else e |?| (S t)
and (|<|) a b = match a,b with
	| V x, V y -> x <= y
	| S _, V _ | V _, S _ -> failwith "type error"
	| S [], S _ -> true
	| S (h :: t), S _ ->
		if h |?| b
		then (S t) |<| b
		else false
and (|>|) a b = b |<| a
and (|=|) a b = (a |<| b) && (a |>| b)
;;

let (~~) = function
	| V _ -> 0
	| S u -> List.length u
;;

let (|:|) e a = match a with
	| V _ -> a
	| S u -> 
		if e |?| a
		then a
		else S (e :: u)
;;

let rec (|!|) a e = match a with
	| V _ -> failwith "type error"
	| S [] -> a
	| S (h :: t) ->
		if h |=| e
		then S t
		else h |:| ((S t) |!| e)
;;

let rec (|+|) a b = match a,b with
	| V x, V y -> V (x + y)
	| S _, V _ | V _, S _ -> failwith "type error"
	| S [], S _ -> b
	| S (h :: t), S _ ->
		let c = (S t) |+| b in
		if h |?| b
		then c
		else h |:| c
;;

let rec (|-|) a b = match a,b with
	| V x, V y -> V (x - y)
	| S _, V _ | V _, S _ -> failwith "type error"
	| S [], S _ -> a
	| S (h :: t), S _ ->
		let c = (S t) |-| b in
		if h |?| b
		then c
		else h |:| c
;;

let rec (|*|) a b = match a,b with
	| V x, V y -> V (x * y)
	| S _, V _ | V _, S _ -> failwith "type error"
	| S [], S _ -> a
	| S (h :: t), S _ ->
		let c = (S t) |*| b in
		if h |?| b
		then h |:| c
		else c
;;

let rec parties =
	let rec conc x = function
		| [] -> x
		| h :: t -> conc (h :: x) t
	and tous_avec acc e = function
		| [] -> acc
		| h :: t -> tous_avec ((e :: h) :: acc) e t
	in function
		| [] -> [[]]
		| h :: t ->
			let p = parties t in
			conc p (tous_avec [] h p)
;;

let delta a b = match a,b with
	| S _, S _ -> (a |+| b) |-| (a |*| b)
	| _ -> failwith "type error"
;;
delta (S [V 1; V 2; V 3]) (S [V 2; V 3; V 4]);;

let power_set =
	let rec sub = function
		| [] -> []
		| h :: t -> (S h) :: (sub t)
	in function
	| V _ -> failwith "type error"
	| S u -> S (sub (parties u))
;;

let reduct = function
	| S (h :: []) -> h
	| a -> a
;;

let filter f a =
	let rec sub = function
		| V _ -> failwith "type error"
		| S [] -> S []
		| S (h :: t) ->
			let b = sub (S t) in
			if f h
			then h |:| b
			else b
	in reduct (sub a)
;;

let rec union = function
	| V _ -> failwith "type error"
	| S [] -> S []
	| S (h :: t) -> h |+| (union (S t))
;;

let rec list_to_set l =
	let rec sub_v = function
		| [] -> S []
		| h :: t -> (V h) |:| (sub_v t)
	and sub_s = function
		| [] -> []
		| h :: t -> (sub_v h) :: (sub_s t)
	in S (sub_s l)
;;

type skeleton = F | N of skeleton * skeleton;;

let get_skeleton t =
	let n = List.length t + 3 in
	let ropes = list_to_set t in
	let sides =
		let t = ref [] in
		for k = 1 to n do
			t := [k-1; k mod n] :: (!t);
		done; list_to_set !t in
	let links = sides |+| ropes in
	let f a = (~~a = 3) && (~~(union a) = 3) in
	let triangles = filter f (power_set links) in
	(* let father = S [V 0; V (n-1)] in
	let rec sub p t =
		
	in let f a = father |?| a in
	sub father (filter f triangles) *)
	triangles
;;
get_skeleton test;;