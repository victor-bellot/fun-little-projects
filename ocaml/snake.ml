#load "graphics.cma";;
open Graphics;;

type vect = {mutable x : int; mutable y : int};;
let new_v x_ y_ = {x = x_; y = y_};;
let (|+|) v1 v2 = new_v (v1.x + v2.x) (v1.y + v2.y);;

let u = 56 and w = 16 and h = 16;;
let timustr n = string_of_int (u*n)
and is_out v = v.x < 0 || v.y < 0 || w <= v.x || h <= v.y
and eval b = if b then 1 else 0;;

open_graph ((timustr w)^"x"^(timustr h));
set_window_title "Snake";
set_color black; fill_rect 0 0 (u*w) (u*h);
let draw v c =
	set_color c;
	fill_rect (u * v.x) (u * v.y) u u; in
let tab = ref [|new_v (w/2) (h/2)|] and p = ref 0 in
let grow n k =
	let nt = Array.make (n+k) (new_v w h) in
	for i=0 to n+k-1 do
		if i < !p+1 || !p+k < i
		then nt.(i) <- !tab.(i - k * eval (!p+k < i));
	done; nt in
let colide v =
	let flag = ref false in
	for i=0 to Array.length !tab - 1 do
		if v = !tab.(i) && i <> !p
		then flag := true;
	done; !flag in
let t = ref 0. and dt = ref 0.1 and delta = new_v 0 0
and fruit = ref (new_v (w/2) (h/2)) in
let rec next_fruit v =
	let rv = new_v (Random.int w) (Random.int h) in
	if colide rv || (rv = v) then next_fruit v
	else (fruit := rv; draw rv red) in next_fruit !tab.(!p);
while not (is_out !tab.(!p) || colide !tab.(!p)) do
	if key_pressed ()
	then begin
		let k = read_key () in
		delta.x <- eval (k='d') - eval (k='q');
		delta.y <- eval (k='z') - eval (k='s');
	end;
	if !t +. !dt < Sys.time ()
	then begin
		t := Sys.time ();
		let nv = !tab.(!p) |+| delta in
		let n = Array.length !tab in p := (!p+1) mod n;
		draw !tab.(!p) black; !tab.(!p) <- nv; draw nv green;
		if nv = !fruit
		then (next_fruit nv; tab := grow n 1)
	end;
done;
close_graph ();
;;