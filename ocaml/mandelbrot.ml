#load "graphics.cma";;
open Graphics;;

let r = 200 and max_iteration = 69
and color_map = [|black; blue; cyan; green; yellow; red; magenta|];;

let s =  string_of_int (2*r) in
open_graph (s^"x"^s);;
set_window_title "Mandelbrot";;

let u = ref 1. and xc = ref 0. and yc = ref 0. in
let getos z = !u *. (z /. (float_of_int r) -. 1.) in
while true do
	for x=0 to size_x()-1 do
		for y=0 to size_y()-1 do
			let ai = getos (float_of_int x) +. !xc and bi = getos (float_of_int y) +. !yc in
			let a = ref ai and b = ref bi and aa = ref 0. and bb = ref 0. and count = ref 0 in
			while (!aa +. !bb < 4.) && (!count < max_iteration) do
				aa := !a *. !a;
				bb := !b *. !b;
				a := !aa -. !bb +. ai;
				b := 2. *. !a *. !b +. bi;
				incr count;
			done;
			set_color color_map.((!count / 10) mod 7);
			plot x y;
		done;
	done;
	let status = wait_next_event [Button_up] in
	xc := getos (float_of_int status.mouse_x) +. !xc;
	yc := getos (float_of_int status.mouse_y) +. !yc;
	u := !u /. 2.;
done;
;;

close_graph ();;