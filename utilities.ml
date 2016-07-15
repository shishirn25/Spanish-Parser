(* utility functions for 2015 comp ling class
  Adapted from code distributed by Professor John Hale
*)


(* where you would be tempted to use this, consider instead the standard library function String.concat  *)
let interdigitate i x y = match (x,y) with
    x,"" -> x
  | "",y -> y
  | x,y -> x^i^y


(* back and forth from characters to string to char lists *)
let explode s =
  let rec explode s i = if i< String.length s
  then s.[i]::(explode s (i+1))
  else []
  in (explode s 0)

let implode l = 
  begin
    let v = (Bytes.create (List.length l)) in
      for i = 0 to ((List.length l)-1) do
	Bytes.set v i (List.nth l i)
      done; (v : string)
  end

let split_char s = 
  List.map (String.make 1) (explode s)


(* apply the one-place function f to an argument x *)
(* as a side effect, print out the time it took *)
let time f x =
  let time0 = Sys.time () in
  let result = f x in
  let time1 = Sys.time ()in
  Printf.printf "%2.2f seconds\n" ((time1 -. time0));
  result


(* divides a string s into a list of substrings at the points where it finds any of the *)
(* characters on the `splitters' list *)
let split_on splitters s =
  let rec tok remainingchars most_recent_word_accumulator = match remainingchars,most_recent_word_accumulator with
    [], w -> [implode (List.rev w)]
    | (c::cs), w ->
	if (List.mem c splitters)
	then (implode (List.rev w))::(tok cs [])
	else tok cs (c::w)
  in
  tok (explode s) []

(* Whitington chapter 4 with full pattern-match*)
let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t
      | [] -> []

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t
      | [] -> []




let divideat n l = (take n l,drop n l)


(* Batteries provides an infix function composition operator,  % *)

(* compose a function f with itself n times *)
let rec iterate f n x = match n with
    0 -> x
  | _ -> iterate f (n-1) (f x)


(* create a BatEnum.t from each line of a file *)
let from_file filename =
  let infile = open_in filename in
  let next () =
    try
      input_line infile
    with
	End_of_file -> (close_in infile ; raise BatEnum.No_more_elements)
  in
  BatEnum.from next


let write_file fname str =
  let oc = open_out fname in
    begin
      output_string oc str;
      close_out oc
    end
