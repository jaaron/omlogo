let string_of_chars l =
  let str = String.make (List.length l) ' ' in
  let rec helper i l = (match l with
    | [] -> str
    | c::cs -> Bytes.set str i c ; helper (i+1) cs) in
  helper 0 l;;

type token =
| STOP
| QUOTE
| COLON
| Word of string
| Num of int
| Lst of token list
| NoValue
and state = {next : unit -> token ;
	     dict : (string* fn) list ;
	     env  : (string * token) list ;
	     value  : token}
and fn =
| Builtin  of (state -> state)
| WordList of (string list) * (token list)
;;

let rec string_of_token = function
  | STOP -> "STOP"
  | QUOTE -> "\""
  | COLON -> ":"
  | Word s -> s
  | Num i -> string_of_int i
  | NoValue -> "_"
  | Lst ts -> List.fold_left (fun str -> fun t -> str^" "^(string_of_token t)) "[" ts

let parse = 
  let rec lookahead : char option ref = ref None in
  let isdigit c = (match c with
    | '0' | '1' | '2' | '3' | '4' | '5' |'6' |'7' |'8' | '9' -> true
    | _ -> false) in
  let isdelim c = (match c with
    | '(' | ')' | '[' | ']' | '{' | '}' -> true
    | _ -> false) in
  let buf = Bytes.create 1 in
  let rec skip_whitespace chan = (really_input chan buf 0 1 ;
			          match Bytes.get buf 0 with
			          | ' ' | '\n' | '\t' -> skip_whitespace chan
			          | c -> c) in
  let input_char chan = (match !lookahead with
    | Some c -> lookahead := None ;c
    | None   -> skip_whitespace chan) in
  let rec parse_token chan = 
    let rec helper c l = (match c with
      | ' ' | '\n' | '\t' -> string_of_chars (List.rev l)
      | _   -> if isdelim c
	then (lookahead := Some c ;
	      string_of_chars (List.rev l))
	else (really_input chan buf 0 1 ;
	      helper (Bytes.get buf 0) (c::l))) in
    match (input_char chan) with
    | '(' -> parse_list chan ')' []
    | ')' -> raise (Failure "Unexpected )")
    | '[' -> parse_list chan ']' []
    | ']' -> raise (Failure "Unexpected ]")
    | '{' -> parse_list chan '}' []
    | '}' -> raise (Failure "Unexpected }")
    | ':' -> COLON
    | '"' -> QUOTE
    | c   -> let w = helper c [] in
	     if isdigit c
	     then Num (int_of_string w)
	     else if w = "STOP" then STOP else Word w
  and parse_list chan r l = let c = (input_char chan) in
			    if c = r then Lst (List.rev l)
			    else (lookahead := Some c ;
				  parse_list chan r ((parse_token chan)::l)) in
  (fun chan -> fun _ -> parse_token chan);;


let rec run state =
  let rec bindargs state =
    let rec helper state acc =
      (function
      | [] -> {state with env = List.rev acc}
      | x::xs -> let state = run state in
		 (helper {state with value = NoValue} ((x,state.value)::acc) xs)) in
    helper state [] in
  (let tok = (state.next ()) in
   match tok with
   | STOP    -> {state with value = STOP}
   | Num n   -> {state with value = Num n}
   | QUOTE   -> {state with value = (state.next ())}
   | Word w  -> (match (try List.assoc w state.dict
     with Not_found -> raise (Failure ("Procedure \""^w^"\" not found"))) with
     | WordList (args, words) -> run_list words (bindargs state args)
     | Builtin f              -> f state)
   | COLON   -> (match (state.next ()) with
     | Word w -> {state with value = try List.assoc w state.env
       with Not_found -> raise (Failure ("Identifier \""^w^"\" not found")) }
     | _ -> raise (Failure ": must be followed by a symbol"))
   | Lst ws   -> run_list ws state
   | NoValue  -> {state with value = NoValue})
and run_list l origstate = let ws = ref l in
			   let nextfn _ = match !ws with
			     | [] -> STOP
			     | x::xs -> ((ws := xs) ; x) in
			   let rec helper st = match !ws with
			     | [] -> {st with next = origstate.next}
			     | _  -> helper (run {st with next = nextfn}) in
			   helper origstate

let read_until tok next = let rec helper acc = let n = (next ()) in
					       if n = tok then List.rev acc
					       else helper (n::acc) in
			  helper []
let drop_until tok next = let rec helper () = let n = (next ()) in
					      if n = tok then ()
					      else helper () in
			  helper ()


let init = {dict = [("PRINT", Builtin (fun st -> let st = (run st) in
						 st.value |> string_of_token |>
						     print_string ; print_string "\n" ;
						 {st with value = NoValue})) ;
		    ("SET", Builtin (fun st -> let st = (run st) in
					       match st.value with
					       | Word place -> let st = (run st) in
							       {st with env = (place,st.value)::st.env ;
								 value = NoValue}
					       | _ -> raise (Failure "in SET place must be a word"))) ;
		    ("REPEAT", Builtin (fun st -> (let st = (run st) in
                                                   match st.value with
                                                   | Num n -> let body = read_until (Word "END") st.next in
							      List.iter (fun w -> print_string ("-> " ^(string_of_token w)^"\n")) body ;
							      let rec fn = (function
								| 0 -> (fun (x : state) -> x)
								| x -> (fun st -> fn (x - 1)
								  (run_list body st))) in
							      (fn n st)
                                                   | _ -> raise (Failure "REPEAT bound must be a number")))) ;
		    ("TO", Builtin (fun st -> (match (st.next ()) with
		    | Word w -> (match (st.next ()) with
		      | Lst p_toks -> let ps = List.map (function | Word w -> w
			| _ -> raise (Failure "Parameter name must be a word")) p_toks in
				      {st with dict = (w, WordList (ps, read_until (Word "END") st.next))::st.dict}
		      | _ -> raise (Failure "Parameter list must be a list"))
		    | _      -> raise (Failure "Proc name must be a word")))) ;
		    ("IF", Builtin (fun st -> let st = (run st) in
					      match st.value with
					      | Num 0 -> drop_until (Word "ELSE") st.next ; 
						let st = (run st) in
						(match (st.next ()) with
						| Word "END" -> st
						| _          -> raise (Failure "Expected END after ELSE"))
					      | _     -> (match (st.next ()) with
						| Word "THEN" -> let st = (run st) in
								 (match (st.next ()) with
								 | Word "ELSE" -> drop_until (Word "END") st.next ; st
								 | Word "END"  -> st
								 | _           -> raise (Failure "Expected ELSE or END after THEN e"))
						| _ -> raise (Failure "Expected THEN after IF e"))))


		   ] ;
	    env = [] ;
	    next = (parse stdin) ;
	    value = NoValue
	   };;


let rec runprog s = match s.value with
  | STOP -> ()
  | v    -> runprog (run s)
;;


open Graphics

type point = {x : int ; y : int}
type direction = Degrees of int
		 | Radians of float
type gfx_state = {pos : point ;
		  dir : direction ;
                  pen : bool;
                 };;

let pi = 4.0 *. atan 1.0;;

let round x = (if x -. (floor x) >= 0.5
  then truncate (ceil x)
  else truncate x)
  
let to_radians = function
  | Radians r -> r
  | Degrees d -> (pi*.(float_of_int (2*d)) /. 360.0)

let to_degrees = function
  | Radians r -> round ((180.0 *. r) /. pi)
  | Degrees d -> d

let rec compute_end start dir dist =
  let theta = to_radians dir in
  {x = round ((float_of_int start.x) +.
		 ((float_of_int dist) *. (cos theta))) ;
   y = round ((float_of_int start.y) +.
		 ((float_of_int dist) *. (sin theta)))}

let graphics_height = 600
let graphics_width  = 600
let graphics_dict = let turtle = ref {pos = {x = graphics_height/2; y = graphics_width/2} ;
                                      dir = Degrees 0 ;
                                      pen = true} in
		    [("FORWARD", Builtin (fun st -> let st = (run st) in
						    match st.value with
						    | Num n -> let dest = compute_end (!turtle).pos (!turtle.dir) n in
							       if (!turtle).pen then lineto dest.x dest.y else () ;
							       turtle := {!turtle with pos = dest} ;
							       {st with value = NoValue}
						    | _     -> raise (Failure "FORWARD requires a numeric argument"))) ;
		     ("TURN",    Builtin (fun st -> let st = (run st) in
						    match st.value with
						    | Num n -> turtle := {!turtle with dir = Degrees (((to_degrees (!turtle).dir) + n) mod 360)} ; st
						    | _ -> raise (Failure "TURN requires a numeric argument")));
		     ("PENUP",   Builtin (fun st -> turtle := {!turtle with pen = false} ; st)) ;
		     ("PENDOWN", Builtin (fun st -> turtle := {!turtle with pen = true}; st))];;

open_graph(" "^(string_of_int graphics_width)^"x"^(string_of_int graphics_height))  ;
runprog {init with dict = graphics_dict@init.dict}
