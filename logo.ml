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
   | Word w  -> (match List.assoc w state.dict with
		 | WordList (args, words) -> run_list words (bindargs state args)
		 | Builtin f              -> f state)					    
   | COLON   -> (match (state.next ()) with
  		 | Word w -> {state with value = List.assoc w state.env}
  		 | _ -> raise (Failure ": must be followed by a symbol"))
   | Lst ws   -> run_list ws state
   | NoValue  -> {state with value = NoValue})
 and run_list l state = run {state with next = (let ws = ref l in
					       fun _ -> match !ws with
							| [] -> STOP
 							| x::xs -> ((ws := xs) ; x))}
 
let init = {dict = [("PRINT", Builtin (fun st -> let st = (run st) in
						 st.value |> string_of_token |>
						   print_string ; print_string "\n" ;st)) ;
		      ("SET", Builtin (fun st -> let st = (run st) in
						 match st.value with
						 | Word place -> let st = (run st) in
								 {st with env = (place,st.value)::st.env ;
									  value = NoValue}
						 | _ -> raise (Failure "in SET place must be a word"))) ;
		      ("REPEAT", Builtin (fun st -> (let st = (run st) in
                                                     match st.value with
                                                     | Num n -> (match st.next () with
								 | Lst body -> let rec fn = (function
											      | 0 -> (fun (x : state) -> x)
											      | x -> (fun st -> fn (x - 1)
														   (run_list body st))) in
									       (fn n st)
								 | _ -> raise (Failure "REPEAT body must be a word list"))
                                                     | _ -> raise (Failure "REPEAT bound must be a number")))) ;
		      ("TO", Builtin (fun st -> (match (st.next ()) with
						 | Word w -> (match (st.next ()) with
							      | Lst p_toks -> let ps = List.map (function | Word w -> w
												 | _ -> raise (Failure "Parameter name must be a word")) p_toks in
									      let rec read_body acc = (match (st.next ()) with
												   | Word "END" -> List.rev acc
												   | x -> read_body (x::acc)) in
									      {st with dict = (w, WordList (ps, read_body []))::st.dict}
							      | _ -> raise (Failure "Parameter list must be a list"))
						 | _      -> raise (Failure "Proc name must be a word"))))

		   ] ;
	    env = [] ;
	    next = (parse stdin) ;
	    value = NoValue
	   };;
  
let rec runprog s = match s.value with
  | STOP -> ()
  | _    -> runprog (run s)
;;
  
runprog init
