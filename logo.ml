let string_of_chars l =
  let str = String.make (List.length l) ' ' in
  let rec helper i l = (match l with
    | [] -> str
    | c::cs -> Bytes.set str i c ; helper (i+1) cs) in
  helper 0 l;;

type token =
  | Word of string
  | Num of int
  | QLst of token list
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


let word_of_bool = function
  | true -> Word "TRUE"
  | false -> Word "FALSE"

let rec join_tokens l =
  List.fold_left (fun str -> fun t ->
    str^(if String.length str = 0
         then "" else " ")^
    (string_of_token t)) "" l
and string_of_token = function | Word s -> s
                               | Num i -> string_of_int i
                               | NoValue -> "_"
                               | Lst ts -> "(" ^ (join_tokens ts) ^")"
                               | QLst ts -> "[" ^ (join_tokens ts) ^"]"

let parse = 
  let rec lookahead : char option ref = ref None in
  let isdigit c = (match c with | '0' | '1' | '2' | '3'
                                | '4' | '5' | '6' | '7'
                                |'8' | '9' -> true
                                | _ -> false) in
  let isdelim c = (match c with | '(' | ')' | '['
                                | ']' | '"' | ':' -> true
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
    let rec helper c l = (match c with | ' '
                                       | '\n'
                                       | '\t' -> string_of_chars (List.rev l)
                                       | _    -> (if isdelim c
                                                  then (lookahead := Some c ;
                                                        string_of_chars (List.rev l))
                                                  else (really_input chan buf 0 1 ;
                                                        helper (Bytes.get buf 0) (c::l)))) in
    match (input_char chan) with
    | '(' -> parse_list chan ')' []
    | ')' -> raise (Failure "Unexpected )")
    | '[' -> parse_list chan ']' []
    | ']' -> raise (Failure "Unexpected ]")
    | c   -> (if isdelim c
              then Word (string_of_chars [c])
              else let w = helper c [] in
                if (isdigit c || c = '-')
                then Num (int_of_string w)
                else Word w)
  and parse_list chan r l = let c = (input_char chan) in
    if c = r then (if c = ']'
                   then QLst (List.rev l)
                   else Lst (List.rev l))
    else (lookahead := Some c ;
          parse_list chan r ((parse_token chan)::l)) in
  (fun chan -> fun _ -> parse_token chan);;


let rec run state =
  let rec bindargs state acc = function | []    -> {state with env = List.rev acc}
                                        | x::xs -> (let state = run state in
                                                    (bindargs {state with value = NoValue} 
                                                              ((x,state.value)::acc) xs)) in
  (let tok = (state.next ()) in
   match tok with
   | Num n   -> {state with value = Num n}
   | QLst l  -> {state with value = Lst l}
   | Word w  -> (match (try List.assoc w state.dict
                        with Not_found -> raise (Failure ("Procedure \""^w^"\" not found"))) with
                | WordList (args, words) -> {(run_list words (bindargs state [] args))
                                             with env = state.env}
                | Builtin f              -> f state)
   | Lst ws   -> run_list ws state
   | NoValue  -> {state with value = NoValue})
and run_list l origstate = let ws = ref l in
  let nextfn _ = match !ws with
    | []    -> Word "STOP"
    | x::xs -> ((ws := xs) ; x) in
  let rec helper st = match !ws with
    | [] -> {st with next = origstate.next}
    | _  -> helper (run {st with next = nextfn}) in
  helper origstate

let read_until tok next =
  let rec helper acc = (let n = (next ()) in
                        if n = tok
                        then List.rev acc
                        else helper (n::acc)) in
  helper []

let init = {dict = [("TRUE", Builtin (fun st -> {st with value = Word "TRUE"})) ;
                    ("FALSE", Builtin (fun st -> {st with value = Word "FALSE"})) ;
                    ("STOP", Builtin (fun st -> {st with value = Word "STOP"})) ;
                    ("\"", Builtin (fun st -> {st with value = (st.next ())})) ;

                    (":", Builtin (fun st -> (match st.next () with
                                              | Word w -> (try {st with value = (List.assoc w st.env)}
                                                           with Not_found -> raise (Failure ("Identifier \""^w^"\" not found")))
                                              | Num n  -> {st with value = Num n}
                                              | _      -> raise (Failure ": must be followed by a word")))) ;

                    ("PRINT", Builtin (fun st -> (let st = (run st) in
                                                  st.value |> string_of_token |>
                                                  print_string ; print_string "\n" ;
                                                  flush_all () ;
                                                  {st with value = NoValue}))) ;

                    ("SET", Builtin (fun st -> (let st = (run st) in
                                                match st.value with
                                                | Word place -> let st = (run st) in
                                                    {st with env = (place,st.value)::st.env ;
                                                             value = NoValue}
                                                | _ -> raise (Failure "in SET place must be a word")))) ;

                    ("REPEAT", Builtin (fun st -> (let st = (run st) in
                                                   match st.value with
                                                   | Num n -> (let st = (run {st with value = NoValue}) in
                                                               match st.value with
                                                               | Lst body ->
                                                                   let rec fn = (function
                                                                     | 0 -> (fun (x : state) -> {x with value = NoValue})
                                                                     | x -> (fun st -> fn (x - 1) (run_list body {st with value = NoValue}))) in
                                                                   (fn n st)
                                                               | _ -> raise (Failure "REPEAT body must be a list"))
                                                   | _ -> raise (Failure "REPEAT bound must be a number")))) ;

                    ("TO", Builtin (fun st -> (match (st.next ()) with
                                               | Word w -> (match (st.next ()) with
                                                            | QLst p_toks ->
                                                                (let param_name = function | Word w -> w 
                                                                                           | _ -> raise (Failure 
                                                                                                           "Parameter name must be a word") in
                                                                 let ps = List.map param_name p_toks in
                                                                 let body = read_until (Word "END") st.next in
                                                                 let proc = WordList (ps,body) in
                                                                 {st with value = NoValue ;
                                                                          dict  = ((w, proc)::st.dict)})
                                                            | _ -> raise (Failure "Parameter list must be a quoted list"))
                                               | _      -> raise (Failure "Proc name must be a word")))) ;

                    ("IF", Builtin (fun st -> (let st = (run st) in
                                               match st.value with
                                               | Word "TRUE"     -> (let st = (run st) in
                                                                     (match st.value with
                                                                      | Lst l -> {(run_list l st) with value = NoValue}
                                                                      | _     -> raise (Failure "THEN clause of IF must be a list")))
                                               | Word "FALSE"    -> ignore (st.next ()) ; {st with value = NoValue}
                                               | _               -> raise (Failure "IF Condition must be either TRUE or FALSE")))) ;

                    ("IFELSE", Builtin (fun st -> (let st = (run st) in
                                                   match st.value with
                                                   | Word "TRUE"  -> (let st = (run st) in
                                                                      (match st.value with
                                                                       | Lst l -> {(run_list l st) with value = NoValue}
                                                                       | _     -> raise (Failure "THEN clause of IFELSE must be a list")))
                                                   | Word "FALSE" -> (ignore (st.next ()) ; 
                                                                      let st = (run st) in
                                                                      match st.value with
                                                                      | Lst l -> {(run_list l st) with value = NoValue}
                                                                      | _     -> raise (Failure "ELSE clause of IFELSE must be a list"))
                                                   | _     -> raise (Failure "IFELSE Condition must be either TRUE or FALSE"))))


                   ] ;
            env = [] ;
            next = (fun _ -> try (parse stdin ()) with End_of_file -> Word "STOP");
            value = NoValue
           };;


let rec runprog s = match s.value with
  | Word "STOP" -> ()
  | v    -> runprog (run s)
;;

let binop name f = (name, Builtin (fun st ->
  let st = (run st) in
  (match st.value with
   | Num x -> let st = (run st) in
       (match st.value with
        | Num y -> {st with value = f x y}
        | _     -> raise (Failure ("Second argument to "^
                                   name^
                                   " must be a number")))
   | _ -> raise (Failure ("First argument to "^
                          name^
                          " must be a number")))))
let arithmetic_dict =
  [binop "SUM" (fun x -> fun y -> Num (x + y)) ;
   binop "DIFFERENCE" (fun x -> fun y -> Num (x - y)) ;
   binop "PRODUCT" (fun x -> fun y -> Num (x * y)) ;
   binop "QUOTIENT" (fun x -> fun y -> Num (x/y)) ;
   binop "REMAINDER" (fun x -> fun y -> Num (x mod y)) ;
   binop "GREATERP" (fun x -> fun y -> if x > y then Word "TRUE" else Word "FALSE");
   binop "LESSP" (fun x -> fun y -> if x < y then Word "TRUE" else Word "FALSE")];;

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
                             if (!turtle).pen
                             then lineto dest.x dest.y
                             else moveto dest.x dest.y ;
                             turtle := {!turtle with pos = dest} ;
                             {st with value = NoValue}
                         | _     -> raise (Failure "FORWARD requires a numeric argument"))) ;
   ("TURN",    Builtin (fun st -> let st = (run st) in
                         match st.value with
                         | Num n -> (turtle := {!turtle with dir = Degrees (((to_degrees (!turtle).dir) + n) mod 360)} ; 
                                     {st with value = NoValue})
                         | _ -> raise (Failure "TURN requires a numeric argument")));
   ("PENUP",   Builtin (fun st -> turtle := {!turtle with pen = false} ; {st with value = NoValue})) ;
   ("PENDOWN", Builtin (fun st -> turtle := {!turtle with pen = true}; {st with value = NoValue})) ;  
   ("CLEAR",   Builtin (fun st -> clear_graph () ; {st with value = NoValue})) ;
   ("GETX",    Builtin (fun st -> {st with value = Num (!turtle).pos.x})) ;
   ("GETY",    Builtin (fun st -> {st with value = Num (!turtle).pos.y})) ;
   ("GETPEN",  Builtin (fun st -> {st with value = word_of_bool (!turtle).pen})) ;
   binop "MOVETO" (fun x -> fun y -> moveto x y ; turtle := {!turtle with pos = {x = x ; y = y}} ; NoValue)
];;

open_graph(" "^(string_of_int graphics_width)^"x"^(string_of_int graphics_height))  ;
moveto (graphics_width/2) (graphics_height/2) ;
runprog {init with dict = graphics_dict@arithmetic_dict@init.dict}
