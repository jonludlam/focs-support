type 'a abstract = { eq : 'a -> 'a -> bool; pp : 'a Fmt.t }

type _ t =
  | Int : int t
  | Float : float -> float t
  | String : string t
  | Bool : bool t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | List : 'a t -> 'a list t
  | Abstract : 'a abstract -> 'a t

let int = Int
let float epsilon = Float epsilon
let string = String
let bool = Bool
let pair (x, y) = Pair (x, y)
let list l = List l
let abstract abs = Abstract abs

let rec eq : type a. a t -> a -> a -> bool =
 fun ty x y ->
  match ty with
  | Int -> Int.equal x y
  | Float epsilon ->
      let delta = Float.abs (x -. y) in
      delta < epsilon
  | String -> String.equal x y
  | Bool -> Bool.equal x y
  | Pair (t1, t2) ->
      let x1, x2 = x and y1, y2 = y in
      eq t1 x1 y1 && eq t2 x2 y2
  | List t -> not (List.exists not (List.map2 (eq t) x y))
  | Abstract abs -> abs.eq x y

let rec to_string : type a. a t -> a -> string =
 fun ty x ->
  match ty with
  | Int -> string_of_int x
  | Float _ -> string_of_float x
  | String -> x
  | Bool -> string_of_bool x
  | Pair (t1, t2) ->
      let x1, x2 = x in
      Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)
  | List t ->
      Printf.sprintf "[%s]" (List.map (to_string t) x |> String.concat ",")
  | Abstract abs -> Format.asprintf "%a" abs.pp x

let buf = Buffer.create 1000
let failure_detected = ref false
let first_failure = ref None

let test name ty x y =
  try
    if eq ty x y then Printf.bprintf buf "%s: OK\n" name
    else 
      ( failure_detected := true;
        (match !first_failure with | None -> first_failure := Some (name, to_string ty x, to_string ty y) | _ -> ());
        Printf.bprintf buf "%s: Failed\n  Expecting: %s\n  Got      : %s\n" name
          (to_string ty x) (to_string ty y))
  with e ->
    failure_detected := true;
    (match !first_failure with | None -> first_failure := Some (name, "no exception raised", "exception '"^(Printexc.to_string e)^"' was raised") | _ -> ());
    Printf.bprintf buf "Exception raised during test: %s\n" (Printexc.to_string e);
    ()
   
exception Tests_failed of string

let run fn =
  Buffer.clear buf;
  failure_detected := false;
  fn ();
  let _ = Jupyter_notebook.display "text/plain" (Buffer.contents buf) in
  if !failure_detected then (let (test,expected,observed) = Option.get !first_failure in raise (Tests_failed (Printf.sprintf "%s:\nExpecting %s\nGot %s" test expected observed)))


