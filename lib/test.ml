type 'a abstract = { eq : 'a -> 'a -> bool; pp : 'a Fmt.t }

type _ t =
  | Int : int t
  | Float : float -> float t
  | String : string t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | List : 'a t -> 'a list t
  | Abstract : 'a abstract -> 'a t

let int = Int
let float epsilon = Float epsilon
let string = String
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
  | Pair (t1, t2) ->
      let x1, x2 = x in
      Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)
  | List t ->
      Printf.sprintf "[%s]" (List.map (to_string t) x |> String.concat ",")
  | Abstract abs -> Format.asprintf "%a" abs.pp x

let buf = Buffer.create 1000

let test name ty x y =
  if eq ty x y then Printf.bprintf buf "%s: OK\n" name
  else
    Printf.bprintf buf "%s: Failed\n  Expecting: %s\n  Got      : %s\n" name
      (to_string ty x) (to_string ty y)
