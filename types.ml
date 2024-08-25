type tt =
  | Unit
  | Bool
  | Char
  | Int
  | Float
  | String
  | CustomType of string
  | Array of tt
  | Unknown

let arr_index = [Unit; Bool; Char; Int; Float; String; CustomType ""; Array Unknown]

let index_of_t t =
  match t with
  | Unit -> 0
  | Bool -> 1
  | Char -> 2
  | Int -> 3
  | Float -> 4
  | String -> 5
  | CustomType _ -> 6
  | Array _ -> 7
  | Unknown -> 8

let rec compare_types a b =
  match a, b with
  | Array a', Array b' -> compare a' b'
  | CustomType a', CustomType b' -> compare a' b'
  | _, _ -> compare (index_of_t a) (index_of_t b)

module TypeM = struct
  type t = tt
  let compare = compare_types
end

module SetType = Set.Make(TypeM)

let rec string_of t =
  match t with
  | Unit -> "unit"
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | CustomType s -> s
  | Array(t') -> "array(" ^ string_of t' ^ ")"
  | Unknown -> "unknown"
