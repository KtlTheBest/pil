type 'a res_list =
  | Ok of 'a
  | Err of string list

let (>>=) a f =
  match a with
  | Ok v -> f v
  | Err _ -> a

let (<*>) f a =
  match f with
  | Ok f' -> a >>= f'
  | Err sa -> Err sa


type 'a t =
  | Succ of 'a
  | Fail of string

let succ a = Succ a
let fail s = Fail s
