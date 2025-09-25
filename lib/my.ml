
let square x = x * x

let double (x : int) : int = 2 * x

let long_computation (x:int) : int = 
  let p = double (x + 4) in
  let q = square (double (square x)) in
  let r = 5 * (square (double x)) in
  p - q + r

let long_computation' (x : int) : int =
  let sq_db y = square (double y) in
  let p = double (x + 4) in
  let q = sq_db (square x) in
  let r = 5 * (sq_db x) in
  p - q + r  

let rec exp2 (n : int) : int =
  if n = 0 then 1 else 2 * exp2 (n - 1)

let absolute (n : int) : int =
    if n >= 0 then n else -n

let identity (x : 'a) : 'a = x

let num_period (s : string) : int =
  let num = ref 0 in
  let update c = 
    if c = '.' then num := !num + 1 else () 
  in
  String.iter update s;
  !num

let read_second_line () : unit =
  let _ = read_line () in
  let s = read_line () in
  print_endline s 

let rec echo () : unit =
  try 
    let s = read_line () in
    print_endline s;
    echo ()
  with
  | End_of_file -> ()

type suit = Hearts | Diamonds | Clubs | Spades
type rank = Num of int | Jack | Queen | King | Ace 
type card = rank * suit
type comparison = Higher | Lower | Equal

let string_of_suit (s : suit) : string =
  match s with
  | Hearts -> "\u{2661}"
  | Diamonds -> "\u{2662}"
  | Clubs -> "\u{2667}"
  | Spades -> "\u{2664}"

let string_of_rank (s : rank) : string =
  match s with
  | Num n -> string_of_int n
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let string_of_card (c : card) : string =
  match c with
  | (r, s) -> string_of_rank r ^ string_of_suit s

let value_card (c : card) : int =
  match c with
  | (Ace, _)   -> 1
  | (Num n, _) -> n
  | (Jack, _)  -> 11
  | (Queen, _) -> 12
  | (King, _)  -> 13

let comparison_of_string (s : string) : comparison =
  match s with
  | "l" | "L" | "lo" | "Lo" -> Lower
  | "h" | "H" | "hi" | "Hi" -> Higher
  | _                       -> failwith "Input was not of the correct shape."

let ranks = 
  [Ace; Num 2; Num 3; Num 4; Num 5; Num 6; Num 7; Num 8; Num 9; Jack; Queen; King]

let deck = 
  let heart_cards = List.map (fun r -> (r, Hearts)) ranks in
  let club_cards = List.map (fun r -> (r, Clubs)) ranks in
  let spade_cards = List.map (fun r -> (r, Spades)) ranks in
  let diamond_cards = List.map (fun r -> (r, Diamonds)) ranks in
  heart_cards @ club_cards @ spade_cards @ diamond_cards
