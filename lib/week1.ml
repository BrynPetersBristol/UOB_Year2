

(*1*)
let rec fac (x : int) : int =
  if x = 0 then 1 else x * fac (x - 1)

(*2*)
let square x = x * x

let rec sum_squares (x : int) : int = 
  if x = 0 then 0 else square (x) + sum_squares (x - 1)

(*3*)
let rec sum_digits (n : int) : int =
  
  if n <= 9 then
    n
  else
    let p = n / 10 in
    let q = n mod 10 in
    q + sum_digits (p)

