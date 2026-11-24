exception Overflow

let add x y =
  let z = Int64.add x y in
  if Int64.logand (Int64.logxor x z) (Int64.logxor y z) < 0L then raise Overflow;
  z

let sub x y =
  let z = Int64.sub x y in
  if Int64.logand (Int64.logxor x y) (Int64.logxor x z) < 0L then raise Overflow;
  z

let mul x y =
  if x = Int64.min_int && y = -1L then raise Overflow;
  let z = Int64.mul x y in
  if y <> 0L && Int64.div z y <> x then raise Overflow;
  z
