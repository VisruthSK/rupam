let between a b =
  let row = Array.init (String.length b + 1) Fun.id in
  String.iteri
    (fun i x ->
       let diagonal = ref row.(0) in
       row.(0) <- i + 1;
       String.iteri
         (fun j y ->
            let old = row.(j + 1) in
            row.(j + 1) <- min (!diagonal + Bool.to_int (x <> y)) (1 + min row.(j) old);
            diagonal := old)
         b)
    a;
  row.(String.length b)
;;
