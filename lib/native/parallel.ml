type t =
  | Sequential
  | Pool of Domainslib.Task.pool * int

let threshold = Int64.of_int (8 lsl 20)

let run ?(workers = 15) ~size f =
  let workers = min workers (Domain.recommended_domain_count () - 1) in
  if workers < 1 || size < threshold
  then f Sequential
  else (
    let pool = Domainslib.Task.setup_pool ~num_domains:workers () in
    Fun.protect
      ~finally:(fun () -> Domainslib.Task.teardown_pool pool)
      (fun () -> Domainslib.Task.run pool (fun () -> f (Pool (pool, workers)))))
;;

let each t count body =
  match t with
  | Sequential ->
    for k = 0 to count - 1 do
      body k
    done
  | Pool (pool, _) ->
    if count > 0
    then
      Domainslib.Task.parallel_for pool ~chunk_size:1 ~start:0 ~finish:(count - 1) ~body
;;

let ordered t ~next ~work consume =
  match t with
  | Sequential -> Seq.iter (fun x -> consume (work x)) (Seq.of_dispenser next)
  | Pool (pool, workers) ->
    let pending = Queue.create () in
    let settle () = consume (Domainslib.Task.await pool (Queue.pop pending)) in
    let settle_all () =
      while not (Queue.is_empty pending) do
        settle ()
      done
    in
    let rec go () =
      match next () with
      | exception e ->
        settle_all ();
        raise e
      | None -> settle_all ()
      | Some x ->
        Queue.push (Domainslib.Task.async pool (fun () -> work x)) pending;
        if Queue.length pending > 2 * workers then settle ();
        go ()
    in
    go ()
;;
