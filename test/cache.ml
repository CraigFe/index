let check_none msg = Alcotest.(check (option reject)) msg None
let check_some msg x = Alcotest.(check (option int)) msg (Some x)

let%test "noop" =
  let open Index.Cache.Noop in
  (* Test that added entries are never found. *)
  let c = create () in
  find c "not-added" |> check_none "Cannot find non-existent value";
  add c "added" 1;
  find c "added" |> check_none "Cannot find added value";
  remove c "added";
  find c "added" |> check_none "Cannot find added value after remove";
  ()

let%test "unbounded" =
  let open Index.Cache.Unbounded in
  (* Test that added entries are always found. *)
  let c = create () in
  find c "not-added" |> check_none "Cannot find non-existent value";
  add c "added" 1;
  find c "added" |> check_some "Can find added value" 1;
  remove c "added";
  find c "added" |> check_none "Cannot find added value after remove";
  ()
