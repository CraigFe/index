module I = Index
open Common

let ( // ) = Filename.concat

let root = "_tests" // "unix.main"

module Context = Common.Make_context (struct
  let root = root
end)

let main = Context.full_index ()

(* Helper functions *)
let rec random_new_key tbl =
  let r = Key.v () in
  if Hashtbl.mem tbl r then random_new_key tbl else r

let check_entry findf k v =
  match findf k with
  | v' when v = v' -> ()
  | v' (* v =/= v' *) ->
      Alcotest.failf "Wrong insertion: found %s when expected %s at key %s." v'
        v k
  | exception Not_found ->
      Alcotest.failf "Wrong insertion: %s key is missing." k

let check_index_entry index = check_entry (Index.find index)

let check_tbl_entry tbl = check_entry (Hashtbl.find tbl)

let check_equivalence index htbl =
  Hashtbl.iter (check_index_entry index) htbl;
  Index.iter (check_tbl_entry htbl) index

let test_replace t =
  let k = Key.v () in
  let v = Value.v () in
  let v' = Value.v () in
  Index.replace t k v;
  Index.replace t k v';
  check_index_entry t k v'

let test_find_absent t tbl =
  let rec loop i =
    if i = 0 then ()
    else
      let k = random_new_key tbl in
      Alcotest.check_raises (Printf.sprintf "Absent value was found: %s." k)
        Not_found (fun () -> ignore (Index.find t k));
      loop (i - 1)
  in
  loop 100

let mem_entry f k _ =
  if not (f k) then Alcotest.failf "Wrong insertion: %s key is missing." k

let mem_index_entry index = mem_entry (Index.mem index)

let mem_tbl_entry tbl = mem_entry (Hashtbl.mem tbl)

let check_equivalence_mem index tbl =
  Hashtbl.iter (mem_index_entry index) tbl;
  Index.iter (mem_tbl_entry tbl) index

(* Basic tests of find/replace on a live index *)
module Live = struct
  let find_present_live () =
    let Context.{ rw; tbl; _ } = Context.full_index () in
    check_equivalence rw tbl;
    Index.close rw

  let find_absent_live () =
    let Context.{ rw; tbl; _ } = Context.full_index () in
    test_find_absent rw tbl;
    Index.close rw

  let replace_live () =
    let Context.{ rw; _ } = Context.full_index () in
    test_replace rw;
    Index.close rw

  let different_size_for_key () =
    let Context.{ rw; _ } = Context.empty_index () in
    let k = String.init 2 (fun _i -> random_char ()) in
    let v = Value.v () in
    let exn = Index.Invalid_key_size k in
    Alcotest.check_raises
      "Cannot add a key of a different size than string_size." exn (fun () ->
        Index.replace rw k v);
    Index.close rw

  let different_size_for_value () =
    let Context.{ rw; _ } = Context.empty_index () in
    let k = Key.v () in
    let v = String.init 200 (fun _i -> random_char ()) in
    let exn = Index.Invalid_value_size v in
    Alcotest.check_raises
      "Cannot add a value of a different size than string_size." exn (fun () ->
        Index.replace rw k v);
    Index.close rw

  let membership () =
    let Context.{ rw; tbl; _ } = Context.full_index () in
    check_equivalence_mem rw tbl;
    Index.close rw

  let iter_after_clear () =
    let Context.{ rw; _ } = Context.full_index () in
    let () = Index.clear rw in
    Index.iter (fun _ _ -> Alcotest.fail "Indexed not cleared.") rw;
    Index.close rw

  let find_after_clear () =
    let Context.{ rw; tbl; _ } = Context.full_index () in
    let () = Index.clear rw in
    Hashtbl.fold
      (fun k _ () ->
        match Index.find rw k with
        | exception Not_found -> ()
        | _ -> Alcotest.fail "Indexed not cleared.")
      tbl ();
    Index.close rw

  let tests =
    [
      ("find (present)", `Quick, find_present_live);
      ("find (absent)", `Quick, find_absent_live);
      ("replace", `Quick, replace_live);
      ("fail add (key)", `Quick, different_size_for_key);
      ("fail add (value)", `Quick, different_size_for_value);
      ("membership", `Quick, membership);
      ("clear and iter", `Quick, iter_after_clear);
      ("clear and find", `Quick, find_after_clear);
    ]
end

(* Tests of behaviour after restarting the index *)
module DuplicateInstance = struct
  let find_present () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    let rw2 = clone ~readonly:false in
    check_equivalence rw tbl;
    Index.close rw;
    Index.close rw2

  let find_absent () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    let rw2 = clone ~readonly:false in
    test_find_absent rw tbl;
    Index.close rw;
    Index.close rw2

  let replace () =
    let Context.{ rw; clone; _ } = Context.full_index ~size:5 () in
    let rw2 = clone ~readonly:false in
    test_replace rw;
    Index.close rw;
    Index.close rw2

  let membership () =
    let Context.{ tbl; clone; rw } = Context.full_index () in
    let rw2 = clone ~readonly:false in
    check_equivalence_mem rw2 tbl;
    Index.close rw;
    Index.close rw2

  let fail_restart_ro_fresh () =
    let reuse_name = Context.fresh_name "empty_index" in
    let rw = Index.v ~fresh:true ~readonly:false ~log_size:4 reuse_name in
    let exn = Failure "IO.v: cannot reset a readonly file" in
    Alcotest.check_raises "Index readonly cannot be fresh." exn (fun () ->
        ignore (Index.v ~fresh:true ~readonly:true ~log_size:4 reuse_name));
    Index.close rw

  let sync () =
    let Context.{ rw; clone; _ } = Context.full_index () in
    let k1, v1 = (Key.v (), Value.v ()) in
    Index.replace rw k1 v1;
    let rw2 = clone ~readonly:false in
    let k2, v2 = (Key.v (), Value.v ()) in
    Index.replace rw2 k2 v2;
    check_index_entry rw k2 v2;
    check_index_entry rw2 k1 v1;
    Index.close rw;
    Index.close rw2

  let tests =
    [
      ("find (present)", `Quick, find_present);
      ("find (absent)", `Quick, find_absent);
      ("replace", `Quick, replace);
      ("membership", `Quick, membership);
      ("fail restart readonly fresh", `Quick, fail_restart_ro_fresh);
      ("in sync", `Quick, sync);
    ]
end

(* Tests of read-only indices *)
module Readonly = struct
  let readonly () =
    let Context.{ rw; clone; _ } = Context.empty_index () in
    let ro = clone ~readonly:true in
    Hashtbl.iter (fun k v -> Index.replace rw k v) main.tbl;
    Index.flush rw;
    check_equivalence ro main.tbl;
    Index.close rw;
    Index.close ro

  let readonly_clear () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    let ro = clone ~readonly:true in
    check_equivalence ro tbl;
    Index.clear rw;
    Hashtbl.iter
      (fun k _ ->
        Alcotest.check_raises (Printf.sprintf "Found %s key after clearing." k)
          Not_found (fun () -> ignore (Index.find ro k)))
      tbl;
    Index.close rw;
    Index.close ro

  let fail_readonly_add () =
    let Context.{ rw; clone; _ } = Context.empty_index () in
    let ro = clone ~readonly:true in
    let exn = I.RO_not_allowed in
    Alcotest.check_raises "Index readonly cannot write." exn (fun () ->
        Index.replace ro (Key.v ()) (Value.v ()));
    Index.close rw;
    Index.close ro

  (* Tests that the entries that are not flushed cannot be read by a readonly index. The test relies on the fact that, for log_size > 0, adding one entry into an empty index does not lead to flush/merge. *)
  let fail_readonly_read () =
    let Context.{ rw; clone; _ } = Context.empty_index () in
    let ro = clone ~readonly:true in
    let k1, v1 = (Key.v (), Value.v ()) in
    Index.replace rw k1 v1;
    Alcotest.check_raises "Index readonly cannot read if data is not flushed."
      Not_found (fun () -> ignore (Index.find ro k1));
    Index.close rw;
    Index.close ro

  let tests =
    [
      ("add", `Quick, readonly);
      ("read after clear", `Quick, readonly_clear);
      ("add not allowed", `Quick, fail_readonly_add);
      ("fail read if no flush", `Quick, fail_readonly_read);
    ]
end

(* Tests of {Index.close} *)
module Close = struct
  let close_reopen_rw () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    Index.close rw;
    let w = clone ~readonly:false in
    check_equivalence w tbl;
    Index.close w

  let find_absent () =
    let Context.{ rw; tbl; clone; _ } = Context.full_index () in
    Index.close rw;
    let rw = clone ~readonly:false in
    test_find_absent rw tbl;
    Index.close rw

  let replace () =
    let Context.{ rw; clone; _ } = Context.full_index ~size:5 () in
    Index.close rw;
    let rw = clone ~readonly:false in
    test_replace rw;
    Index.close rw

  let open_readonly_close_rw () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    let ro = clone ~readonly:true in
    Index.close rw;
    check_equivalence ro tbl;
    Index.close ro

  let close_reopen_readonly () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    Index.close rw;
    let ro = clone ~readonly:true in
    check_equivalence ro tbl;
    Index.close ro

  let test_read_after_close t k tbl =
    check_equivalence t tbl;
    Index.close t;
    Alcotest.check_raises "Read after close raises Not_found" Not_found
      (fun () -> ignore (Index.find t k))

  let test_read_after_close_readonly t k tbl =
    check_equivalence t tbl;
    Index.close t;
    let exn = Unix.Unix_error (Unix.EBADF, "read", "") in
    Alcotest.check_raises "Cannot read in readonly index after close." exn
      (fun () -> ignore (Index.find t k))

  let fail_read_after_close () =
    let Context.{ rw; tbl; _ } = Context.full_index () in
    let k, v = (Key.v (), Value.v ()) in
    Index.replace rw k v;
    Hashtbl.replace tbl k v;
    test_read_after_close rw k tbl

  let fail_write_after_close () =
    let Context.{ rw; _ } = Context.empty_index () in
    Index.close rw;
    let k, v = (Key.v (), Value.v ()) in
    (* a single add does not fail*)
    Index.replace rw k v;
    let exn = Unix.Unix_error (Unix.EBADF, "read", "") in
    Alcotest.check_raises "Cannot write in index after close." exn (fun () ->
        Hashtbl.iter (fun k v -> Index.replace rw k v) main.tbl)

  let open_twice () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    let rw2 = clone ~readonly:false in
    let k, v = (Key.v (), Value.v ()) in
    Index.replace rw k v;
    Hashtbl.replace tbl k v;
    Index.close rw;

    (* while another instance is still open, read does not fail*)
    check_equivalence rw tbl;
    test_read_after_close rw2 k tbl

  let open_twice_readonly () =
    let Context.{ rw; tbl; clone } = Context.full_index () in
    let k, v = (Key.v (), Value.v ()) in
    Index.replace rw k v;
    Hashtbl.replace tbl k v;
    Index.close rw;
    let ro1 = clone ~readonly:true in
    let ro2 = clone ~readonly:true in
    check_equivalence ro1 tbl;
    Index.close ro1;
    test_read_after_close_readonly ro2 k tbl

  let tests =
    [
      ("close and reopen", `Quick, close_reopen_rw);
      ("find (absent)", `Quick, find_absent);
      ("replace", `Quick, replace);
      ("open two instances, close one", `Quick, open_readonly_close_rw);
      ("close and reopen on readonly", `Quick, close_reopen_readonly);
      ("fail to read after close", `Quick, fail_read_after_close);
      ("fail to write after close", `Quick, fail_write_after_close);
      ("open twice same instance", `Quick, open_twice);
      ("open twice same instance readonly", `Quick, open_twice_readonly);
    ]
end

let () =
  Common.report ();
  Alcotest.run "index.unix"
    [
      ("io_array", Io_array.tests);
      ("merge", Force_merge.tests);
      ("live", Live.tests);
      ("on restart", DuplicateInstance.tests);
      ("readonly", Readonly.tests);
      ("close", Close.tests);
    ]
