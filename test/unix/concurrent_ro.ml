module I = Index
open Common

let index_name = Filename.concat "_tests" "unix.concurrent_ro"

let test_find_present t tbl =
  let i = ref ~-1 in
  Hashtbl.iter
    (fun k v ->
      incr i;
      match Index.find t k with
      | exception Not_found ->
          Alcotest.failf
            "Wrong insertion: %s key is missing. (element %d in scan)" k !i
      | v' ->
          if not (v = v') then
            Alcotest.failf
              "Wrong insertion: %s value is missing. (element %d in scan)" v !i)
    tbl

let add_values t tbl =
  for _ = 1 to 10 do
    let k = Key.v () in
    let v = Value.v () in
    Index.replace t k v;
    Hashtbl.replace tbl k v
  done;
  Index.flush t

let print_fan_out_state w r =
  Logs.err (fun m ->
      m "R/W fan out state:\n%a\n\nR/O fan out state:\n%a\n\n" I.Private.Fan.pp
        (Index.fan_out w) I.Private.Fan.pp (Index.fan_out r))

let reads () =
  let tbl = tbl index_name in
  let w = Index.v ~fresh:false ~log_size index_name in
  let r = Index.v ~fresh:false ~readonly:true ~log_size index_name in
  test_find_present w tbl;
  test_find_present r tbl;
  print_fan_out_state w r;
  Logs.info (fun m -> m "Adding additional values to the index");
  add_values w tbl;
  Logs.info (fun m ->
      m "Checking that the r/w instance finds the additional values");
  test_find_present w tbl;
  Index.flush w;

  print_fan_out_state w r;

  (*this test fails*)
  Logs.info (fun m ->
      m "Checking that the r/o instance finds the additional values");

  test_find_present r tbl

let tests = ("concurrent", [ ("concurrent reads", `Quick, reads) ])

let () =
  Common.report ();
  Alcotest.run "index" [ tests ]
