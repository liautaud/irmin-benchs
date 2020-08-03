open Bechamel
open Toolkit
module Diet = Diet.Int

(** Returns a printable ASCII character at random. *)
let random_char () = Char.chr (97 + Random.int 26)

(** Returns a byte-string of the given length at random. *)
let random_bytes n =
  let buf = Buffer.create n in
  for _ = 1 to n do
    Buffer.add_char buf (random_char ())
  done;
  Buffer.to_bytes buf

(** Generates a random interval. *)
let generate_interval () =
  let a = Random.int 1_000_000 in
  let b = a + Random.int 200 in
  Diet.Interval.make a b

(** Generates a DIET tree of a given [size] pre-filled with random intervals. *)
let generate_tree size =
  let rec aux n t =
    if n > 0 then aux (n - 1) (Diet.add (generate_interval ()) t) else t
  in
  aux size Diet.empty

(** Measures the time to add an interval to a DIET tree. *)
let add_interval size =
  let t = generate_tree size in
  let it = generate_interval () in
  Staged.stage @@ fun () -> ignore (Diet.add it t)

(** Measures the time to remove an interval from a DIET tree. *)
let remove_interval size =
  let t = generate_tree size in
  let it = Diet.choose t in
  Staged.stage @@ fun () -> ignore (Diet.remove it t)

(** Measures the time to find a sub-interval of some size from a DIET tree. *)
let take_interval size =
  let t = generate_tree size in
  Staged.stage @@ fun () -> ignore (Diet.take t 100)

(** The set of tests to be performed. *)
let test =
  let args = List.init 300 (fun i -> (i + 1) * 10) in
  Test.make_grouped ~name:"diet"
    [
      Test.make_indexed ~name:"add_interval" ~args add_interval;
      Test.make_indexed ~name:"remove_interval" ~args remove_interval;
      Test.make_indexed ~name:"take_interval" ~args take_interval;
    ]

(** Runs the benchmark and the analysis. *)
let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg = Benchmark.cfg ~run:3000 ~quota:(Time.second 0.5) () in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  results

(** Outputs a summary of the given measurements in JSON. *)
let summarize t =
  let path =
    Printf.sprintf "diet_benchs_%s.json" (random_bytes 6 |> Bytes.to_string)
  in
  let translate_entry k v o =
    match Analyze.OLS.estimates v with
    | Some [ x ] ->
        Scanf.sscanf k "%s@:%d" (fun n m ->
            `List [ `String n; `Int m; `Float x ] :: o)
    | _ -> o
  in
  let translate_all k v o =
    `List [ `String k; `List (Hashtbl.fold translate_entry v []) ] :: o
  in
  Logs.info (fun l -> l "Storing the measurements in %s." path);
  Yojson.Basic.to_file path (`List (Hashtbl.fold translate_all t []))

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.info (fun l -> l "Starting the benchmarks.");
  summarize @@ benchmark ()
