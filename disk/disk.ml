(** Performance benchmark of various disk access patterns.

    This benchmark consists of reading from and writing to a single file on the
    filesystem either sequentially, sequentially with O_APPEND, or at randomized
    offsets. Samples are taken several times and averaged for various file
    sizes. *)

let ( ++ ) = Int64.add

let ( -- ) = Int64.sub

(** Number of times each measure is taken and averaged. *)
let nb_runs = 200

(** Values of [n] to sample. *)
let nspace = Owl.Mat.logspace 1. 100. 200

(** Returns a file descriptor opened to a new temporary file. *)
let create ?(flags = []) () =
  let path =
    Bos.OS.File.tmp "irmin_bench_disk_%s" |> Result.get_ok |> Fpath.to_string
  in
  Logs.debug (fun l -> l "Creating temporary file %s." path);
  Unix.openfile path (Unix.[ O_CREAT; O_RDWR ] @ flags) 0o644

(** Seeks a Unix file descriptor. *)
let lseek fd off =
  let r = Unix.LargeFile.lseek fd off Unix.SEEK_SET in
  assert (r >= 0L)

(** Writes bytes to a Unix file descriptor. *)
let write fd buf =
  let rec aux off len =
    let w = Unix.write fd buf off len in
    if w = 0 || w = len then () else (aux [@tailcall]) (off + w) (len - w)
  in
  (aux [@tailcall]) 0 (Bytes.length buf)

(** Reads bytes from a Unix file descriptor. *)
let read fd len buf =
  let rec aux off len =
    let r = Unix.read fd buf off len in
    if r = 0 then off (* end of file *)
    else if r = len then off + r
    else (aux [@tailcall]) (off + r) (len - r)
  in
  (aux [@tailcall]) 0 len

(** Returns a printable ASCII character at random. *)
let random_char () = Char.chr (97 + Random.int 26)

(** Returns a byte-string of the given length at random. *)
let random_bytes n =
  let buf = Buffer.create n in
  for _ = 1 to n do
    Buffer.add_char buf (random_char ())
  done;
  Buffer.to_bytes buf

(** Randomly shuffles an array. *)
let random_shuffle arr =
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done

(** Returns the average of a list of floats. *)
let list_average l = List.fold_left ( +. ) 0. l /. float (List.length l)

(** Measures the runtime of a function and stores the measurement. *)
let measure t name param f =
  let start = Sys.time () in
  f ();
  let stop = Sys.time () in
  let time = stop -. start in
  let prev = Hashtbl.find_opt t (name, param) |> Option.value ~default:[] in
  Hashtbl.replace t (name, param) (time :: prev)

(** Outputs a summary of the given measurements in JSON. *)
let summarize t =
  let open Hashtbl in
  let avgs = create (length t) in
  let path =
    Printf.sprintf "disk_benchs_%s.json" (random_bytes 6 |> Bytes.to_string)
  in
  Logs.info (fun l -> l "Storing the measurements in %s." path);
  iter (fun k v -> add avgs k (list_average v)) t;
  Yojson.Basic.to_file path
    (`List
      (fold
         (fun (n, p) v o -> `List [ `String n; `Int p; `Float v ] :: o)
         avgs []))

(* Benchmark 1:
   -----------
  Sequentially writes and reads [n] 4096-byte blocks of data to the disk. *)
let bench_sequential t n =
  Logs.info (fun l -> l "Starting bench_sequential with n = %n." n);
  let file = create () in
  let buffer = Bytes.create 4096 in
  for _ = 0 to nb_runs do
    measure t "sequential.write" n (fun () ->
        for _ = 0 to n - 1 do
          write file (random_bytes 4096)
        done);
    lseek file 0L;
    measure t "sequential.read" n (fun () ->
        for _ = 0 to n - 1 do
          ignore @@ read file 4096 buffer
        done)
  done;
  Unix.close file

(* Benchmark 2:
   -----------

   Sequentially writes and reads [n] 4096-byte block of data to the disk, but
   opening the file descriptor with [O_APPEND]. *)
let bench_append t n =
  Logs.info (fun l -> l "Starting bench_append with n = %n." n);
  let file = create ~flags:Unix.[ O_APPEND ] () in
  let buffer = Bytes.create 4096 in
  for _ = 0 to nb_runs do
    measure t "append.write" n (fun () ->
        for _ = 0 to n - 1 do
          write file (random_bytes 4096)
        done);
    lseek file 0L;
    measure t "append.read" n (fun () ->
        for _ = 0 to n - 1 do
          ignore @@ read file 4096 buffer
        done)
  done;
  Unix.close file

(* Benchmark 3:
   -----------
   Writes and reads [n] 4096-byte blocks of data to the disk at random offsets. *)
let bench_random t n =
  Logs.info (fun l -> l "Starting bench_random with n = %n." n);
  let file = create () in
  let buffer = Bytes.create 4096 in
  for _ = 0 to nb_runs do
    let offsets = Array.init n (fun i -> i * 4096) in
    random_shuffle offsets;
    measure t "random.write" n (fun () ->
        for i = 0 to n - 1 do
          lseek file (Int64.of_int offsets.(i));
          write file (random_bytes 4096)
        done);
    lseek file 0L;
    measure t "random.read" n (fun () ->
        for i = 0 to n - 1 do
          lseek file (Int64.of_int offsets.(i));
          ignore @@ read file 4096 buffer
        done)
  done;
  Unix.close file

(** Runs the benchmarks and summarizes the measurements. *)
let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let t = Hashtbl.create 1_000_000 in
  Owl.Mat.iter (fun n ->
    let n = int_of_float n in
    bench_sequential t n;
    bench_append t n;
    bench_random t n)
    nspace;
  summarize t
