type 'a gen = int -> 'a
type pretty_str = Format.formatter -> unit -> unit

let generate : int -> 'a gen -> 'a =
  fun n m ->
    let size = Random.int n in
    m size

type result = {
  ok : bool option;
  stamp : string list;
  arguments : pretty_str list;
}

type config = {
  maxTest : int;
  maxFail : int;
  size    : int -> int;
  every   : Format.formatter -> int * pretty_str list -> unit;
}

let quick = {
  maxTest = 100;
  maxFail = 1000;
  size    = (fun n -> 3 + n / 2);
  every   = (fun f (n, _) -> Format.fprintf f "%d" n) 
}

let done_ : string -> int -> string list list -> unit =
  fun mesg ntest stamps ->
    ()

let rec tests : config -> result gen -> int -> int -> string list list -> unit =
    fun config gen ntest nfail stamps ->
      if ntest = config.maxTest
      then done_ "OK, passed" ntest stamps
      else if nfail = config.maxFail
      then done_ "Arguments exhausted after" nfail stamps
      else begin
        let result = generate (config.size ntest) gen in
        let () =
          Format.printf "@[%a@]@?" config.every (ntest, result.arguments)
        in
        match result.ok with
            None ->
              tests config gen ntest (nfail+1) stamps
          | Some true ->
              tests config gen (ntest+1) nfail (result.stamp :: stamps)
          | Some false ->
              let p f = function
                  [] -> ()
                | h::t ->
                    h f ();
                    List.iter (fun s -> Format.fprintf f "@ %a" s ()) t
              in
              Format.printf "@[<2>Falsifiable, after %d tests:@ %a@]@."
                ntest p result.arguments
      end


(* (set (make-local-variable 'flymake-ocaml-build-file) "Makefile") *)
