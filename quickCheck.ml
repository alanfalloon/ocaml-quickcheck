module Random = struct
  include Random
  let char : char -> char =
    fun lim ->
      let l = Char.code lim in
      let i = int l in
      Char.chr i
  let int_range : int * int -> int =
    fun (lo, hi) ->
      lo + int (hi-lo)
  let int32_range : Int32.t * Int32.t -> Int32.t =
    fun (lo, hi) ->
      Int32.add lo (int32 (Int32.sub hi lo))
  let int64_range : Int64.t * Int64.t -> Int64.t =
    fun (lo, hi) ->
      Int64.add lo (int64 (Int64.sub hi lo))
  let nativeint_range : Nativeint.t * Nativeint.t -> Nativeint.t =
    fun (lo, hi) ->
      Nativeint.add lo (nativeint (Nativeint.sub hi lo))
  let float_range : float * float -> float =
    fun (lo, hi) ->
      lo +. float (hi -. lo)
  let char_range : char * char -> char =
    fun (lo, hi) ->
      let lo' = Char.code lo and hi' = Char.code hi in
      let i = int_range (lo', hi') in
      Char.chr i
end

type 'a gen = Gen of (int -> 'a)
type pretty_str = Format.formatter -> unit -> unit

(* generator functions *)

let sized : (int -> 'a gen) -> 'a gen =
  fun f -> Gen (fun n ->
                  let Gen m = f n in
                  m n)

let resize : int -> 'a gen -> 'a gen =
  fun n (Gen m) -> Gen (fun _ -> m n)

let promote : ('a -> 'b gen) -> ('a -> 'b) gen =
  fun f ->
    Gen (fun n ->
           fun a ->
             let Gen m = f a in
             m n)

let variant : int -> 'a gen -> 'a gen =
  fun _v (Gen m) -> Gen (fun n -> m n)

let generate : int -> 'a gen -> 'a =
  fun n (Gen m) ->
    let size = Random.int n in
    m size

let map_gen : ('a -> 'b) -> 'a gen -> 'b gen =
  fun f (Gen m) ->
    Gen (fun n ->
           let v = m n in
           f v)

let ret_gen : 'a -> 'a gen =
  fun a -> Gen (fun _n -> a)

let (>>=) : 'a gen -> ('a -> 'b gen) -> 'b gen =
  fun (Gen m) k ->
    Gen (fun n ->
           let v = m n in
           let Gen m' = k v in
           m' n)

let lift_gen : ('a -> 'b) -> 'a -> 'b gen =
  fun f -> fun a -> Gen (fun _ -> f a)

let choose_int = lift_gen Random.int_range
let choose_int0 = lift_gen Random.int
let choose_char = lift_gen Random.char_range
let choose_float = lift_gen Random.float_range

let elements : 'a list -> 'a gen =
  fun xs ->
    map_gen (List.nth xs)
      (choose_int0 (List.length xs))

let vector : 'a gen -> int -> 'a list gen =
  fun (Gen gelt) l ->
    Gen (fun n ->
           let rec gen acc = function
               0 -> acc
             | l -> gen (gelt n :: acc) (l-1)
           in gen [] l)

let oneof : 'a gen list -> 'a gen =
  fun gens -> elements gens >>= fun x -> x

module Arbitrary = struct
  let unit = ret_gen ()
  let bool = elements [true; false]
  let char =
    choose_int (32,255) >>= fun c -> 
      ret_gen (Char.chr c)
  let int = sized (fun n -> choose_int (-n, n))
  let float =
    int >>= fun a ->
      int >>= fun b ->
        sized choose_int0 >>= fun c ->
          ret_gen
            (float a +. (float b /. (float c +. 1.)))
  let pair a1 a2 =
    a1 >>= fun v1 ->
      a2 >>= fun v2 ->
        ret_gen (v1,v2)
  let triple a1 a2 a3 =
    a1 >>= fun v1 ->
      a2 >>= fun v2 ->
        a3 >>= fun v3 ->
          ret_gen (v1,v2,v3)
  let list e =
    sized choose_int0 >>= vector e
end

(*********** testable ************)

type result = {
  ok : bool option;
  stamp : string list;
  arguments : pretty_str list;
}

let nothing : result = {ok=None; stamp=[]; arguments=[]}

type property = Prop of result gen

let result : result -> property =
  fun res -> Prop (ret_gen res)

let evaluate : ('a -> property) -> 'a -> result gen =
  fun property a ->
    let Prop gen = property a in
    gen

let forAll : ('a -> pretty_str) -> ('b -> property) ->
  'a gen -> ('a -> 'b) -> property =
  fun show_a property_b gen body ->
    let argument a res =
      { res with arguments = show_a a ::res.arguments }
    in
    Prop
      (gen >>= fun a ->
         evaluate property_b (body a) >>= fun res ->
           ret_gen (argument a res))

module Testable = struct
  type 'a t = 'a -> property
  let unit _ = result nothing
  let bool b = result {nothing with ok=Some b}
  let result r = result r
  let property p = p
  let f : 'a gen -> ('a -> pretty_str) -> ('b -> property) ->
    ('a -> 'b) -> property =
    fun arbitrary_a show_a property_b f ->
      forAll show_a property_b arbitrary_a f
end

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
    Format.printf "%s %d" mesg ntest

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
