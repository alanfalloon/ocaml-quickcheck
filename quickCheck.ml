module Random = struct
  include Random
  let int n = int (max n 1)
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

module List = struct
  include List
  let rec span : ('a -> bool) -> 'a list -> 'a list * 'a list =
    fun p -> function
        [] -> [],[]
      | x::xs when p x ->
          let ys,zs = span p xs in
          (x::ys,zs)
      | xs -> [],xs

  let rec groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list =
    fun p -> function
        [] -> []
      | x::xs ->
          let ys,zs = span (p x) xs in
          (x::ys) :: groupBy p zs

  let group xs = groupBy (=) xs
end

type 'a gen = Gen of (int -> 'a)
type pretty_str = Format.formatter -> unit -> unit

module type PSHOW = sig
  type t
  val show : t -> pretty_str
end

module type SHOW = sig
  type t
  val show : t -> string
end

module Show(P:PSHOW) = struct
  open Buffer
  open Format
  type t = P.t
  let show : t -> string =
    fun x ->
      let f _ =
        let str = contents stdbuf in
        clear stdbuf;
        str
      in
      clear stdbuf;
      kfprintf f str_formatter "@[%a@]@?" (P.show x) ()
end

module PShow_list(Elt:PSHOW) = struct
  type t = Elt.t list
  let show : t -> pretty_str =
    fun xs fmt () ->
      let pp = Format.fprintf in
      match List.map Elt.show xs with
          [] -> pp fmt "[]"
        | a1::an ->
            let pprest f =
              List.iter (fun e -> pp f ";@ %a" e ())
            in
	    pp fmt "[%a%a]" a1 () pprest an
end

module PShow_char = struct
  type t = char
  let show : t -> pretty_str =
    fun c fmt () ->
      Format.fprintf fmt "%C" c
end

module PShow_int = struct
  type t = int
  let show : t -> pretty_str =
    fun c fmt () ->
      Format.fprintf fmt "%d" c
end

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

module type ARBITRARY = sig
  type t
  val arbitrary : t gen
end

module Arbitrary_unit = struct
  type t = unit
  let arbitrary = ret_gen ()
end

module Arbitrary_bool = struct
  type t = bool
  let arbitrary = elements [true; false]
end

module Arbitrary_char = struct
  type t = char
  let arbitrary =
    choose_int (32,255) >>= fun c -> 
      ret_gen (Char.chr c)
end

module Arbitrary_int = struct
  type t = int
  let arbitrary = sized (fun n -> choose_int (-n, n))
end

module Arbitrary_float = struct
  type t = float
  let arbitrary =
    Arbitrary_int.arbitrary >>= fun a ->
      Arbitrary_int.arbitrary >>= fun b ->
        sized choose_int0 >>= fun c ->
          ret_gen
            (float a +. (float b /. (float c +. 1.)))
end

module Aribitrary_pair(Fst:ARBITRARY)(Snd:ARBITRARY) = struct
  type t = Fst.t * Snd.t
  let arbitrary =
    Fst.arbitrary >>= fun v1 ->
      Snd.arbitrary >>= fun v2 ->
        ret_gen (v1,v2)
end

module Aribitrary_triple(Fst:ARBITRARY)(Snd:ARBITRARY)(Trd:ARBITRARY) = struct
  type t = Fst.t * Snd.t * Trd.t
  let arbitrary =
    Fst.arbitrary >>= fun v1 ->
      Snd.arbitrary >>= fun v2 ->
        Trd.arbitrary >>= fun v3 ->
          ret_gen (v1,v2,v3)
end

module Arbitrary_list(Elt:ARBITRARY) = struct
  type t = Elt.t list
  let arbitrary =
    sized choose_int0 >>= vector Elt.arbitrary
end

(*********** testable ************)

type result = {
  ok : bool option;
  stamp : string list;
  arguments : pretty_str list;
}

type property = Prop of result gen

module type TESTABLE = sig
  type t
  val property : t -> property
end

let nothing : result = {ok=None; stamp=[]; arguments=[]}

let result : result -> property =
  fun res -> Prop (ret_gen res)

module Testable_unit = struct
  type t = unit
  let property () = result nothing
end

module Testable_bool = struct
  type t = bool
  let property b = result {nothing with ok=Some b}
end

module Testable_result = struct
  type t = result
  let property r = result r
end

module Testable_property = struct
  type t = property
  let property p = p
end

module Evaluate(T:TESTABLE) = struct
  let evaluate : T.t -> result gen =
    fun a ->
      let Prop gen = T.property a in
      gen
end

module ForAll(S:PSHOW)(T:TESTABLE) = struct
  module E = Evaluate(T)
  let forAll : S.t gen -> (S.t -> T.t) -> property =
  fun gen body ->
    let argument a res =
      { res with arguments = S.show a ::res.arguments }
    in
    Prop
      (gen >>= fun a ->
         E.evaluate (body a) >>= fun res ->
           ret_gen (argument a res))
end

module Testable_fun
  (A:ARBITRARY)
  (S:PSHOW with type t = A.t)
  (T:TESTABLE) =
struct
  module F = ForAll(S)(T)
  type t = A.t -> T.t
  let property : t -> property =
    fun f ->
      F.forAll A.arbitrary f
end

module Implies(T:TESTABLE) = struct
  let (==>) : bool -> T.t -> property =
    fun b a ->
      if b
      then T.property a
      else Testable_unit.property ()
end

module Label(T:TESTABLE) = struct
  module E = Evaluate(T)
  let label : string -> T.t -> property =
    fun s a ->
      let add r = {r with stamp = s :: r.stamp } in
      let a' = E.evaluate a in
      Prop (map_gen add a')
end

module Classify(T:TESTABLE) = struct
  module L = Label(T)
  let classify : bool -> string -> T.t -> property =
    function
        true -> L.label
      | false -> fun _ -> T.property
  let trivial : bool -> T.t -> property =
    fun b -> classify b "trivial"
end

module Collect(S:SHOW)(T:TESTABLE) = struct
  module L = Label(T)
  let collect : S.t -> T.t -> property =
    fun v -> L.label (S.show v)
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
  every   = (fun _ (_, _) -> ()) 
}

let verbose = {
  quick with
    every = begin fun f (n, args) ->
      let pargs fmt l =
        List.iter (fun a -> Format.fprintf fmt "@ %a" a ()) l
      in
      Format.fprintf f "@[%d:@[<hov 2>%a@]@]@." n pargs args
    end
}

let done_ : string -> int -> string list list -> unit =
  fun mesg ntest stamps ->
    let percentage n m =
      Format.sprintf "%2d%%" ((100 * n) / m)
    in
    let entry (n, xs) =
      Format.sprintf "%s %s" (percentage n ntest) (String.concat ", " xs)
    in
    let pairLength = function
        (xs::_) as xss -> (List.length xss, xs)
      | [] -> assert false
    in
    let display = function
        [] -> ".\n"
      | [x] -> Format.sprintf " (%s).\n" x
      | xs ->
          String.concat "\n" ("." :: List.map (Format.sprintf "%s.") xs)
    in
    let not_null = function [] -> false | _ -> true in
    let table =
      display
        (List.map entry
           (List.rev
              (List.sort compare
                 (List.map pairLength
                    (List.group
                       (List.sort compare
                          (List.filter not_null
                             stamps)))))))
    in
    Format.printf "%s %d tests%s" mesg ntest table

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

module Check(T:TESTABLE) = struct
  module E=Evaluate(T)
  let check : config -> T.t -> unit =
    fun config a ->
      tests config (E.evaluate a) 0 0 []
  let test = check quick
  let quickCheck = test
  let verboseCheck = check verbose
end

(* (set (make-local-variable 'flymake-ocaml-build-file) "Makefile") *)
