open QuickCheck

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

let prop_revrev : 'a list -> bool =
  fun xs -> List.rev (List.rev xs) = xs

module AL =
  Arbitrary_list(Arbitrary_int)
module SL =
  PShow_list(PShow_int)
module Testable_char_list_to_bool =
  Testable_fun
    (AL)
    (SL)
    (Testable_bool)
module C = Check(Testable_char_list_to_bool)
let () = C.quickCheck prop_revrev
