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

let prop_revrev : char list -> bool =
  fun xs -> List.rev (List.rev xs) = xs

module Arbitrary_char_list =
  Arbitrary_list(Arbitrary_char)
module PShow_char_list =
  PShow_list(PShow_char)
module Testable_char_list_to_bool =
  Testable_fun
    (Arbitrary_char_list)
    (PShow_char_list)
    (Testable_bool)
module C = Check(Testable_char_list_to_bool)
let () = C.verboseCheck prop_revrev
