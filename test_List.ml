open QuickCheck

let prop_revrev : 'a list -> bool =
  fun xs -> List.rev (List.rev xs) = xs

module AL =
  Arbitrary_list(Arbitrary_int)
module SL =
  PShow_list(PShow_int)
module Testable_list_to_bool =
  Testable_fun
    (AL)
    (SL)
    (Testable_bool)
module C = Check(Testable_list_to_bool)
let () = C.quickCheck prop_revrev
