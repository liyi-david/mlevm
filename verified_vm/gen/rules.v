Require Coq.extraction.Extraction.

Require Import List.

Extract Inductive list => list [ "[]" "List.cons" ].

Extract Constant nth    => "fun n l default -> try List.nth l n with _ -> default".
Extract Constant push   => "fun l a -> a :: l".
Extract Constant pop    => "fun l -> match l with a :: l -> (l, Some a) | _ -> (l, None)".
