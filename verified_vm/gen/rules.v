Require Coq.extraction.Extraction.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlNatInt.

Require Import Arith.
Require Import List.
Require Import Nat.

Definition u256 := nat.
Definition u256_add := add.
Definition u256_sub := sub.
Definition u256_mul := mul.
Definition u256_div := div.
Definition u256_eqb := eqb.
Definition u256_pow := pow.

Definition u256_lt  := ltb.

Definition u256_of_nat (a:nat) := a.

Definition u2560 := u256_of_nat 0.
Definition u2561 := u256_of_nat 1.

Extract Constant u256       => "big_int".

<<<<<<< HEAD
Extract Constant u256_add   => "fun a b -> (mod_big_int (add_big_int a b) (power_int_positive_int 2 256))".
Extract Constant u256_sub   => "sub_big_int".
Extract Constant u256_mul   => "fun a b -> (mod_big_int (mult_big_int a b) (power_int_positive_int 2 256))".
Extract Constant u256_div   => "div_big_int".
Extract Constant u256_pow   => "fun a b -> let rec p b = if (eq_big_int b (big_int_of_int 0)) then (big_int_of_int 1) else u256_mul (p (sub_big_int b (big_int_of_int 1))) a in p b".
=======
Extract Constant u256_add   => "add_big_int".
Extract Constant u256_sub   => "sub_big_int".
Extract Constant u256_mul   => "mult_big_int".
Extract Constant u256_div   => "div_big_int".
Extract Constant u256_pow   => "power_big_int_positive_big_int".
>>>>>>> 5e91c5dbb29c35c46e5b54d8b766dd011eb6ccf5

Extract Constant u256_lt    => "lt_big_int".

Extract Constant u256_of_nat=> "big_int_of_int".

Extract Constant u256_eqb   => "eq_big_int".

Extract Inductive list      => list [ "[]" "List.cons" ].

Extract Constant pow        => "let rec f a b = match b with 0 -> 1 | _ -> a * (f a (b - 1)) in f".

Extract Constant nth        => "fun n l default -> try List.nth l n with _ -> default".
Extract Constant nth_error  => "fun l n -> try Some (List.nth l n) with _ -> None".
Extract Constant push       => "fun l a -> a :: l".
Extract Constant pop        => "fun l -> match l with a :: l -> (l, Some a) | _ -> (l, None)".

<<<<<<< HEAD
Extract Constant swap       => "fun l i -> if (List.length l <= i) then None else Some (List.fold_left begin fun ln e -> if (List.length ln == i) then ln @ [List.hd l] else ln @ [e] end [List.nth l i] (List.tl l))".
=======
Extract Constant swap       => "fun l i -> try Some ((List.nth l i) :: l) with _ -> None".
>>>>>>> 5e91c5dbb29c35c46e5b54d8b766dd011eb6ccf5
