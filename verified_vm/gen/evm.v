Require Extraction.
Require ExtrOcamlNatInt.

Require Import List.

Open Scope type_scope.

Definition stack := list nat.

Inductive return_type : Set :=
    | Value   : nat -> return_type
    | Failure : return_type
    | None    : return_type
.

Record state := mkState {
    s   : stack;
    pc  : nat;
    ret : return_type;
}.

Inductive opcode : Set :=
    | PUSH : nat -> opcode
    | POP  : opcode.

Definition interp (op: opcode): state -> state := fun st =>
    match op with
    | PUSH a => mkState (a :: st.(s)) (1 + st.(pc)) st.(ret)
    | POP    => match st.(s) with
                | cons t s' => mkState s' (1 + st.(pc)) st.(ret)
                | nil => mkState st.(s) (1 + st.(pc)) Failure
                end
    end.

Definition run (ops: list opcode) : nat := 0.

(* generate code *)
Extraction "evm.ml" run.
