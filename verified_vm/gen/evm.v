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

Inductive opcode : Set :=
    | PUSH    : nat -> opcode
    | POP     : opcode
    | INVALID : opcode
    .

Record state := mkState {
    s       : stack;
    opcodes : list opcode;
    pc      : nat;
    ret     : return_type;
}.

Definition get_opcode st :=
    match st with
    | mkState _ ops pc _ => nth pc ops INVALID
    end.

Definition interp : state -> state := fun st =>
    let op := get_opcode st in
    match op with
    | PUSH a => mkState (a :: st.(s)) st.(opcodes) (1 + st.(pc)) st.(ret)
    | POP    => match st.(s) with
                | cons t s' => mkState s' st.(opcodes) (1 + st.(pc)) st.(ret)
                | nil => mkState st.(s) st.(opcodes) (1 + st.(pc)) Failure
                end
    | INVALID => mkState st.(s) st.(opcodes) st.(pc) Failure
    end.

Fixpoint iterate (gas:nat) (s:state) {struct gas} : state :=
    match gas with
    | O => s
    | S gas' =>
        let s' := interp s in
        match s'.(ret) with
        | None => iterate gas' s'
        | _ => s'
        end
    end.

Definition run (ops: list opcode) (gas: nat) : nat :=
    let init := mkState nil ops 0 None in
    let result_state := iterate gas init in
    0.

(* generate code *)
Extraction "evm.ml" run.
