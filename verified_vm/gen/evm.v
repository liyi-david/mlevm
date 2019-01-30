Require Extraction.
Require ExtrOcamlNatInt.

Require Import List.

Open Scope type_scope.

Definition push {T:Type} (s: list T) a := a :: s.

Definition pop {T:Type} (s: list T) : list T * option T :=
    match s with
    | a :: s' => (s', Some a)
    | _ => (s, None)
    end.

Inductive error : Set :=
    | OutOfBounds
    | OutOfGas
    | OutOfStack
    | OutOfCode
    | OutOfData
    | InvalidOpcode
.

Inductive return_type : Set :=
    | Normal : nat -> return_type
    | Create : nat -> return_type
    | Revert : return_type
.

Inductive vm_status : Set :=
    | Running : vm_status
    | Error   : error -> vm_status
    | Finish  : return_type -> vm_status
.

Inductive opcode : Set :=
    | PUSH    : nat -> opcode
    | POP     : opcode
    | INVALID : nat -> opcode
    | STOP    : opcode
    .

Definition gas_of op : nat :=
    let raw_gas := match op with
    | PUSH _    => 3
    | POP       => 2
    | _         => 1
    end
    in
    (raw_gas - 1)
.

Record state := mkState {
    stack   : list nat;
    opcodes : list opcode;
    pc      : nat;
    status  : vm_status;
}.

Definition get_opcode st :=
    match st with
    | mkState _ ops pc _ => nth pc ops (INVALID 0)
    end.

Definition interp : state -> state := fun st =>
    let op := get_opcode st in
    match op with
    | PUSH a        => mkState (push st.(stack) a) st.(opcodes) (1 + st.(pc)) st.(status)
    | POP           => let (s', a) := (pop st.(stack)) in
                       match a with
                       | Some _  => mkState s' st.(opcodes) (1 + st.(pc)) st.(status)
                       | None    => mkState s' st.(opcodes) (1 + st.(pc)) (Error OutOfStack)
                       end
    | STOP          => mkState st.(stack) st.(opcodes) st.(pc) (Finish (Normal 0))
    | INVALID _     => mkState st.(stack) st.(opcodes) st.(pc) (Error InvalidOpcode)
    end.

Fixpoint iterate (gas:nat) (s:state) {struct gas} : state :=
    let op := get_opcode s in
    match gas with
    | O => s
    | S gas =>
        let s' := interp s in
        match s'.(status) with
        | Running => iterate (gas - (gas_of op)) s'
        | _ => s'
        end
    end.

Definition run (ops: list opcode) (gas: nat) : vm_status :=
    let init := mkState nil ops 0 Running in
    let result_state := iterate gas init in
    result_state.(status).

(* generate code *)
Extraction "evm.ml" run.
