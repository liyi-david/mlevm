Require Extraction.
Require ExtrOcamlNatInt.

Require Import List.
Require Import Arith.

Open Scope type_scope.
Open Scope list_scope.

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
    | STOP          : opcode
    | ADD           : opcode
    | MUL           : opcode
    | SUB           : opcode
    | DIV           : opcode
    | SDIV          : opcode
    | MOD           : opcode
    | SMOD          : opcode
    | ADDMOD        : opcode
    | MULMOD        : opcode
    | EXP           : opcode
    | SIGNEXTEND    : opcode
    | LT            : opcode
    | GT            : opcode
    | SLT           : opcode
    | SGT           : opcode
    | EQ            : opcode
    | ISZERO        : opcode
    | CALLVALUE     : opcode
    | CALLDATALOAD  : opcode
    | MLOAD         : opcode
    | MSTORE        : opcode
    | MSTORE8       : opcode
    | SLOAD         : opcode
    | SSTORE        : opcode
    | JUMP          : opcode
    | JUMPI         : opcode
    | PC            : opcode
    | MSIZE         : opcode
    | GAS           : opcode
    | JUMPDEST      : opcode
    | RETURN        : opcode
    | POP           : opcode
    | PUSH          : nat -> opcode
    | DUP           : nat -> opcode
    | SWAP          : nat -> opcode
    | INVALID       : nat -> opcode
    (* TODO lots of things *)
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

(* avoid ambiguration on operators *)
Close Scope type_scope.

Definition interp : state -> state := fun st =>
    let op := get_opcode st in
    match op with
    | STOP          => mkState st.(stack) st.(opcodes) st.(pc) (Finish (Normal 0))
    | ADD           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       match a, b with
                       | Some a', Some b' => mkState (push s'' (a' + b')) st.(opcodes) (1 + st.(pc)) st.(status)
                       | _, _ => mkState s'' st.(opcodes) (1 + st.(pc)) (Error OutOfStack)
                       end
    | MUL           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       match a, b with
                       | Some a', Some b' => mkState (push s'' (a' * b')) st.(opcodes) (1 + st.(pc)) st.(status)
                       | _, _ => mkState s'' st.(opcodes) (1 + st.(pc)) (Error OutOfStack)
                       end
    | SUB           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       match a, b with
                       | Some a', Some b' => mkState (push s'' (a' * b')) st.(opcodes) (1 + st.(pc)) st.(status)
                       | _, _ => mkState s'' st.(opcodes) (1 + st.(pc)) (Error OutOfStack)
                       end
    | DIV           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       match a, b with
                       | Some a', Some b' => mkState (push s'' (a' / b')) st.(opcodes) (1 + st.(pc)) st.(status)
                       | _, _ => mkState s'' st.(opcodes) (1 + st.(pc)) (Error OutOfStack)
                       end
    | PUSH a        => mkState (push st.(stack) a) st.(opcodes) (1 + st.(pc)) st.(status)
    | POP           => let (s', a) := (pop st.(stack)) in
                       match a with
                       | Some _  => mkState s' st.(opcodes) (1 + st.(pc)) st.(status)
                       | None    => mkState s' st.(opcodes) (1 + st.(pc)) (Error OutOfStack)
                       end
    | INVALID _     => mkState st.(stack) st.(opcodes) st.(pc) (Error InvalidOpcode)
    | _             => mkState st.(stack) st.(opcodes) st.(pc) (Error InvalidOpcode)
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

(* from here we start working on the theorems *)

Open Scope type_scope.

(* 1. proofs of correctness of contract code *)
Inductive is_successor s s' : Prop :=
    | DirectSuccessor   : (interp s = s' /\ s.(status) = Running) -> is_successor s s'
    | IndirectSuccessor : (s.(status) = Running /\ exists s'', interp s = s'' /\ is_successor s'' s') -> is_successor s s'
.

(* 2. proofs of optimization *)
Variable is_equivalent : list opcode -> list opcode -> Prop.

(* op1; op2 ...; PUSH a; POP; sop1; ... sopn *)

Theorem trivial_opt_push_pop:
    forall opprefix opsuffix a,
        is_equivalent
            (opprefix ++ opsuffix)
            (opprefix ++ (PUSH a) :: POP :: opsuffix)
.
Proof.
Admitted.


(* generate code *)
Local Load rules.

Extraction "evm.ml" run.
