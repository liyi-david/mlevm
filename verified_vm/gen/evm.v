Require Extraction.

Require Import List.
Require Import Arith.
Require Import Nat.

Open Scope type_scope.
Open Scope list_scope.
Open Scope nat_scope.


(* STACK *)

Definition push {T:Type} (s: list T) a := a :: s.

Definition pop {T:Type} (s: list T) : list T * option T :=
    match s with
    | a :: s' => (s', Some a)
    | _ => (s, None)
    end.

Variable swap : forall {T:Type}, list T -> nat -> option (list T).

(* generate code *)
Local Load rules.


Inductive error : Set :=
    | OutOfBounds
    | OutOfGas
    | OutOfStack
    | OutOfCode
    | OutOfData
    | InvalidOpcode
.

(* MEMORY *)
Inductive memory_value : Set :=
    | Item8 : u256 -> memory_value
    | Item256 : u256 -> memory_value
.

Definition memory : Type := u256 -> option memory_value.

Definition empty_mem : memory := fun _ => None.

Definition update_mem_val (m:memory) (index:u256) (val:option memory_value) : memory :=
    fun i => match (u256_eqb i index) with 
    | true => val
    | false => (m i)
    end.

Inductive return_type : Set :=
    | Normal : u256 -> return_type
    | Create : u256 -> return_type
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
    | PUSH          : u256 -> opcode
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
    stack   : list u256;
    mem     : memory;

    (* slightly different from the original form *)
    opcodes : list opcode;
    jumpmap : u256 -> nat;    

    pc      : nat;
    status  : vm_status;
}.

Definition update_stack  s st := mkState st s.(mem) s.(opcodes) s.(jumpmap) s.(pc) s.(status).
Definition update_mem    s m  := mkState s.(stack) m s.(opcodes) s.(jumpmap) s.(pc) s.(status).
Definition update_pc     s i  := mkState s.(stack) s.(mem) s.(opcodes) s.(jumpmap) i s.(status).
Definition update_status s st := mkState s.(stack) s.(mem) s.(opcodes) s.(jumpmap) s.(pc) st.

Definition inc_pc        s    := update_pc s (s.(pc) + 1).

Definition get_opcode st :=
    match st with
    | mkState _ _ ops _ pc _ => nth pc ops (INVALID O)
    end.

(* avoid ambiguration on operators *)
Close Scope type_scope.

Definition interp : state -> state := fun st =>
    let op := get_opcode st in
    match op with
    | STOP          => update_status st (Finish (Normal (u256_of_nat 0)))
    | ADD           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       inc_pc
                       match a, b with
                       | Some a', Some b' => update_stack st (push s'' (u256_add a' b'))
                       | _, _ => update_status st (Error OutOfStack)
                       end
    | MUL           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       inc_pc
                       match a, b with
                       | Some a', Some b' => update_stack st (push s'' (u256_mul a' b'))
                       | _, _ => update_status st (Error OutOfStack)
                       end
    | SUB           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       inc_pc
                       match a, b with
                       | Some a', Some b' => update_stack st (push s'' (u256_sub a' b'))
                       | _, _ => update_status st (Error OutOfStack)
                       end
    | DIV           => let (s', a)  := (pop st.(stack)) in
                       let (s'', b) := (pop s') in
                       inc_pc
                       match a, b with
                       | Some a', Some b' => update_stack st (push s'' (u256_div a' b'))
                       | _, _ => update_status st (Error OutOfStack)
                       end
    | EXP           =>  let (s', a)   := (pop st.(stack)) in
                        let (s'', b) := (pop s') in
                        inc_pc
                        match a, b with
                        | Some a', Some b' => (update_stack st (push s'' (u256_pow a' b')))
                        | _, _ => update_status st (Error OutOfStack)
                        end
    | EQ            =>  let (s', a)  := (pop st.(stack)) in
                        let (s'', b) := (pop s') in
                        inc_pc
                        match a, b with
                        | Some a', Some b' => update_stack st (push s'' (if u256_eqb a' b' then (u256_of_nat 1) else (u256_of_nat 0)))
                        | _, _ => update_status st (Error OutOfStack)
                        end
    | ISZERO        =>  let (s', _a) := (pop st.(stack)) in
                        inc_pc
                        match _a with
                        | Some a  => update_stack st (push s' (if (u256_eqb a (u256_of_nat 0)) then (u256_of_nat 1) else (u256_of_nat 0)))
                        | None    => update_status st (Error OutOfStack)
                        end
                        (* FIXME *)
    | LT            =>  let (s', a)  := (pop st.(stack)) in
                        let (s'', b) := (pop s') in
                        inc_pc
                        match a, b with
                        | Some a', Some b' => update_stack st (push s'' (if u256_lt a' b' then (u256_of_nat 1) else (u256_of_nat 0)))
                        | _, _ => update_status st (Error OutOfStack)
                        end
    | CALLVALUE     =>  inc_pc (update_stack st (push st.(stack) u2560))
    | CALLDATALOAD  =>  let (s', _offset) := (pop st.(stack)) in
                        inc_pc
                        match _offset with
                            (* FIXME *)
                        | Some offset   => update_stack st (push s' (u256_of_nat 1))
                        | None          => update_status st (Error OutOfStack)
                        end
    | POP           =>  let (s', a) := (pop st.(stack)) in
                        inc_pc
                        match a with
                        | Some _  => update_stack st s'
                        | None    => update_status st (Error OutOfStack)
                        end
    | JUMP          =>  let (s', _dest) := (pop st.(stack)) in
                        match _dest with
                        | Some dest => update_pc (update_stack st s') (st.(jumpmap) dest)
                        | None      => update_status st (Error OutOfStack)
                        end
    | JUMPI         =>  let (s', _dest) := (pop st.(stack)) in
                        let (s'', _cond) := (pop s') in
                        match _dest, _cond with
                        | Some dest, Some cond =>
                            if (u256_eqb cond u2560) then
                                inc_pc (update_stack st s'')
                            else
                                update_pc st (st.(jumpmap) dest)
                        | _, _ => update_status st (Error OutOfStack)
                        end
    | JUMPDEST      =>  inc_pc st
    | PUSH a        =>  inc_pc (update_stack st (push st.(stack) a))
    | DUP i         =>  let _a := (nth_error st.(stack) (i - 1)) in
                        inc_pc
                        match _a with
                        | Some a  => update_stack st (push st.(stack) a)
                        | None    => update_status st (Error OutOfStack)
                        end
    | SWAP i        =>  let _s := swap st.(stack) i in
                        inc_pc
                        match _s with
                        | Some s => update_stack st s
                        | None => update_status st (Error OutOfStack)
                        end
    | MSTORE        =>  let (s', _offset) := (pop st.(stack)) in
                        let (s'', _val) := (pop s') in
                        inc_pc
                        match _offset, _val with
                        | Some offset, Some val =>
                            let m' := update_mem_val st.(mem) offset (Some (Item256 val)) in 
                            update_mem (update_stack st s'') m'
                        | _, _ => update_status st (Error OutOfStack)
                        end
    | MSTORE8       =>  let (s', _offset) := (pop st.(stack)) in
                        let (s'', _val) := (pop s') in
                        inc_pc
                        match _offset, _val with
                        | Some offset, Some val =>
                            let m' := update_mem_val st.(mem) offset (Some (Item8 val)) in 
                            update_mem (update_stack st s'') m'
                        | _, _ => update_status st (Error OutOfStack)
                        end
    | _                 => update_status st (Error InvalidOpcode)
    end.

Fixpoint iterate (gas:nat) (_s:state) (hook:state -> state) {struct gas} : state :=
    let s := hook _s in
    let op := get_opcode s in
    match gas with
    | O => update_status s (Error OutOfGas)
    | S gas =>
        let s' := interp s in
        match s'.(status) with
        | Running => iterate (gas - (gas_of op)) s' hook
        | _ => s'
        end
    end.

Definition run (ops: list opcode) (gas: nat) (jmap: u256 -> nat) (hook: state -> state): vm_status :=
    let init := mkState nil empty_mem ops jmap 0 Running in
    let result_state := iterate gas init hook in
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


Extraction "evm.ml" run.
