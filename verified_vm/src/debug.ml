open Evm;;
open List;;
open Printf;;
open Big_int;;

let tty_activated = ref false;;

let tty_activate () =
    tty_activated := true
;;

let tty_inactivate () =
    tty_activated := false
;;

let tty_print s =
    if !tty_activated then
        let tty = open_out "/dev/stdout" in
        Printf.fprintf tty "%s" s;
        close_out tty
    else
        ()
;;

let tty_println s = tty_print (s ^ "\n");;

let pp_opcode op = 
    match op with
    | STOP          -> "STOP"
    | ADD           -> sprintf "ADD"
    | MUL           -> sprintf "MUL"
    | SUB           -> sprintf "SUB"
    | DIV           -> sprintf "DIV"
    | SDIV          -> sprintf "SDIV"
    | MOD           -> sprintf "MOD"
    | SMOD          -> sprintf "SMOD"
    | ADDMOD        -> sprintf "ADDMOD"
    | MULMOD        -> sprintf "MULMOD"
    | EXP           -> sprintf "EXP"
    | SIGNEXTEND    -> sprintf "SIGNEXTEND"
    | LT            -> sprintf "LT"
    | GT            -> sprintf "GT"
    | SLT           -> sprintf "SLT"
    | SGT           -> sprintf "SGT"
    | EQ            -> sprintf "EQ"
    | ISZERO        -> sprintf "ISZERO"
    | CALLVALUE     -> sprintf "CALLVALUE"
    | CALLDATALOAD  -> sprintf "CALLDATALOAD"
    | MLOAD         -> sprintf "MLOAD"
    | MSTORE        -> sprintf "MSTORE"
    | MSTORE8       -> sprintf "MSTORE8"
    | SLOAD         -> sprintf "SLOAD"
    | SSTORE        -> sprintf "SSTORE"
    | JUMP          -> sprintf "JUMP"
    | JUMPI         -> sprintf "JUMPI"
    | PC            -> sprintf "PC"
    | MSIZE         -> sprintf "MSIZE"
    | GAS           -> sprintf "GAS"
    | JUMPDEST      -> sprintf "JUMPDEST"
    | RETURN        -> sprintf "RETURN"
    | POP           -> sprintf "POP"
    | PUSH a        -> sprintf "PUSH %s" (string_of_big_int a)
    | DUP a         -> sprintf "DUP %d" a
    | SWAP a        -> sprintf "SWAP %d" a
    | INVALID c     -> sprintf "INVALID %2X" c
    | _             -> "UNHANDLED"
;;

let tty_print_opcodes (ops : opcode list) =
    iter begin fun op ->
        tty_print (pp_opcode op); tty_print "\t"
    end ops
;;

let tty_print_stack (s: big_int list) =
    for i = 0 to (min 8 (List.length s)) - 1 do
        tty_print " | ";
        tty_print (string_of_big_int (List.nth s i));
    done;
    tty_println ""
;;

let tty_print_state (s: Evm.state) =
    tty_println "STATE: ";
    tty_print "> stack : ";
    tty_print_stack s.stack;
    tty_print "> next  :  ";
    tty_print ("(" ^ (string_of_int s.pc) ^ ") ");
    tty_println (pp_opcode (List.nth s.opcodes s.pc));

    s
;;
