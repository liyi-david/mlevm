open Evm;;
open List;;
open Printf;;

let tty_activated = ref true;;

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

let tty_print_opcodes (ops : opcode list) =
    iter begin fun op ->
        let str_op = match op with
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
        | MSTORE        -> sprintf "MSTORE"
        | MSTORE8       -> sprintf "MSTORE8"
        | POP           -> sprintf "POP"
        | PUSH a        -> sprintf "PUSH %d" a
        | INVALID c     -> sprintf "INVALID %2X" c
        | _             -> "UNHANDLED"
        in
        tty_print str_op; tty_print "\t"
    end ops
;;
