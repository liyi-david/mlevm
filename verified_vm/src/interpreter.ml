open Big_int;;
open Bytes;;
open Ctypes;;
open Debug;;

let u256_from_ptr (p: char ptr) (len: int): big_int =
    let res = ref (big_int_of_int 0) in
    for i = 0 to len - 1 do
        res := mult_int_big_int 256 !res;
        res := add_int_big_int (int_of_char (!@ (p +@ i))) !res;
    done;
    !res
;;

module BigInt = struct
    type t = big_int
    let compare = compare_big_int
end;;

module JumpMap = Map.Make(BigInt);;

let preprocess_byte_list (raw_ops: char ptr) (len: int) : int list =
    let ops = ref [] in
    for i = len - 1 downto 0 do
        ops := (int_of_char !@(raw_ops +@ i)) :: !ops;
    done;
    !ops
;;

let preprocess (raw_ops: char ptr) (len: int): Evm.opcode list * (int JumpMap.t) =
    let jmap = ref JumpMap.empty in
    let full_len = len in

    let rec preprocess (raw_ops: char ptr) (len: int) (depth: int) : Evm.opcode list =
        (* TODO what happens if preprocessing failed? *)
<<<<<<< HEAD
=======
        (*
>>>>>>> 5e91c5dbb29c35c46e5b54d8b766dd011eb6ccf5
        tty_print "map from ";
        tty_print (string_of_int (full_len - len));
        tty_print " to ";
        tty_println (string_of_int depth);
<<<<<<< HEAD
=======
        *)
>>>>>>> 5e91c5dbb29c35c46e5b54d8b766dd011eb6ccf5
        jmap := JumpMap.add (big_int_of_int (full_len - len)) depth !jmap; 
        if len == 0 then [] else begin
            let curr = int_of_char (!@ raw_ops) in
            match curr with
            | 0x00 -> Evm.STOP          :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x01 -> Evm.ADD           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x02 -> Evm.MUL           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x03 -> Evm.SUB           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x04 -> Evm.DIV           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x05 -> Evm.SDIV          :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x06 -> Evm.MOD           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x07 -> Evm.SMOD          :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x08 -> Evm.ADDMOD        :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x09 -> Evm.MULMOD        :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x0a -> Evm.EXP           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x0b -> Evm.SIGNEXTEND    :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x10 -> Evm.LT            :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x11 -> Evm.GT            :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x12 -> Evm.SLT           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x13 -> Evm.SGT           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x14 -> Evm.EQ            :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x15 -> Evm.ISZERO        :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            (*
            AND = 0x17,
            OR = 0x17,
            XOR = 0x18,
            NOT = 0x19,
            BYTE = 0x1a,
            SHL = 0x1b,
            SHR = 0x1c,
            SAR = 0x1d,
            SHA3 = 0x20,
            ADDRESS = 0x30,
            BALANCE = 0x31,
            ORIGIN = 0x32,
            CALLER = 0x33,
            *)
            | 0x34 -> Evm.CALLVALUE     :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x35 -> Evm.CALLDATALOAD  :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            (*
            CALLDATASIZE = 0x36,
            CALLDATACOPY = 0x37,
            CODESIZE = 0x38,
            CODECOPY = 0x39,
            GASPRICE = 0x3a,
            EXTCODESIZE = 0x3b,
            EXTCODECOPY = 0x3c,
            RETURNDATASIZE = 0x3d,
            RETURNDATACOPY = 0x3e,
            EXTCODEHASH = 0x3f,
            BLOCKHASH = 0x40,
            COINBASE = 0x41,
            TIMESTAMP = 0x42,
            NUMBER = 0x43,
            DIFFICULTY = 0x44,
            GASLIMIT = 0x45,
            *)
            | 0x50 -> Evm.POP           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x51 -> Evm.MLOAD         :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x52 -> Evm.MSTORE        :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x53 -> Evm.MSTORE8       :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x54 -> Evm.SLOAD         :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x55 -> Evm.SSTORE        :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x56 -> Evm.JUMP          :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x57 -> Evm.JUMPI         :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x58 -> Evm.PC            :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x59 -> Evm.MSIZE         :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x5a -> Evm.GAS           :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            | 0x5b -> Evm.JUMPDEST      :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            (*
            LOG0 = 0xa0,
            LOG1 = 0xa1,
            LOG2 = 0xa2,
            LOG3 = 0xa3,
            LOG4 = 0xa4,
            CREATE = 0xf0,
            CALL = 0xf1,
            CALLCODE = 0xf2,
            *)
            | 0xf3 -> Evm.RETURN        :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            (*
            DELEGATECALL = 0xf4,
            CREATE2 = 0xf5,
            REVERT = 0xfd,
            STATICCALL = 0xfa,
            SELFDESTRUCT = 0xff,
            *)
            | _ -> begin
                (* PUSH *)
                if 0x60 <= curr && curr <= 0x7f then
                    let wbits = curr - 0x60 + 1 in
                    let a = (u256_from_ptr (raw_ops +@ 1) wbits) in
                    (Evm.PUSH a) :: (preprocess (raw_ops +@ (wbits + 1)) (len - wbits - 1)) (depth + 1)
                else if 0x80 <= curr && curr <= 0x8f then
                    let windex = (curr - 0x80) + 1 in
                    (Evm.DUP windex) :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
                else if 0x90 <= curr && curr <= 0x9f then
                    let windex = (curr - 0x90) + 1 in
                    (Evm.SWAP windex) :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
                else
                    (Evm.INVALID curr) :: (preprocess (raw_ops +@ 1) (len - 1)) (depth + 1)
            end
        end
    in
    let ops = preprocess raw_ops len 0 in
    ops, !jmap
;;

let interprete (ops_addr: int) (len: int) : int =

    tty_print "tty activated.\n";
    tty_println (string_of_int len);

    Printf.printf "printf works.\n";

    (* accepts a byte array and its length by
     * - a raw-pointer as integer
     * - length as integer
     *)
    let ops_nativeint = (nativeint_of_big_int (big_int_of_int ops_addr)) in
    let ops_void_ptr = (ptr_of_raw_address ops_nativeint) in
    let ops_ptr = (from_voidp char ops_void_ptr) in
    let ops, jmap = preprocess ops_ptr len in
    tty_print_opcodes ops;
    tty_print "\n";

    let jump index = JumpMap.find index jmap in
<<<<<<< HEAD
    let result = Evm.run ops 90000000 jump tty_print_state in
=======
    let result = Evm.run ops 10000 jump tty_print_state in
>>>>>>> 5e91c5dbb29c35c46e5b54d8b766dd011eb6ccf5
    tty_print "\n";
    let _ = match result with
    | Evm.Running -> tty_println "RUNNING"
    | Evm.Error _ -> tty_println "ERROR"
    | Evm.Finish _ -> tty_println "FINISHED"
    in
    0
;;

let _ = Callback.register "interprete" interprete;;

