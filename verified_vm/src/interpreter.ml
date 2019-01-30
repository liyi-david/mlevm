open Big_int;;
open Bytes;;
open Ctypes;;
open Debug;;

let rec u256_from_ptr (p: char ptr) (len: int): big_int =
    if len == 0 then
        (big_int_of_int 0)
    else
        (add_big_int (mult_int_big_int 256 (u256_from_ptr (p +@ 1) (len - 1))) (big_int_of_int (int_of_char !@p)))
;;

let rec preprocess (raw_ops: char ptr) (len: int): Evm.opcode list =
    if len == 0 then [] else begin
        let curr = int_of_char (!@ raw_ops) in
        match curr with
        | 0x00 -> Evm.STOP :: (preprocess (raw_ops +@ 1) (len - 1))
        | 0x50 -> Evm.POP  :: (preprocess (raw_ops +@ 1) (len - 1))
        | _ -> begin
            if 0x60 <= curr && curr <= 0x7f then
                let wbits = curr - 0x60 + 1 in
                let a = (u256_from_ptr (raw_ops +@ 1) wbits) in
                (Evm.PUSH (int_of_big_int a)) :: (preprocess (raw_ops +@ (wbits + 1)) (len - wbits - 1))
            else
                (Evm.INVALID curr) :: (preprocess (raw_ops +@ 1) (len - 1))
        end
    end
;;

let interprete (ops_addr: int) (len: int) : int =

    tty_print "tty activated.\n";
    tty_println (string_of_int len);

    (* accepts a byte array and its length by
     * - a raw-pointer as integer
     * - length as integer
     *)
    let ops_nativeint = (nativeint_of_big_int (big_int_of_int ops_addr)) in
    let ops_void_ptr = (ptr_of_raw_address ops_nativeint) in
    let ops_ptr = (from_voidp char ops_void_ptr) in
    let ops = preprocess ops_ptr len in
    tty_print_opcodes ops;
    tty_print "\n";
    let _ = Evm.run ops 10000 in
    0
;;

let _ = Callback.register "interprete" interprete;;

