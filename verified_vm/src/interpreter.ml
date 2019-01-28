open Big_int;;
open Bytes;;
open Ctypes;;

let preprocess (raw_ops: char list) : Evm.opcode list =
    []
;;

let rec interprete (ops_addr: int) (len: int) : int =

    (* accepts a byte array and its length by
     * - a raw-pointer as integer
     * - length as integer
     *)
    let ops_nativeint = (nativeint_of_big_int (big_int_of_int ops_addr)) in
    let ops_void_ptr = (ptr_of_raw_address ops_nativeint) in
    let ops_ptr = (from_voidp char ops_void_ptr) in

    Evm.run 10000 []
;;

let _ = Callback.register "interprete" interprete;;

