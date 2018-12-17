open Big_int;;
open Bytes;;

let rec interprete (ops: bytes) : big_int =
    (big_int_of_int 0)
;;

let _ = Callback.register "interprete" interprete;;
