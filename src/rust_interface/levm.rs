mod levm {

    use std::ptr;

    #[link(name = "evm")]
    extern "C" {
        // translated from caml/callbacks
        fn caml_startup(argv: *mut *mut u8);
        fn caml_named_value(name: *const u8) -> *const Value;
        fn caml_callback(closure: Value, arg: Value) -> Value;
    }

    // Value is a pointer, but given in form of unsigned integer
    type Value = usize;

    pub fn initialize () {
        let mut ptr = ptr::null_mut();
        let argv: *mut *mut u8 = &mut ptr;
        unsafe {
            caml_startup(argv);
        }
    }

    /************************** public interface of levm *********************/
    pub fn interprete (n: i32) {
        static ML_FUNCNAME: &str = "interprete\0";
        static mut ML_CLOSURE: *const Value = ptr::null();
        unsafe {
            if ML_CLOSURE.is_null() {
                ML_CLOSURE = caml_named_value(ML_FUNCNAME.as_ptr());
            }
            caml_callback(*ML_CLOSURE, n as usize);
        }
    }

}

fn main() {
    println!(">>> rust levm test.");
    levm::initialize();
    levm::interprete(100);
}
