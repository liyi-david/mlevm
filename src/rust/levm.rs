use std::{str};
use std::ptr;

#[link(name = "evm")]
extern "C" {
    // translated from caml/callbacks
    fn caml_startup(argv: *mut *mut u8);
    fn caml_named_value(name: *const u8) -> *const Value;

    // fn caml_callback(closure: Value, arg: Value) -> Value;
    fn caml_callback2(closure: Value, arg1: Value, arg2: Value) -> Value;
    // fn caml_callback3(closure: Value, arg1: Value, arg2: Value, arg3: Value) -> Value;
    // fn caml_callbackN(closure: Value, narg: usize, args: *mut Value) -> Value;
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

macro_rules! string_of_val {
    ($x: expr) => {
        str::from_utf8_unchecked(slice::from_raw_parts($x as *const u8, 32))
    };
}

macro_rules! i32_of_val {
    ($x: expr) => {
        ($x >> 1) as i32
    };
}

macro_rules! val_of_i32 {
    ($x: expr) => {
        (($x << 1) + 1) as Value
    };
}

macro_rules! usize_of_val {
    ($x: expr) => {
        ($x >> 1) as usize
    }
}

macro_rules! val_of_usize {
    ($x: expr) => {
        val_of_i32!($x)
    };
}

macro_rules! val_of_ptr {
    ($x: expr) => {
        val_of_usize!($x as usize)
    };
}

/************************** public interface of levm *********************/
pub fn interprete (opcodes: Vec<i8>) {
    static ML_FUNCNAME: &str = "interprete\0";
    static mut ML_CLOSURE: *const Value = ptr::null();
    unsafe {
        if ML_CLOSURE.is_null() {
            ML_CLOSURE = caml_named_value(ML_FUNCNAME.as_ptr());
        }
        println!("ptr          : {:b}", opcodes.as_ptr() as usize);
        let result = caml_callback2(
            *ML_CLOSURE,
            val_of_ptr!(opcodes.as_ptr()),
            val_of_usize!(opcodes.len())
            );
        let i32_result = i32_of_val!(result);
        println!("i32 result   : {}", i32_result);
        
    }
}

