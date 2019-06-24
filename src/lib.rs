#![crate_type = "dylib"]

#[macro_use]
mod bindings;

use crate::bindings::*;
use std::os::raw::c_int;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

#[no_mangle]
pub unsafe extern "C" fn luaopen_ws(state: *mut lua_State) -> c_int {
    define_lib(state, &[
        luaL_Reg::new("connect", lib_connect)
    ])
}

lib_fn!(connect, lib_connect);
fn connect(ctx: LibMethodContext) -> LibResult {
    chk_args!(ctx, "connect", [String, Userdata]);

    Ok(vec![LuaValue::Number(LuaNumber::Int(5)), LuaValue::String(String::from("test"))])
}
