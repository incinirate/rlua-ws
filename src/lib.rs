#![crate_type = "cdylib"]

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
    register_userdata!(state, [ Connection ]);
    define_lib(state, &[
        luaL_Reg::new("connect", lib_connect),
        luaL_Reg::new("checkval", lib_checkval)
    ])
}

struct Connection {
    x: i32
}

// impl Drop for Connection {
//     fn drop(&mut self) {
//         // self.x.drop();
//         println!("Dropping!");
//     }
// }

impl Userdata for Connection {
    fn setup(&mut self) {
        // Nothing to do
        self.x = 0;
    }

    fn get_metatable_name() -> &'static str {
        return "ws.connection";
    }

    extern "C" fn gc(_state: *mut lua_State) -> c_int {
        println!("Dropping!");
        0
    }
}


lib_fn!(connect, lib_connect);
fn connect(ctx: LibMethodContext) -> LibResult {
    chk_args!(ctx, "connect", [String]);
//    ctx.check_udata::<Connection>(4);
    let (my_connection, con_ref) = ctx.gen_udata::<Connection>();

    my_connection.x = 5;

    Ok(vec![LuaValue::Userdata(con_ref)])
}

lib_fn!(checkval, lib_checkval);
fn checkval(ctx: LibMethodContext) -> LibResult {
    chk_args!(ctx, "checkval", [Userdata]);
    
    let conn = ctx.check_udata::<Connection>(1).ok_or_else(|| {println!("wow"); ()})?;
    // if let Err(..) = conn { return Err(()) }
//    let (my_connection, con_ref) = ctx.gen_udata::<Connection>();
//
//    my_connection.x = 5;

    // let val_ref = match &ctx.args[0] { LuaValue::Userdata(x) => x, _ => panic!("Userdata unwrap failed after typecheck") };
    // val_ref.
    println!("Extracted value: {}", conn.x);

    Ok(vec![])
}
