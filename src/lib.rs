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
    const METATABLE_NAME: &'static str = "ws.connection";

    fn setup(&mut self) {
        // Nothing to do
        self.x = 0;
    }

    fn drop(&mut self) {
        // self.x.drop();
        println!("Dropping!");
    }

    // extern "C" fn gc(_state: *mut lua_State) -> c_int {
    //     println!("Dropping!");
    //     0
    // }
}

struct Client {
    sender: ws::Sender
}
impl ws::Handler for Client {
    fn on_open(&mut self, shake: ws::Handshake) -> ws::Result<()> {
        println!("hmmmmm");
        if let Some(addr) = shake.remote_addr()? {
            println!("Connection with {} now open", addr);
        }

        self.sender.send("Hello WebSocket")

        // Ok(())
    }

    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        println!("Received message {:?}", msg);

        self.sender.close(ws::CloseCode::Normal)

        // Ok(())
    }

    fn on_close(&mut self, code: ws::CloseCode, reason: &str) {
        println!("Connection closing due to ({:?}) {}", code, reason);
    }

    fn on_error(&mut self, err: ws::Error) {
        println!("Encountered an error: {}\nEnable a logger to see more information.", err);
    }

    fn on_request(&mut self, req: &ws::Request) -> ws::Result<ws::Response> {
        println!("Handler received request:\n{}", req);
        ws::Response::from_request(req)
    }

    /// A method for handling the low-level workings of the response portion of the WebSocket
    /// handshake.
    ///
    /// Implementors can inspect the Response and choose to fail the connection by
    /// returning an error. This method will not be called when the handler represents a server
    /// endpoint. The response should indicate which WebSocket protocol and extensions the server
    /// has agreed to if any.
    fn on_response(&mut self, res: &ws::Response) -> ws::Result<()> {
        println!("Handler received response:\n{}", res);
        Ok(())
    }
}


lib_fn!(connect, lib_connect);
fn connect(ctx: LibMethodContext) -> LibResult {
    chk_args!(ctx, "connect", [String]);
//    ctx.check_udata::<Connection>(4);
    let (my_connection, con_ref) = ctx.gen_udata::<Connection>();

    my_connection.x = 5;

    let url = get_arg!(ctx, 0, String); // &ctx.args[0];
    println!("Connectiong to {}, s{}", url, url.len());
    match ws::connect(url.to_owned(), |sender: ws::Sender| {
        println!("Hmm:thonk:");
        Client { sender }
        }) {
        Ok(()) => {
            println!("COnnectednice");
        },
        Err(err) => {
            return ctx.error(&err.details);
        }
    }

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
