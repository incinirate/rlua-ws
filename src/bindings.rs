#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused)]

use std::os::raw::{c_int, c_char, c_double, c_void};
use std::ffi::{CStr, CString};
use std::ptr::null;
use std::{ptr, mem};
use std::collections::{BTreeMap, HashMap};
use std::cmp::Ordering;
use std::ops::DerefMut;

pub enum lua_State {}

type CFunction = extern fn(*mut lua_State) -> c_int;

/*

typedef struct luaL_Reg {
  const char *name;
  lua_CFunction func;
} luaL_Reg;

*/
#[repr(C)]
pub struct luaL_Reg {
    pub name: *const c_char,
    pub func: Option<CFunction>
}

impl luaL_Reg {
    pub fn new(name: &str, func: CFunction) -> luaL_Reg {
        luaL_Reg { name: CString::new(name).unwrap().into_raw(), func: Some(func) }
    }
}

#[derive(Debug)]
#[derive(PartialEq, Eq, Clone, Copy)]
#[repr(C)]
pub enum LuaRawType {
    Nil = 0,
    Boolean = 1,
    LightUserdata = 2,
    Number = 3,
    String = 4,
    Table = 5,
    Function = 6,
    Userdata = 7,
    Thread = 8,

    None = -1,
}

type lua_Number = c_double;
type lua_Integer = isize;

pub type LibResult = Result<Vec<LuaValue>, ()>;

pub const LUA_REGISTRYINDEX: c_int = (-10000);

extern "C" {
    pub fn lua_createtable(state: *mut lua_State, narr: c_int, nrec: c_int);

    pub fn luaL_setfuncs(state: *mut lua_State, l: *const luaL_Reg, nup: c_int);

    pub fn lua_checkstack(state: *mut lua_State, sz: c_int);

    pub fn lua_pushnil(state: *mut lua_State);
    pub fn lua_pushvalue(state: *mut lua_State, index: c_int);
    pub fn lua_pushboolean(state: *mut lua_State, b: c_int);
    pub fn lua_pushinteger(state: *mut lua_State, n: lua_Integer);
    pub fn lua_pushnumber(state: *mut lua_State, n: lua_Number);
    pub fn lua_pushlstring(state: *mut lua_State, s: *const u8, len: usize);
//    pub fn lua_pushstring(state: *mut lua_State, s: *const c_char) -> *const c_char;
//    pub fn lua_pushlightuserdata(state: *mut lua_State, data: *mut c_void);
    pub fn lua_pushcclosure(state: *mut lua_State, function: Option<CFunction>, n: c_int);
//    pub fn lua_pushthread(state: *mut lua_State) -> c_int;

    pub fn lua_newuserdata(state: *mut lua_State, size: usize) -> *mut c_void;
    pub fn luaL_checkudata(state: *mut lua_State, index: c_int, name: *const u8) -> *mut c_void;

    pub fn lua_setfield(state: *mut lua_State, index: c_int, k: *const c_char);

    pub fn lua_gettop(state: *mut lua_State) -> c_int;
    pub fn lua_settop(state: *mut lua_State, idx: c_int);

//LUA_API void  (lua_rawgeti) (lua_State *L, int idx, int n);
    pub fn lua_rawgeti(state: *mut lua_State, idx: c_int, n: c_int);

//int (luaL_error) (lua_State *L, const char *fmt, ...)
    pub fn luaL_error(state: *mut lua_State, fmt: *const u8);

//    pub fn lua_pop(state: *mut lua_State, index: c_int);

    pub fn lua_tonumber(state: *mut lua_State, index: c_int) -> lua_Number;
    pub fn lua_tolstring(state: *mut lua_State, index: c_int, len: *mut usize) -> *mut u8;
    pub fn lua_touserdata(state: *mut lua_State, index: c_int) -> *mut c_void;

    pub fn lua_type(state: *mut lua_State, index: c_int) -> LuaRawType;
    pub fn lua_typename(state: *mut lua_State, tp: c_int) -> *const c_char;

    pub fn lua_next(state: *mut lua_State, index: c_int) -> c_int;

    // int luaL_ref (lua_State *L, int t);
    pub fn luaL_ref(state: *mut lua_State, tab: c_int) -> c_int;
    pub fn luaL_unref(state: *mut lua_State, tab: c_int, vRef: c_int);
}

pub unsafe fn lua_pop(state: *mut lua_State, n: i32) {
    lua_settop(state, -n - 1);
}

pub unsafe fn lua_newtable(state: *mut lua_State) {
    lua_createtable(state, 0, 0);
}

pub unsafe fn register_lib(state: *mut lua_State, lib: &[luaL_Reg]) {
    /*

    for (; l->name != NULL; l++) {  /* fill the table with given functions */
      int i;
      if (l->func == NULL)  /* place holder? */
        lua_pushboolean(L, 0);
      else {
        for (i = 0; i < nup; i++)  /* copy upvalues to the top */
          lua_pushvalue(L, -nup);
        lua_pushcclosure(L, l->func, nup);  /* closure with those upvalues */
      }
      lua_setfield(L, -(nup + 2), l->name);
    }
    lua_pop(L, nup);  /* remove upvalues */

    */

    for method in lib {
        lua_pushcclosure(state, method.func, 0);
        lua_setfield(state, -2, method.name);
    }
}

pub fn define_lib(state: *mut lua_State, methods: &[luaL_Reg]) -> c_int {
    unsafe {
        lua_newtable(state);
        register_lib(state, &methods);
    }

    1
}

#[derive(Debug)]
//#[derive(Hash)]
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct LuaTable {
    hash_entries: BTreeMap<LuaValue, LuaValue>,
    array: Vec<LuaValue>
} // TODO

#[derive(Debug)]
//#[derive(Hash)]
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct LuaFn {} // TODO

#[derive(Debug)]
//#[derive(Hash)]
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct LuaRef {
    owning_state: *mut lua_State,
    v_ref: c_int
}

impl LuaRef {
    pub unsafe fn new(state: *mut lua_State, index: c_int) -> LuaRef {
        // Duplicate value at given index
        lua_pushvalue(state, index);

        // Create the reference to the value (this also pops it off the stack)
        let v_ref = luaL_ref(state, LUA_REGISTRYINDEX);

        LuaRef { owning_state: state, v_ref }
    }

    pub unsafe fn onto_stack(&self) {
        lua_rawgeti(self.owning_state, LUA_REGISTRYINDEX, self.v_ref);
    }
}

impl Drop for LuaRef {
    fn drop(&mut self) {
        unsafe { luaL_unref(self.owning_state, LUA_REGISTRYINDEX, self.v_ref) }
    }
}

#[derive(Debug)]
pub enum LuaNumber {
    Float(f64),
    Int(i64)
}

impl LuaNumber {
    pub fn new(num: lua_Number) -> LuaNumber {
        if num.fract() < 1e-10 {
            // Probably an int
            LuaNumber::Int(num as i64)
        } else {
            LuaNumber::Float(num)
        }
    }
}

impl PartialOrd for LuaNumber {
    fn partial_cmp(&self, other: &LuaNumber) -> Option<Ordering> {
        let diff = match (self, other) {
            (LuaNumber::Float(x), LuaNumber::Float(y)) => x - y,
            (LuaNumber::Float(x), LuaNumber::Int(y)) => x - (*y as f64),
            (LuaNumber::Int(x), LuaNumber::Float(y)) => *x as f64 - y,
            (LuaNumber::Int(x), LuaNumber::Int(y)) => (x - y) as f64
        };

        if diff.abs() < 1e-10 {
            Some(Ordering::Equal)
        } else if diff < 0.0 {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    }
}
impl Ord for LuaNumber {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialEq for LuaNumber {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaNumber::Float(x), LuaNumber::Float(y)) => (x - y).abs() < 1e-10,
            (LuaNumber::Float(x), LuaNumber::Int(y)) => (x - (*y as f64)).abs() < 1e-10,
            (LuaNumber::Int(x), LuaNumber::Float(y)) => (*x as f64 - y).abs() < 1e-10,
            (LuaNumber::Int(x), LuaNumber::Int(y)) => x == y
        }
    }
}
impl Eq for LuaNumber {}

#[derive(Debug)]
//#[derive(Hash)]
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub enum LuaValue {
    Nil,
    Number(LuaNumber),
    Boolean(bool),
    String(String),
    Table(LuaTable),
    TableRef(LuaRef), // Not a real Lua type, just used as an optimization (avoid exploring tables recursively unless the end user wants to)
    Function(LuaFn),
    Userdata(LuaRef),
    LightUserdata(*mut c_void),
    Thread(*mut lua_State)
}

pub trait Userdata: Drop {
    fn setup(&mut self);
//    fn gc(&self);
    fn get_metatable_name() -> &'static str;
}

#[derive(Debug)]
pub struct LibMethodContext {
    state: *mut lua_State,
    args: Vec<LuaValue>
}

unsafe fn moveValue(state: *mut lua_State, pos: i32, expandTable: bool) -> LuaValue {
    match unsafe { lua_type(state, pos) } {
        LuaRawType::Nil => LuaValue::Nil,
        LuaRawType::Number => LuaValue::Number(LuaNumber::new(unsafe { lua_tonumber(state, pos) })),
        LuaRawType::String => unsafe {
            // tolstring expects a ptr to an integer which it sets to the length of the returned string
            let len_ptr = Box::into_raw(Box::new(0));
            let str_ptr = lua_tolstring(state, pos, len_ptr);

            // Extract the length from the aforementioned ptr
            let len = *Box::from_raw(len_ptr);

            // Lua will garbage collect the string it gave us eventually, so we need to clone it
            let mut dst = Vec::new();
            dst.resize(len, 0);
            ptr::copy_nonoverlapping(str_ptr, dst.as_mut_ptr(), len);

            LuaValue::String(String::from_utf8_lossy(&dst).into_owned())
        },
        LuaRawType::Table => unsafe {
            if expandTable {
                toRTable(state, pos)
            } else {
                LuaValue::TableRef(LuaRef::new(state, pos))
            }
        },
        LuaRawType::Userdata => unsafe { LuaValue::Userdata(LuaRef::new(state, pos)) },

        _ => unimplemented!()
    }
}

unsafe fn toRTable(state: *mut lua_State, index: c_int) -> LuaValue {
    let mut hashEntries = BTreeMap::new();
    let mut array: Vec<LuaValue> = Vec::new();

    // Space for key and value
    lua_checkstack(state, 2);

    lua_pushnil(state);  /* first key */
    while lua_next(state, index) != 0 {
        /* uses 'key' (at index -2) and 'value' (at index -1) */
//        printf("%s - %s\n",
//               lua_typename(L, lua_type(L, -2)),
//               lua_typename(L, lua_type(L, -1)));
        let key = moveValue(state, -2, false);
        let value = moveValue(state, -1, false);

        match key {
            LuaValue::Number(LuaNumber::Int(x)) => {
                if x > 0 {
                    let i = x as usize;
                    array.resize_with(i, || LuaValue::Nil);
//                    array.splice(i..i, value);
                    mem::replace(&mut array[i - 1], value);
                } else {
                    hashEntries.insert(key, value);
                }
            },
            _ => { hashEntries.insert(key, value); }
        };

//        println!("Key: {:?}", );

        /* removes 'value'; keeps 'key' for next iteration */
        lua_pop(state, 1);
    }

    LuaValue::Table(LuaTable { hash_entries: hashEntries, array})
}

impl LibMethodContext {
    pub fn new(state: *mut lua_State) -> LibMethodContext {
        let mut argVec = Vec::new();

        let argCount = unsafe { lua_gettop(state) };
        for i in 1..argCount + 1 {
            argVec.push(unsafe { moveValue(state, i, true) });
//            argVec.push(match unsafe { lua_type(state, i) } {
//                LuaRawType::Nil => LuaValue::Nil,
//                LuaRawType::Number => LuaValue::Number(LuaNumber::new(unsafe { lua_tonumber(state, i) })),
//                LuaRawType::String => unsafe {
//                    // tolstring expects a ptr to an integer which it sets to the length of the returned string
//                    let len_ptr = Box::into_raw(Box::new(0));
//                    let str_ptr = lua_tolstring(state, i, len_ptr);
//
//                    // Extract the length from the aforementioned ptr
//                    let len = *Box::from_raw(len_ptr);
//
//                    // Lua will garbage collect the string it gave us eventually, so we need to clone it
//                    let mut dst = Vec::new();
//                    dst.resize(len, 0);
//                    ptr::copy_nonoverlapping(str_ptr, dst.as_mut_ptr(), len);
//
//                    LuaValue::String(String::from_utf8_lossy(&dst).into_owned())
//                },
//                LuaRawType::Table => unsafe { toRTable(state, i) },
//                LuaRawType::Userdata => LuaValue::Userdata(unsafe { lua_touserdata(state, i) }),
//
//                _ => unimplemented!()
//            });
        }

        LibMethodContext {
            state,
            args: argVec
        }
    }

    pub fn check_args(&self, name: &str, expect_types: &Vec<LuaRawType>) -> Result<(), String> {
        for (index, (a_type, e_type)) in self.args.iter().map(val_type).zip(expect_types).enumerate() {
            if a_type != *e_type {
                let e_name = self.lua_type_string(*e_type);
                let a_name = self.lua_type_string(a_type);

                return Err(format!("bad argument #{} to '{}' ({} expected, got {})", index + 1, name, e_name, a_name));
            }
        }

        if self.args.len() < expect_types.len() {
            let index = self.args.len();
            let e_name = self.lua_type_string(expect_types[index]);
            let a_name = self.lua_type_string(LuaRawType::None);

            return Err(format!("bad argument #{} to '{}' ({} expected, got {})", index + 1, name, e_name, a_name));
        }

        Ok(())
    }

    pub fn gen_udata<D: Userdata>(&self) -> (&mut D, LuaRef) {
        unsafe {
            let ptr = lua_newuserdata(self.state, mem::size_of::<D>()) as *mut D;
            let lua_ref = LuaRef::new(self.state, -1);
            lua_pop(self.state, 1); // new Ref keeps the value on the stack, but we don't want it there so get rid of it

            let reference = &mut *ptr;
            reference.setup();

            return (reference, lua_ref);
        }
    }

    pub fn check_udata<D: Userdata>(&self, index: i32) -> bool {
        let name = D::get_metatable_name();
        let name_cstr = CString::new(name).unwrap();

        let ptr: *const c_void = unsafe { luaL_checkudata(self.state, index, name.as_ptr()) };

        return ptr != null();
    }

    pub fn error(&self, msg: &str) -> Result<Vec<LuaValue>, ()> {
        unsafe { luaL_error(self.state, msg.as_ptr()) };
        Err(())
    }

    fn lua_type_string(&self, t: LuaRawType) -> String {
        unsafe { CStr::from_ptr(lua_typename(self.state, t as i32)) }.to_owned().into_string().unwrap()
    }
}

pub unsafe fn unload_vals(state: *mut lua_State, vals: &Vec<LuaValue>) {
    lua_checkstack(state, vals.len() as i32);

    for val in vals {
        match val {
            LuaValue::Nil => { lua_pushnil(state); },
            LuaValue::Number(x) => match x {
                LuaNumber::Int(x) => { lua_pushinteger(state, *x as isize) },
                LuaNumber::Float(x) => { lua_pushnumber(state, *x) }
            },
            LuaValue::Boolean(x) => { lua_pushboolean(state, *x as c_int) },
            LuaValue::String(x) => { lua_pushlstring(state, x.as_ptr(), x.len()) },
            LuaValue::Table(x) => { unimplemented!(); },
            LuaValue::TableRef(x) => { unimplemented!() ; },
            LuaValue::Function(x) => { unimplemented!(); },
            LuaValue::Userdata(x) => { x.onto_stack() },
            LuaValue::LightUserdata(x) => { unimplemented!(); },
            LuaValue::Thread(x) => { unimplemented!(); }
        }
    }
}

pub fn val_type(val: &LuaValue) -> LuaRawType {
    match val {
        LuaValue::Nil => LuaRawType::Nil,
        LuaValue::Number(_) => LuaRawType::Number,
        LuaValue::Boolean(_) => LuaRawType::Boolean,
        LuaValue::String(_) => LuaRawType::String,
        LuaValue::Table(_) => LuaRawType::Table,
        LuaValue::TableRef(_) => LuaRawType::Table,
        LuaValue::Function(_) => LuaRawType::Function,
        LuaValue::Userdata(_) => LuaRawType::Userdata,
        LuaValue::LightUserdata(_) => LuaRawType::LightUserdata,
        LuaValue::Thread(_) => LuaRawType::Thread
    }
}

#[macro_export]
macro_rules! lib_fn {
    ($name:ident, $lib_name:ident) => {
        pub extern "C" fn $lib_name(state: *mut lua_State) -> c_int {
            println!("Wow");
            let ctx = LibMethodContext::new(state);
            println!("Wow2 {:?}", ctx);

            let ret = $name(ctx);

            match ret {
                Ok(vals) => {
                    unsafe { $crate::unload_vals(state, &vals) };
                    vals.len() as i32
                }
                Err(..) => 0
            }
        }
    }
}

#[macro_export]
macro_rules! chk_args {
    ($ctx:ident, $name:expr, [ $( $arg:ident ),* ]) => {
        match $ctx.check_args($name, &vec![ $( LuaRawType::$arg ),* ]) {
            Ok(..) => {}
            Err(e) => {
                return $ctx.error(&e);
            }
        };
    }
}
