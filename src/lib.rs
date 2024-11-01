#![feature(never_type)]

use std::marker::PhantomData;
use std::mem;

type SeffEffectSet = u64;
type SeffEffectID = u64;
#[repr(C)]
struct SeffRequest {
    effect: SeffEffectID,
    payload: *mut u8,
}
type SeffStartFn = unsafe extern "C" fn(*const u8) -> *const u8;

// Provided for debugging purposes
#[repr(C)]
#[allow(dead_code)]
enum SeffCoroutineState {
    Finished,
    Paused,
    Running,
}

// Provided for debugging purposes
#[repr(C)]
struct SeffCont {
    current_coroutine: *const u8,
    rsp: *const u8,
    rbp: *const u8,
    ip: *const u8,
    rbx: *const u8,
    r12: *const u8,
    r13: *const u8,
    r14: *const u8,
    r15: *const u8,
}

// Provided for debugging purposes
#[repr(C)]
struct SeffCoroutine {
    frame_ptr: *const u8,
    resume_point: SeffCont,
    state: SeffCoroutineState,
    parent: *const SeffCoroutine,
    handled_effects: u64,
}

extern "C" {
    fn seff_coroutine_new(f: *const SeffStartFn, arg: *const u8) -> *mut SeffCoroutine;
    fn seff_coroutine_delete(coro: *mut SeffCoroutine);

    fn seff_current_coroutine() -> *mut SeffCoroutine;

    fn seff_resume(k: *mut SeffCoroutine, arg: *const u8, handled: SeffEffectSet) -> SeffRequest;
    fn seff_yield(
        yield_from: *mut SeffCoroutine,
        eff: SeffEffectID,
        payload: *const u8,
    ) -> *const u8;
    fn seff_exit(yield_from: *mut SeffCoroutine, eff: SeffEffectID, payload: *const u8) -> !;
}

pub trait Command
where
    Self: 'static,
{
    type Result;
}
impl Command for ! {
    type Result = !;
}

trait Closed {}

trait CloseT<T>
where
    T: Closed,
{
    type Result: Closed;
}
type Close<K, T> = <K as CloseT<T>>::Result;

pub struct Done;
impl Closed for Done {}

pub struct Choose<L, R>(PhantomData<(L, R)>);
impl<L: Closed, R: Closed> Closed for Choose<L, R> {}
impl<T: Closed, L: CloseT<T>, R: CloseT<T>> CloseT<T> for Choose<L, R> {
    type Result = Choose<Close<L, T>, Close<R, T>>;
}

pub struct Perform<Cmd: Command, Rest>(PhantomData<(Cmd, Rest)>);
impl<Cmd: Command, Rest: Closed> Closed for Perform<Cmd, Rest> {}
impl<T: Closed, Cmd: Command, Rest: CloseT<T>> CloseT<T> for Perform<Cmd, Rest> {
    type Result = Perform<Cmd, Close<Rest, T>>;
}

pub struct Recur;
impl<T: Closed> CloseT<T> for Recur {
    type Result = T;
}

pub struct Fix<F>(PhantomData<F>);
impl<F> Closed for Fix<F> {}

struct Get {}
impl Command for Get {
    type Result = i64;
}
struct Put(i64);
impl Command for Put {
    type Result = ();
}
// Example:
type Example = Fix<Choose<Choose<Perform<Get, Recur>, Perform<Put, Recur>>, Done>>;

#[derive(Debug)]
pub enum Request<'a, Ret, Cmd>
where
    Cmd: Command,
{
    Return(Ret),
    Perform(Cmd, Resumption<'a, Ret, Cmd>),
}

type StartFn<'a, Ret, Cmd> = dyn FnOnce(&Capability<Cmd>) -> Ret + 'a;

pub struct Coroutine<'a, Ret, Cmd>
where
    Cmd: Command,
{
    closure: Box<StartFn<'a, Ret, Cmd>>,
}

unsafe impl<'a, Ret, Cmd> Send for Coroutine<'a, Ret, Cmd> where Cmd: Command {}
unsafe impl<'a, Ret, Cmd> Send for Resumption<'a, Ret, Cmd> where Cmd: Command {}

#[derive(Debug)]
pub struct Resumption<'a, Ret, Cmd>
where
    Cmd: Command,
{
    seff_coro: *mut SeffCoroutine,
    marker: PhantomData<&'a (Cmd, Ret)>,
}

impl<'a, Ret, Cmd> Drop for Resumption<'a, Ret, Cmd>
where
    Cmd: Command,
{
    fn drop(&mut self) {
        unsafe { seff_coroutine_delete(self.seff_coro) }
    }
}

unsafe extern "C" fn run<'a, Ret, Cmd>(f: *mut u8) -> *mut u8
where
    Cmd: Command,
{
    let clos_box_ptr = f as *const *mut u8 as *const *mut StartFn<'a, Ret, Cmd>;
    let clos_box = Box::from_raw(*clos_box_ptr);
    let current = seff_current_coroutine();
    let cap = Capability {
        seff_coro: current,
        marker: std::marker::PhantomData,
    };
    let result = clos_box(&cap);
    unsafe { seff_exit(current, !0, &result as *const Ret as *const u8) }
}

impl<'a, Ret: 'a, Cmd> Coroutine<'a, Ret, Cmd>
where
    Cmd: Command,
{
    pub fn new<F>(f: F) -> Coroutine<'a, Ret, Cmd>
    where
        F: FnOnce(&Capability<Cmd>) -> Ret + 'a,
    {
        Coroutine {
            closure: Box::new(f),
        }
    }

    pub fn start(self) -> Request<'a, Ret, Cmd> {
        let clos_box = Box::into_raw(self.closure);
        let clos_box_ptr = &clos_box as *const *mut StartFn<'a, Ret, Cmd>;
        let coro = unsafe {
            seff_coroutine_new(
                run::<Ret, Cmd> as *const SeffStartFn,
                clos_box_ptr as *const *const u8 as *const u8,
            )
        };
        let res = Resumption {
            seff_coro: coro,
            marker: std::marker::PhantomData,
        };
        res.interpret_request(unsafe { seff_resume(coro, std::ptr::null_mut(), 0) })
    }
}

impl<'a, Ret: 'a, Cmd> Resumption<'a, Ret, Cmd>
where
    Cmd: Command,
{
    fn interpret_request(self, request: SeffRequest) -> Request<'a, Ret, Cmd> {
        if request.effect == !0 {
            let result =
                unsafe { mem::transmute_copy::<Ret, Ret>(&*(request.payload as *const Ret)) };
            Request::Return(result)
        } else {
            let request =
                unsafe { mem::transmute_copy::<Cmd, Cmd>(&*(request.payload as *const Cmd)) };
            Request::Perform(request, self)
        }
    }

    pub fn resume(self, result: Cmd::Result) -> Request<'a, Ret, Cmd> {
        let result_ptr = &result as *const Cmd::Result as *const u8;
        mem::forget(result);
        let k = self.seff_coro;
        self.interpret_request(unsafe { seff_resume(k, result_ptr, 0) })
    }
}

pub struct Capability<Cmd>
where
    Cmd: Command,
{
    seff_coro: *mut SeffCoroutine,
    marker: std::marker::PhantomData<Cmd>,
}

impl<Cmd> Capability<Cmd>
where
    Cmd: Command,
{
    pub fn perform(&self, cmd: Cmd) -> Cmd::Result {
        let cmd_ptr = &cmd as *const Cmd as *const u8;
        mem::forget(cmd);
        let result_ptr = unsafe { seff_yield(self.seff_coro, 0, cmd_ptr) as *const Cmd::Result };
        unsafe { mem::transmute_copy::<Cmd::Result, Cmd::Result>(&*(result_ptr)) }
    }
}
