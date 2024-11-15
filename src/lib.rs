#![feature(never_type)]
#![feature(inherent_associated_types)]

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

trait IProtocol {}
trait IOpen {}
pub trait Protocol: IProtocol {}
pub trait Open: IOpen {}

trait IRecurT<T>
where
    T: Protocol,
{
    type Result: Protocol;
}
trait RecurT<T>: IRecurT<T> {}
pub type Recurse<K, T> = <K as RecurT<T>>::Result;

pub struct Choice<L, R>(PhantomData<(L, R)>);
impl<L: Open, R:Open> Open for Choice<L, R> {}
impl<L: Protocol, R: Protocol> Protocol for Choice<L, R> {}
impl<T: Protocol, L: RecurT<T>, R: RecurT<T>> RecurT<T> for Choice<L, R> {
    type Result = Choice<Recurse<L, T>, Recurse<R, T>>;
}

pub struct Perform<Cmd: Command, Rest>(PhantomData<(Cmd, Rest)>);
impl<Cmd: Command, Rest: Open> Open for Perform<Cmd, Rest> {}
impl<Cmd: Command, Rest: Protocol> Protocol for Perform<Cmd, Rest> {}
impl<T: Protocol, Cmd: Command, Rest: RecurT<T>> RecurT<T> for Perform<Cmd, Rest> {
    type Result = Perform<Cmd, Recurse<Rest, T>>;
}

pub struct Recur;
impl Open for Recur {}
impl<T: Protocol> RecurT<T> for Recur {
    type Result = T;
}

pub struct Fix<F: Open>(PhantomData<F>);
impl<F> Protocol for Fix<F> where F: Open {}

pub struct Done;
impl Open for Done {}
impl Protocol for Done {}

#[derive(Debug)]
pub enum Request<'a, Ret, Cmd, Proto: Protocol>
where
    Cmd: Command,
{
    Return(Ret),
    Perform(Cmd, Resumption<'a, Cmd::Result, Ret, Proto>),
}

type StartFn<'a, Ret, Proto> = dyn FnOnce(&Capability<Proto>) -> Ret + 'a;

pub struct Coroutine<'a, Ret, Proto: Protocol>
{
    closure: Box<StartFn<'a, Ret, Proto>>,
}

unsafe impl<'a, Ret, Proto: Protocol> Send for Coroutine<'a, Ret, Proto> {}
unsafe impl<'a, Res, Ret, Proto: Protocol> Send for Resumption<'a, Res, Ret, Proto> {}

#[derive(Debug)]
pub struct Resumption<'a, Res, Ret, Proto: Protocol>
{
    seff_coro: *mut SeffCoroutine,
    marker: PhantomData<&'a (Res, Ret, Proto)>,
}

impl<'a, Res, Ret, Proto: Protocol> Drop for Resumption<'a, Res, Ret, Proto>
{
    fn drop(&mut self) {
        unsafe { seff_coroutine_delete(self.seff_coro) }
    }
}

impl<'a, Res, Ret, Proto: Protocol> Resumption<'a, Res, Ret, Proto> {
    unsafe fn reinterpret<Proto1: Protocol>(self) -> Resumption<'a, Res, Ret, Proto1> {
        Resumption { seff_coro: self.seff_coro, marker: std::marker::PhantomData }
    }
}

impl<'a, Res, Ret, Proto: Protocol> Resumption<'a, Res, Ret, Proto> {
}

unsafe extern "C" fn run<'a, Ret, Proto: Protocol>(f: *mut u8) -> *mut u8
{
    let clos_box_ptr = f as *const *mut u8 as *const *mut StartFn<'a, Ret, Proto>;
    let clos_box = Box::from_raw(*clos_box_ptr);
    let current = seff_current_coroutine();
    let cap = Capability::<Proto> {
        seff_coro: current,
        marker: std::marker::PhantomData,
    };
    let result = clos_box(&cap);
    unsafe { seff_exit(current, !0, &result as *const Ret as *const u8) }
}

pub struct Capability<Proto: Protocol>
{
    seff_coro: *mut SeffCoroutine,
    marker: std::marker::PhantomData<Proto>,
}

impl <Proto: Protocol> Capability<Proto> {
    unsafe fn reinterpret<Proto1: Protocol>(self) -> Capability<Proto1> {
        Capability { seff_coro: self.seff_coro, marker: std::marker::PhantomData }
    }
}

impl <Proto: Protocol, Cmd: Command> Capability<Perform<Cmd, Proto>> {
    pub fn perform(self, cmd: Cmd) -> (Capability<Proto>, Cmd::Result) {
        (unsafe { self.reinterpret::<Proto>() }, todo!())
    }
}

impl <L: Protocol, R: Protocol> Capability<Choice<L, R>> {
    pub fn left(self) -> Capability<L> {
        unsafe { self.reinterpret::<L>() }
    }
    pub fn right(self) -> Capability<R> {
        unsafe { self.reinterpret::<R>() }
    }
}

impl <F: Open> Capability<Fix<F>>
    where F: RecurT<Fix<F>>
{
    pub fn go(self) -> Capability<Recurse<F, Fix<F>>> {
        unsafe { self.reinterpret::<Recurse<F, Fix<F>>>() }
    }
}

impl<'a, Ret: 'a, Proto: Protocol> Coroutine<'a, Ret, Proto>
{
    pub fn new<F>(f: F) -> Coroutine<'a, Ret, Proto>
    where
        F: FnOnce(&Capability<Proto>) -> Ret + 'a,
    {
        Coroutine {
            closure: Box::new(f),
        }
    }

    /*
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
    */
}

/*
impl<'a, Res, Ret: 'a, Proto> Resumption<'a, Res, Ret, Proto>
{
    fn interpret_request(self, request: SeffRequest) -> Request<'a, Ret, Proto> {
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

impl<Proto> Capability<Proto>
{
    pub fn perform(&self, cmd: Cmd) -> Cmd::Result {
        let cmd_ptr = &cmd as *const Cmd as *const u8;
        mem::forget(cmd);
        let result_ptr = unsafe { seff_yield(self.seff_coro, 0, cmd_ptr) as *const Cmd::Result };
        unsafe { mem::transmute_copy::<Cmd::Result, Cmd::Result>(&*(result_ptr)) }
    }
}
*/
