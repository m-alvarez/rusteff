#![feature(extern_types)]
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

#[repr(C)]
enum SeffCoroutineState { FINISHED, PAUSED, RUNNING }

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

#[derive(Debug)]
pub enum Request<'a, Ret, Cmd>
where
    Cmd: Command,
{
    Return(Ret),
    Perform(Cmd, Coroutine<'a, Ret, Cmd>),
}

pub struct Coroutine<'a, Ret, Cmd>
where
    Cmd: Command,
{
    seff_coro: *mut SeffCoroutine,
    marker: PhantomData<&'a (Cmd, Ret)>,
}

impl<'a, Ret, Cmd> Drop for Coroutine<'a, Ret, Cmd>
where
    Cmd: Command,
{
    fn drop(&mut self) {
        unsafe { seff_coroutine_delete(self.seff_coro) }
    }
}

impl<'a, Ret, Cmd> std::fmt::Debug for Coroutine<'a, Ret, Cmd>
where
    Cmd: Command,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        "Coroutine(??)".fmt(f)
    }
}

unsafe extern "C" fn run<Ret, Cmd>(f: *mut u8) -> *mut u8
where
    Cmd: Command,
{
    let closure_ptr = &*(f as *mut Box<dyn FnOnce(&Capability<Cmd>)->Ret>);
    let closure = mem::transmute_copy::<Box<dyn FnOnce(&Capability<Cmd>)->Ret>, Box<dyn FnOnce(&Capability<Cmd>)->Ret>>(closure_ptr);
    let current = seff_current_coroutine();
    let cap = Capability {
        seff_coro: current,
        marker: std::marker::PhantomData,
    };
    Box::<Ret>::into_raw(Box::new((closure)(&cap))) as *mut u8
}

impl<'a, Ret: 'a, Cmd> Coroutine<'a, Ret, Cmd>
where
    Cmd: Command,
{
    pub fn new<F>(f: F) -> Coroutine<'a, Ret, Cmd>
    where
        F: FnOnce(&Capability<Cmd>) -> Ret + 'a,
    {
        let boxed_clos: Box<dyn FnOnce(&Capability<Cmd>) -> Ret> = Box::new(f);
        let clos_ptr = &boxed_clos as *const Box<dyn FnOnce(&Capability<Cmd>)->Ret> as *const u8 as *mut u8;
        mem::forget(boxed_clos);
        Coroutine {
            seff_coro: unsafe {
                seff_coroutine_new(run::<Ret, Cmd> as *mut SeffStartFn, clos_ptr)
            },
            marker: std::marker::PhantomData,
        }
    }

    fn interpret_request(self, request: SeffRequest) -> Request<'a, Ret, Cmd> {
        if request.effect == !0 {
            let result = unsafe { Box::<Ret>::from_raw(request.payload as *mut Ret) };
            Request::Return(*result)
        } else {
            let request =
                unsafe { mem::transmute_copy::<Cmd, Cmd>(&*(request.payload as *const Cmd)) };
            Request::Perform(request, self)
        }
    }

    pub fn start(self) -> Request<'a, Ret, Cmd> {
        let k = self.seff_coro;
        self.interpret_request(unsafe { seff_resume(k, std::ptr::null_mut(), 0) })
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
