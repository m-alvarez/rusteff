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
#[allow(dead_code)]
enum SeffCoroutineState {
    FINISHED,
    PAUSED,
    RUNNING,
}

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
    fn seff_exit(yield_from: *mut SeffCoroutine, eff: SeffEffectID, payload: *const u8) -> !;
}

pub trait Command {
    type Result: 'static;
}

pub enum Either<L, R> {
    L(L),
    R(R),
}
#[allow(private_interfaces)]
pub trait Responsibility {
    type Req<'a, Ret: 'a, Proto: 'a + Responsibility>;
    fn untag<'a, Req: 'a, Ret: 'a, Proto: 'a + Responsibility>(
        res: Resumption<'a, Req, Ret, Proto>,
        req: SeffRequest,
    ) -> Self::Req<'a, Ret, Proto>;
}
#[allow(private_interfaces)]
impl<Cmd: Command> Responsibility for Cmd {
    type Req<'a, Ret: 'a, Proto: 'a + Responsibility> =
        (Cmd, Resumption<'a, Cmd::Result, Ret, Proto>);
    fn untag<'a, Req: 'a, Ret: 'a, Proto: 'a + Responsibility>(
        res: Resumption<'a, Req, Ret, Proto>,
        req: SeffRequest,
    ) -> (Cmd, Resumption<'a, Cmd::Result, Ret, Proto>) {
        assert!(req.effect == 0);
        let command = unsafe { mem::transmute_copy::<Cmd, Cmd>(&*(req.payload as *const Cmd)) };
        let request = (
            command,
            Resumption {
                seff_coro: res.seff_coro,
                marker: std::marker::PhantomData,
            },
        );
        std::mem::forget(res);
        request
    }
}
#[allow(private_interfaces)]
impl<L: Responsibility, R: Responsibility> Responsibility for Either<L, R> {
    type Req<'a, Ret: 'a, Proto: 'a + Responsibility> =
        Either<L::Req<'a, Ret, Proto>, R::Req<'a, Ret, Proto>>;
    fn untag<'a, Req: 'a, Ret: 'a, Proto: 'a + Responsibility>(
        res: Resumption<'a, Req, Ret, Proto>,
        req: SeffRequest,
    ) -> Self::Req<'a, Ret, Proto> {
        let sub_request = SeffRequest {
            effect: req.effect >> 1,
            payload: req.payload,
        };
        if req.effect & 1 == 0 {
            Either::L(L::untag(res, sub_request))
        } else {
            Either::R(R::untag(res, sub_request))
        }
    }
}

type StartFn<'a, Ret, Proto> = dyn FnOnce(&Capability<Proto>) -> Ret + 'a;

pub struct Coroutine<'a, Ret, Proto: Responsibility> {
    closure: Box<StartFn<'a, Ret, Proto>>,
}

#[derive(Debug)]
pub struct Resumption<'a, Req, Ret, Proto: Responsibility> {
    seff_coro: *mut SeffCoroutine,
    marker: PhantomData<&'a (Req, Proto, Ret)>,
}

impl<'a, Req, Ret, Proto: Responsibility> Drop for Resumption<'a, Req, Ret, Proto> {
    fn drop(&mut self) {
        unsafe { seff_coroutine_delete(self.seff_coro) }
    }
}

unsafe extern "C" fn run<'a, Ret, Proto: Responsibility>(f: *mut u8) -> *mut u8 {
    let clos_box_ptr = f as *const *mut u8 as *const *mut StartFn<'a, Ret, Proto>;
    let clos_box = Box::from_raw(*clos_box_ptr);
    let current = seff_current_coroutine();
    let cap = Capability {
        seff_coro: current,
        marker: std::marker::PhantomData,
    };
    let result = clos_box(&cap);
    unsafe { seff_exit(current, !0, &result as *const Ret as *const u8) }
}

pub enum Request<'a, Ret: 'a, Proto: 'a + Responsibility> {
    Return(Ret),
    Perform(Proto::Req<'a, Ret, Proto>),
}

impl<'a, Ret: 'a, Proto: 'a + Responsibility> Coroutine<'a, Ret, Proto> {
    pub fn new<F>(f: F) -> Coroutine<'a, Ret, Proto>
    where
        F: FnOnce(&Capability<Proto>) -> Ret + 'a,
    {
        Coroutine {
            closure: Box::new(f),
        }
    }

    pub fn start(self) -> Request<'a, Ret, Proto> {
        let clos_box = Box::into_raw(self.closure);
        let clos_box_ptr = &clos_box as *const *mut StartFn<'a, Ret, Proto>;
        let coro = unsafe {
            seff_coroutine_new(
                run::<Ret, Proto> as *const SeffStartFn,
                clos_box_ptr as *const *const u8 as *const u8,
            )
        };
        let res = Resumption::<'a, (), Ret, Proto> {
            seff_coro: coro,
            marker: std::marker::PhantomData,
        };
        res.interpret_request(unsafe { seff_resume(coro, std::ptr::null_mut(), 0) })
    }
}

impl<'a, Expect: 'a, Ret: 'a, Proto: Responsibility> Resumption<'a, Expect, Ret, Proto> {
    fn interpret_request(self, request: SeffRequest) -> Request<'a, Ret, Proto> {
        if request.effect == !0 {
            let result =
                unsafe { mem::transmute_copy::<Ret, Ret>(&*(request.payload as *const Ret)) };
            Request::Return(result)
        } else {
            Request::Perform(Proto::untag(self, request))
        }
    }

    pub fn resume(self, result: Expect) -> Request<'a, Ret, Proto> {
        let result_ptr = &result as *const Expect as *const u8;
        mem::forget(result);
        let k = self.seff_coro;
        self.interpret_request(unsafe { seff_resume(k, result_ptr, 0) })
    }
}

pub struct L<T>(pub T);
pub struct R<T>(pub T);

trait IRight<T> {
    fn tag() -> u64;
}
#[allow(private_bounds)]
pub trait Right<T>: IRight<T> {
    type Result;
}
impl<Cmd: Command> IRight<Cmd> for Cmd {
    fn tag() -> u64 {
        0
    }
}
impl<Cmd: Command> Right<Cmd> for Cmd {
    type Result = Cmd::Result;
}
impl<LP, RP, P: Right<LP>> IRight<Either<LP, RP>> for L<P> {
    fn tag() -> u64 {
        P::tag() << 1 | 0
    }
}
impl<LP, RP, P: Right<LP>> Right<Either<LP, RP>> for L<P> {
    type Result = P::Result;
}
impl<LP, RP, P: Right<RP>> IRight<Either<LP, RP>> for R<P> {
    fn tag() -> u64 {
        P::tag() << 1 | 1
    }
}
impl<LP, RP, P: Right<RP>> Right<Either<LP, RP>> for R<P> {
    type Result = P::Result;
}

pub struct Capability<Cap> {
    seff_coro: *mut SeffCoroutine,
    marker: std::marker::PhantomData<Cap>,
}

impl<Cap> Capability<Cap> {
    pub fn perform<R: Right<Cap>>(&self, cmd: R) -> R::Result {
        let cmd_ptr = &cmd as *const R as *const u8;
        mem::forget(cmd);
        let result_ptr =
            unsafe { seff_yield(self.seff_coro, R::tag(), cmd_ptr) as *const R::Result };
        unsafe { mem::transmute_copy::<R::Result, R::Result>(&*result_ptr) }
    }
}
