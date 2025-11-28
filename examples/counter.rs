use rusteff::{Command, Coroutine, Request, Either, L, R};

struct Get<T>(std::marker::PhantomData<T>);
impl<T: 'static> Command for Get<T> {
    type Result = T;
}

struct Put<T>(T);
impl<T: 'static> Command for Put<T> {
    type Result = ();
}

fn main() {
    let coro = Coroutine::<i64, Either<Get<i64>, Put<i64>>>::new(move |cap| {
        let mut sum = 0;
        for _ in 0 .. 10 {
            sum += cap.perform(L(Get(std::marker::PhantomData)));
            cap.perform(R(Put(sum)))
        }
        sum
    });
    let mut result = Some(coro.start());
    let mut state = 1;
    while let Some(req) = result.take() {
        match req {
            Request::Perform(Either::L((Get(_), res))) => {
                println!("Get");
                result = Some(res.resume(state))
            },
            Request::Perform(Either::R((Put(v), res))) => {
                println!("Put {v}");
                state = v;
                result = Some(res.resume(()))
            },
            Request::Return(ret) => {
                println!("Result: {ret}")
            }
        }
    }
}
