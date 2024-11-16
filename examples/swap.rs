use rusteff::{Capability, Command, Coroutine, Request};


struct A {}
impl Command for A { type Result = i64; }

struct B {}
impl Command for B { type Result = i64; }

fn main() {
    let handler_a = Coroutine::<(), A>::new(move |cap_a: &Capability<A>| {
        println!("Handler for A!");
        let handler_b = Coroutine::<(), B>::new(move |cap_b: &Capability<B>| {
            println!("Handler for B!");
            for _ in 0 .. 10 {
                let a = cap_a.perform(A{});
                println!("A is {a}");
                let b = cap_b.perform(B{});
                println!("B is {b}")
            }
        });
        let mut flip = false;
        let mut result = Some(handler_b.start());
        while let Some(Request::Perform((_, handle))) = result.take() {
            flip = !flip;
            if flip {
                result = Some(handle.resume(cap_a.perform(A{}) * 10))
            } else {
                result = Some(handle.resume(456))
            }
        }
    });

    let mut result = Some(handler_a.start());
    while let Some(Request::Perform((_, handle))) = result.take() {
        result = Some(handle.resume(123))
    }
}
