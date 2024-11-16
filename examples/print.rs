use rusteff::{Capability, Command, Coroutine, Request};

#[derive(Debug)]
struct PrintInt {
    num: i64,
}
impl Command for PrintInt {
    type Result = ();
}

fn main() {
    let coro = Coroutine::<i32, PrintInt>::new(move |cap: &Capability<PrintInt>| {
        println!("Hello from a closure!!");
        for i in 0..10 {
            cap.perform(PrintInt { num: i });
        }
        99
    });
    let mut result = Some(coro.start());
    while let Some(Request::Perform((eff, coro))) = result.take() {
        println!("Handler prints {}", eff.num);
        result = Some(coro.resume(()))
    }
    println!("Hello, world!");
}
