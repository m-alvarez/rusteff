use rusteff::*;

struct Get<T>(std::marker::PhantomData<T>);
impl<T: 'static> Command for Get<T> {
    type Result = T;
}

struct Put<T>(T);
impl<T: 'static> Command for Put<T> {
    type Result = ();
}

// Example:
type Example<T: 'static> = Fix<Choice<Perform<Get<T>, Recur>, Perform<Put<T>, Recur>>>;

fn example(cap: Capability<Example<i64>>) {
    //let (cap, result) = cap.go().left().perform(Get());
    let c1 = cap.go();
}

fn foo<T: 'static + Copy>(cap: Capability<Perform<Get<T>, Perform<Put<T>, Done>>>) -> T {
    let (cap, result) = cap.perform(Get(std::marker::PhantomData));
    let (cap, ()) = cap.perform(Put(result));
    result
}


fn main() {

}
