use rusteff::{Capability, Command, Coroutine, Resumption, Request};
use std::collections::VecDeque;

enum Task<'a> {
    Start(Coroutine<'a, (), SchedulerRequest>),
    Resume(Resumption<'a, (), (), SchedulerRequest>),
}
struct Scheduler<'a> {
    tasks: VecDeque<Task<'a>>
}

enum SchedulerRequest {
    Pause, Fork(Box<dyn FnOnce(&Capability<SchedulerRequest>)>),
}
impl Command for SchedulerRequest {
    type Result = ();
}

trait SchedulerCapability {
    fn pause(&self) -> ();
    fn fork<F: FnOnce(&Capability<SchedulerRequest>) + 'static>(&self, f: F) -> ();
}
impl SchedulerCapability for Capability<SchedulerRequest> {
    fn pause(&self) { self.perform(SchedulerRequest::Pause) }
    fn fork<F: FnOnce(&Capability<SchedulerRequest>) + 'static>(&self, f: F) {
        self.perform(SchedulerRequest::Fork(Box::new(f)))
    }
}

impl <'a> Scheduler<'a> {
    fn new() -> Scheduler<'a> {
        Scheduler { tasks: VecDeque::new() }
    }

    fn enqueue<F: FnOnce(&Capability<SchedulerRequest>) + 'static>(&mut self, f: F) {
        self.tasks.push_back(Task::Start(Coroutine::new(f)))
    }

    fn done(&self) -> bool { self.tasks.is_empty() }
    fn step(&mut self) -> bool {
        let request = match self.tasks.pop_front() {
            None => None,
            Some(Task::Start(coro)) => Some(coro.start()),
            Some(Task::Resume(res)) => Some(res.resume(())),
        };
        match request {
            None => false,
            Some(Request::Return(())) => true,
            Some(Request::Perform((SchedulerRequest::Pause, res))) => {
                self.tasks.push_back(Task::Resume(res)); true
            },
            Some(Request::Perform((SchedulerRequest::Fork(f), res))) => {
                self.tasks.push_back(Task::Resume(res));
                self.tasks.push_back(Task::Start(Coroutine::new(f)));
                true
            }
        }
    }
}

fn main() {
    let mut sched = Scheduler::new();
    sched.enqueue(move |sched_1: &Capability<SchedulerRequest>| {
        println!("Spawning from parent");
        for i in 0 .. 10 {
            sched_1.fork(move |sched_2: &Capability<SchedulerRequest>| {
                for _ in 0 .. 10 {
                    println!("Hello, I'm thread {i}");
                    sched_2.pause();
                }
            });
        }
    });
    while !sched.done() {
        sched.step();
    }
}
