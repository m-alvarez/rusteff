# `rusteff`: Rust bindings for `libseff`

You ever wish Rust had algebraic effects? Me neither.

## Building

Make sure that you've built `libseff` using _fixed_ stacks. Segmented stacks are not supported (nor will they be).

Then run `RUSTFLAGS='-L /path/to/libseff.a/' cargo run --example print`
