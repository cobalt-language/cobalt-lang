# cobalt-lang
Cobalt is a compiled programming language, with goals similar to C++.
The `co` command can be used for everything in the language, including compiling individual files, building projects, and downloading packages.
## The `co` Command
With different subcommands, you can:
- AOT compile a single file (`co aot`)
- JIT compile and run a program (`co jit`)
- build a project (`co build`)
- install a package (`co install`)
## Getting Started
Prebuilt versions aren't available, so you'll have to build from source using Cargo.
```bash
git clone https://github.com/matt-cornell/cobalt-lang
cd cobalt-lang
cargo build
```
## Language features
- C-like speed from LLVM optimization passes
- Modern programming features without a focus on compatibility with a 40 year-old standard
  - First-class functions, arrays, and types
- A built-in build system and package manager
