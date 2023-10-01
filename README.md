# cobalt-lang
***This branch is no longer developed, and is only here as a reference.***
Cobalt is a compiled programming language, with goals similar to C++.
Documentation is available at <https://matt-cornell.github.io/cobalt-docs>, but may not be up to date.
Everything related to the language is accessible through subcommands on the `co` command ([documentation](https://matt-cornell.github.io/cobalt-docs/co_cli.html)).
## Getting Started
Prebuilds for Linux x86\_64 and Apple arm64 are available for the all of the releases.
To build from source, use:
```bash
cargo install --git https://github.com/matt-cornell/cobalt-lang
```
By default, builds link dynamically to LLVM 16. They can also statically link to it by enabling the `prefer-static` or `force-static` features. LLVM 15 can be linked to instead with the `llvm-15` feature.
## Language features
- Modern programming features without a focus on compatibility with an outdated standard
  - First-class functions, arrays, and types
- A built-in build system and package manager
  - Build system is similar to Cargo
