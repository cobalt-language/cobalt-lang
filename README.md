# cobalt-lang
Cobalt is a compiled programming language, with goals similar to C++.
Documentation is available at <https://cobalt-language.github.io/>, but may not be up to date.
Everything related to the language is accessible through subcommands on the `co` command ([documentation](https://cobalt-language.github.io/co_cli.html)).
## Getting Started
Prebuilds for Linux x86\_64 and Apple arm64 are available for the all of the releases.
To build from source, use:
```bash
cargo install --git https://github.com/matt-cornell/cobalt-lang
```
By default, builds link dynamically to LLVM 16. They can also statically link to it by enabling the `prefer-static` or `force-static` features.
## Language features
- Modern programming features without a focus on compatibility with an outdated standard
  - First-class functions, arrays, and types
- A built-in build system and package manager
  - Build system is similar to Cargo
