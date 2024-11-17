# YAP

*YAP* is Yet Another PL/0. This is a (WIP) compiler for the PL/0 language implementation that follows [Adam Dunson's specification](https://raw.githubusercontent.com/adamdunson/pl0-compiler/master/doc/PL0%20User's%20Manual.pdf).

## Description

YAP is, as mentioned previously, a compiler for the PL/0 language. It is written in Erlang using OTP 27 and is currently in very early stages *(read: not functional)*.
YAP is entirely handwritten using a recursive descent parser, keeping the number of dependencies low.

There are 3 main reasons for this project:

1. For me to test my ability to independently implement a compiler
2. To give me a familiar codebase that I can try implementations of compiler optimizations on.
3. For me to learn the Erlang language and ecosystem.

## Progress

- [x] Lexer
- [ ] **Parser** [(Issue)](https://github.com/erikcghedlund/yap/issues/1) [(Branch)](https://github.com/erikcghedlund/yap/tree/1-write-parser)
- [ ] Semantic Analyzer [(Issue)](https://github.com/erikcghedlund/yap/issues/5)
- [ ] Intermediate Representation [(Issue)](https://github.com/erikcghedlund/yap/issues/2)
- [ ] Code Generator [(Issue)](https://github.com/erikcghedlund/yap/issues/6)

## Getting started

### Dependencies

- [Erlang](https://www.erlang.org/downloads)
- [Rebar3](https://www.rebar3.org/docs/getting-started/)
- [(go-)task](https://taskfile.dev/installation/)

Rebar3 will resolve the remaining dependencies

### Building

Building is simple (once the dependencies are installed):

```
task build
```

### Testing

And testing is just as simple:

```
task test
```

### Running

There is currently no good usage for running the program, but some individual components can be run. For instance, to see the generated tokens run:

```
task run-lexer -- <file>
```

### License

This project is licensed under the [GPLv3 License](LICENSE.md).
