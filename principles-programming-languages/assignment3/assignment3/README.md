## Assignment 3 — Expression Simplifier and IMP Interpreter

This project contains:
- A polynomial-based simplifier for arithmetic expressions over a single variable `x`
- A small IMP language with parser and evaluator, including:
  - Integers, booleans, arithmetic and boolean ops
  - First-class functions (closures)
  - Commands (`declare`, assignment, sequencing, `if`, `while`, `for`)

Self-tests are included in `src/assignment3.ml`. Sample programs for the IMP language are under `programs/`.

### Prerequisites
- OCaml toolchain, `make`
- `ocamllex`, `ocamlyacc`

### Build
```bash
make
```

This produces the `imp` executable.

### Run tests
```bash
./imp
```

The tests also parse and evaluate programs from `programs/*.imp`.


