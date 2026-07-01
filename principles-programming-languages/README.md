## Principles of Programming Languages — Assignments

This repository contains coursework and projects for a Principles of Programming Languages class. The code is primarily in OCaml and includes functional programming exercises, parsing, and a small interpreter.

### Repository layout
- `assignment1/`: Intro OCaml list/recursion exercises with self-tests.
- `assignment2/`: Higher-order functions, folds, combinators with self-tests.
- `assignment3/assignment3/`: Expression simplifier and a small IMP interpreter (parser + evaluator) with tests and sample programs.
- `Parsing-main/` and `TypeInference-main/`: Standalone reference projects from course materials.
- `Week 2/`: Early practice files.

### Prerequisites
- OCaml (tested with OCaml 4.x toolchain)
- `make`
- For assignment3: `ocamllex` and `ocamlyacc` (usually included with OCaml)

### Quick start
- Assignment 1:
  - `cd assignment1`
  - `make`
  - `./assignment1`  # runs the built-in tests
- Assignment 2:
  - `cd assignment2`
  - `make`
  - `./assignment2`  # runs the built-in tests
- Assignment 3:
  - `cd assignment3/assignment3`
  - `make`
  - `./imp`  # runs the built-in tests; sample programs are in `programs/`

### Notes
- Build artifacts and platform files are ignored via `.gitignore`.
- The `Parsing-main/` and `TypeInference-main/` folders are separate projects and include their own README and build steps.


