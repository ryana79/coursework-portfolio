# Systems Programming Projects

This repository contains a collection of systems programming projects developed for CS 214. Each project focuses on different aspects of systems programming including memory management, file processing, shell implementation, and network programming.

## Project Overview

### P1: Custom Memory Allocator (My Little Malloc)
**Authors:** Ryan Amir, Gian Cases  
**File:** `P1/`

A custom implementation of `malloc()` and `free()` functions with advanced error detection and memory management features.

**Key Features:**
- 8-byte memory alignment
- Best-fit allocation strategy
- Chunk splitting and coalescing to minimize fragmentation
- Error detection for invalid frees, double frees, and memory leaks
- Comprehensive error reporting with file and line number information

**Files:**
- `mymalloc.c` - Core allocator implementation
- `mymalloc.h` - Header with macros and function prototypes
- `memgrind.c` - Stress testing program with 5 different workloads
- `memtest.c` - Memory integrity verification tests
- `test_mymalloc.c` - Comprehensive test suite

**Usage:**
```bash
cd P1
make
./memgrind    # Run stress tests
./memtest     # Run integrity tests
gcc -Wall -Werror -std=c99 -g -o test_mymalloc test_mymalloc.c mymalloc.c
./test_mymalloc  # Run comprehensive tests
```

### P2: Word Frequency Analyzer and Outlier Detection
**Authors:** Ryan Amir, Gian Cases  
**File:** `P2/`

A text analysis tool that identifies words with unusual frequency distributions across multiple files, highlighting potential outliers.

**Key Features:**
- Recursive directory traversal for `.txt` files
- Dynamic memory allocation for handling files of any size
- Case-insensitive word processing
- Outlier detection based on frequency ratios
- Robust word parsing with punctuation handling

**Files:**
- `outlier.c` - Main program implementation
- `Makefile` - Build configuration
- Sample text files for testing

**Usage:**
```bash
cd P2
make
./outlier <directory_or_file> [additional_files...]
```

### P3: Shell Implementation (mysh)
**Authors:** Ryan Amir, Gian Cases  
**File:** `P3/`

A custom shell implementation supporting both interactive and batch modes with advanced command processing features.

**Key Features:**
- Built-in commands: `pwd`, `cd`, `which`, `exit`, `die`
- Input/output redirection (`<`, `>`)
- Pipeline support (`|`)
- Wildcard expansion (`*.txt`)
- Conditional execution (`and`, `or`)
- Both interactive and batch mode operation

**Files:**
- `mysh.c` - Shell implementation
- `Makefile` - Build configuration
- Test scripts: `test_basic.sh`, `test_redirection.sh`, `test_pipeline.sh`, `test_wildcard.sh`, `test_conditionals.sh`, `test_exit.sh`, `test_die.sh`

**Usage:**
```bash
cd P3
make
./mysh                    # Interactive mode
./mysh test_basic.sh      # Batch mode
```

**Testing:**
```bash
# Run individual test suites
bash test_basic.sh
bash test_redirection.sh
bash test_pipeline.sh
bash test_wildcard.sh
bash test_conditionals.sh
```

### P4: Rock-Paper-Scissors Network Server (RPSD)
**Authors:** Ryan Amir, Gian Cases  
**File:** `p4/`

A networked Rock-Paper-Scissors server implementing the RPSP protocol with support for concurrent games and multiple client connections.

**Key Features:**
- Concurrent game handling using `fork()`
- RPSP protocol implementation
- Dynamic player management with automatic capacity scaling
- Duplicate login prevention
- Signal handling for clean shutdown
- Non-blocking I/O using `poll()`

**Files:**
- `rpsd.c` - Main server implementation
- `network.c` - Network helper functions
- `network.h` - Network function headers
- `rc.c` - Test client implementation
- `Makefile` - Build configuration

**Usage:**
```bash
cd p4
make
./rpsd <port_number>      # Start server

# In another terminal:
./rc localhost <port_number>  # Connect client
```

### Additional Files

#### `strlen.c`
A standalone C program demonstrating pointer arithmetic and string length calculation with examples of:
- Manual string length calculation using pointer arithmetic
- Array traversal patterns
- Bubble sort implementation framework

**Usage:**
```bash
gcc -o strlen strlen.c
./strlen
```

## Prerequisites

- GCC compiler
- Make build system
- POSIX-compliant system (Linux/macOS)
- Basic networking support for P4

## General Compilation

Each project includes its own Makefile. To build all projects:

```bash
# Build each project individually
cd P1 && make && cd ..
cd P2 && make && cd ..
cd P3 && make && cd ..
cd p4 && make && cd ..

# Compile strlen.c
gcc -o strlen strlen.c
```

## Testing

Each project includes comprehensive testing:

- **P1**: Automated stress tests and memory integrity verification
- **P2**: Sample text files and edge case testing
- **P3**: Shell script test suites for all features
- **P4**: Network client for server testing

## Memory Management

All projects have been tested with Valgrind to ensure:
- No memory leaks
- Proper memory allocation/deallocation
- Buffer overflow protection

## Authors

**Ryan Amir** (NetID: rma167)  
**Gian Cases** (NetID: grc74)

## Course Information

**Course:** CS 214 - Systems Programming  
**Semester:** Spring 2025

## License

This repository is for educational purposes as part of coursework for CS 214 Systems Programming. 