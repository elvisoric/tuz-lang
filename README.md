# tuz - A Toy Programming Lanugage 

tuz is a toy programming language built with LLVM and Kimi, designed to be simple yet expressive, with syntax inspired by Rust and C.

## Features

- **Strong static typing** with type inference
- **Functions** with explicit return types
- **Control flow**: if/else, while, for loops
- **Variables**: mutable and immutable
- **Basic types**: int (i32), float (f32), bool, void
- **Recursive functions**
- **LLVM-based code generation**

## Building

### Prerequisites

- CMake 3.20+
- LLVM 14+ (with development libraries)
- C++20 compatible compiler (Clang/GCC)
- Clang (for linking)

### macOS

```bash
# Install LLVM (if not already installed)
brew install llvm

# Set LLVM path
export LLVM_DIR=$(brew --prefix llvm)/lib/cmake/llvm

# Build
mkdir build && cd build
cmake ..
make -j

# Or install globally
sudo make install
```

### Linux (Ubuntu/Debian)

```bash
# Install LLVM
sudo apt-get install llvm-dev

# Build
mkdir build && cd build
cmake ..
make -j
```

## What's Next?

**Standard Library**: Currently you can use `extern fn` to call C library functions. A standard library for tuz could include:
- String manipulation functions
- Memory allocation (malloc/free wrapper)
- File I/O operations
- Collection types (arrays, maps)
- Error handling (Result/Option types)

You have two options:
1. **Continue with `extern`** - Keep using C libraries via extern declarations (good for interoperability)
2. **Build a standard library** - Write core functionality in tuz or C and link it (good for language identity)

## Language Syntax

### Functions

```rust
fn add(a: int, b: int) -> int {
    return a + b;
}

// Entry point
fn main() -> int {
    let result = add(10, 20);
    return result;
}
```

### Variables

```rust
fn main() -> int {
    let x = 10;         // Immutable variable
    let mut y = 20;     // Mutable variable
    y = y + x;          // Can modify mutable variables
    return y;
}
```

### Types

```rust
fn main() -> i32 {
    // Integer types
    let a: i8 = 127;      // 8-bit signed
    let b: i16 = 1000;    // 16-bit signed
    let c: i32 = 100000;  // 32-bit signed (default 'int')
    let d: i64 = 1000000; // 64-bit signed
    
    // Unsigned integer types
    let e: u8 = 255;      // 8-bit unsigned
    let f: u16 = 65535;   // 16-bit unsigned
    let g: u32 = 100000;  // 32-bit unsigned
    let h: u64 = 1000000; // 64-bit unsigned
    
    // Floating point
    let f1: f32 = 1.5;    // 32-bit float
    let f2: f64 = 3.14;   // 64-bit float (default 'float')
    
    // Pointers
    let ptr: *i32 = &c;   // Pointer to i32
    
    return 0;
}
```

### Control Flow

```rust
fn main() -> int {
    // If/else
    let x = 10;
    if x > 5 {
        return 1;
    } else {
        return 0;
    }
    
    // While loop
    let mut i = 0;
    while i < 10 {
        i = i + 1;
    }
    
    // For loop (range-based)
    let mut sum = 0;
    for i = 0, 10 {
        sum = sum + i;
    }
    
    return sum;
}
```

### External Functions (FFI)

```rust
// Declare external C functions
extern fn puts(s: *u8) -> i32;
extern fn malloc(size: u64) -> *u8;
extern fn free(ptr: *u8);

fn main() -> i32 {
    puts("Hello from C library!");
    return 0;
}
```

### Recursive Functions

```rust
fn factorial(n: int) -> int {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

fn main() -> int {
    return factorial(5);  // Returns 120
}
```

## Usage

### Compile a program

```bash
# Compile to executable
./tuzc program.tz -o program

# Compile with verbose output
./tuzc -v program.tz

# Emit LLVM IR only
./tuzc -S program.tz -o program.ll

# Emit object file only
./tuzc -c program.tz -o program.o

# Optimize (O2)
./tuzc -O2 program.tz -o program
```

### Run the compiled program

```bash
./program
echo $?  # View return code
```

## Project Structure

```
tuz/
├── CMakeLists.txt          # Build configuration
├── README.md               # This file
├── include/tuz/            # Header files
│   ├── token.h            # Token definitions
│   ├── lexer.h            # Lexical analyzer
│   ├── ast.h              # Abstract syntax tree
│   ├── type.h             # Type system
│   ├── parser.h           # Parser
│   ├── codegen.h          # LLVM code generator
│   └── driver.h           # Compiler driver
├── src/                    # Source files
│   ├── token.cpp
│   ├── lexer.cpp
│   ├── ast.cpp
│   ├── type.cpp
│   ├── parser.cpp
│   ├── codegen.cpp
│   ├── driver.cpp
│   └── main.cpp
└── examples/               # Example programs
    ├── hello.tz
    ├── fibonacci.tz
    ├── loops.tz
    └── print.tz            # External function example
```

## Architecture

The compiler follows a standard multi-pass design:

1. **Lexer**: Converts source code into tokens
2. **Parser**: Builds an Abstract Syntax Tree (AST) using recursive descent
3. **Code Generator**: Walks the AST and generates LLVM IR
4. **LLVM**: Optimizes and generates machine code

## Roadmap

- [x] Basic expressions and statements
- [x] Functions and control flow  
- [x] LLVM code generation
- [x] Extern/FFI support
- [x] Sized integer types (i8, i16, i32, i64, u8, u16, u32, u64)
- [x] Pointers
- [ ] Arrays and slices
- [ ] Structs
- [ ] String type
- [ ] Modules/imports
- [ ] Pattern matching
- [ ] Generics

## License

MIT License - Feel free to use this as a starting point for your own language!
