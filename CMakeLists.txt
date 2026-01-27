cmake_minimum_required(VERSION 3.20)
project(tuz VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Include LLVM headers
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# LLVM components we need
llvm_map_components_to_libnames(llvm_libs
    Core
    Support
    native
    nativecodegen
    mcjit
    interpreter
    ExecutionEngine
    target
)

# Include directories
include_directories(include)

# Source files
set(TUZ_SOURCES
    src/lexer.cpp
    src/parser.cpp
    src/ast.cpp
    src/codegen.cpp
    src/type.cpp
    src/driver.cpp
    src/main.cpp
)

# Create executable
add_executable(tuzc ${TUZ_SOURCES})

# Link LLVM
target_link_libraries(tuzc ${llvm_libs})

# Enable warnings
target_compile_options(tuzc PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Wno-unused-parameter
)

# Install
install(TARGETS tuzc DESTINATION bin)
