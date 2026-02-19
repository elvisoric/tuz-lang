cmake_minimum_required(VERSION 3.20)

# ============================================================================
# vcpkg Bootstrapping
# ============================================================================
# Check if vcpkg is already installed via environment variable or find it
if(DEFINED ENV{VCPKG_ROOT})
    set(VCPKG_ROOT "$ENV{VCPKG_ROOT}")
elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake")
    set(VCPKG_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg")
endif()

# If vcpkg not found, bootstrap it
if(NOT DEFINED VCPKG_ROOT OR NOT EXISTS "${VCPKG_ROOT}/vcpkg")
    message(STATUS "vcpkg not found. Bootstrapping vcpkg...")
    
    # Clone vcpkg repository
    execute_process(
        COMMAND git clone https://github.com/Microsoft/vcpkg.git
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        RESULT_VARIABLE GIT_RESULT
    )
    
    if(NOT GIT_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to clone vcpkg repository")
    endif()
    
    set(VCPKG_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg")
    
    # Run bootstrap script based on platform
    if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
        execute_process(
            COMMAND cmd /c bootstrap-vcpkg.bat
            WORKING_DIRECTORY ${VCPKG_ROOT}
            RESULT_VARIABLE BOOTSTRAP_RESULT
        )
    else()
        execute_process(
            COMMAND ./bootstrap-vcpkg.sh
            WORKING_DIRECTORY ${VCPKG_ROOT}
            RESULT_VARIABLE BOOTSTRAP_RESULT
        )
    endif()
    
    if(NOT BOOTSTRAP_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to bootstrap vcpkg")
    endif()
    
    message(STATUS "vcpkg bootstrapped successfully at ${VCPKG_ROOT}")
endif()

# Set the vcpkg toolchain file
set(CMAKE_TOOLCHAIN_FILE "${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake"
    CACHE STRING "Vcpkg toolchain file" FORCE)

# ============================================================================
# Project Definition
# ============================================================================
project(tuz VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ============================================================================
# Find LLVM via vcpkg
# ============================================================================
# First try to find LLVM via vcpkg
find_package(LLVM CONFIG QUIET)

# If not found via vcpkg, try system LLVM
if(NOT LLVM_FOUND)
    message(STATUS "LLVM not found via vcpkg, trying system LLVM...")
    find_package(LLVM REQUIRED CONFIG)
endif()

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

# ============================================================================
# Build Configuration
# ============================================================================
# Include directories
include_directories(include)

# Source files
set(TUZ_SOURCES
    src/token.cpp
    src/lexer.cpp
    src/parser.cpp
    src/ast.cpp
    src/codegen.cpp
    src/type.cpp
    src/driver.cpp
    src/diagnostic.cpp
    src/resolver.cpp
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

# ============================================================================
# Installation
# ============================================================================
install(TARGETS tuzc DESTINATION bin)

# ============================================================================
# Testing
# ============================================================================
enable_testing()

# Diagnostic unit tests
add_executable(test_diagnostic tests/test_diagnostic.cpp src/diagnostic.cpp)
target_include_directories(test_diagnostic PRIVATE include)
target_compile_options(test_diagnostic PRIVATE -Wall -Wextra)
add_test(NAME diagnostic COMMAND test_diagnostic)

# Integration tests (exclude main.cpp)
set(TUZ_LIB_SOURCES
    src/token.cpp
    src/lexer.cpp
    src/parser.cpp
    src/ast.cpp
    src/codegen.cpp
    src/type.cpp
    src/driver.cpp
    src/diagnostic.cpp
    src/resolver.cpp
)
add_executable(test_integration tests/test_integration.cpp ${TUZ_LIB_SOURCES})
target_include_directories(test_integration PRIVATE include ${LLVM_INCLUDE_DIRS})
target_compile_definitions(test_integration PRIVATE ${LLVM_DEFINITIONS})
target_link_libraries(test_integration ${llvm_libs})
target_compile_options(test_integration PRIVATE -Wall -Wextra -Wno-unused-parameter)
add_test(NAME integration COMMAND test_integration)
