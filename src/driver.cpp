#include "tuz/driver.h"

#include "tuz/ast.h"
#include "tuz/codegen.h"
#include "tuz/diagnostic.h"
#include "tuz/lexer.h"
#include "tuz/parser.h"
#include "tuz/resolver.h"

#include <cstdlib>
#include <iostream>

namespace tuz {

bool Driver::compile(const CompileOptions& options) {
  // Set up diagnostic system
  auto source_manager = std::make_shared<SourceManager>();
  auto source_file = source_manager->load_file(options.input_file);
  if (!source_file) {
    std::cerr << "Error: Could not open file: " << options.input_file << std::endl;
    return false;
  }
  source_manager->set_main_file(source_file);

  // Create diagnostic engine with console output
  auto& diagnostics = get_global_diagnostics();
  diagnostics.set_source_manager(source_manager);
  diagnostics.set_consumer(std::make_unique<ConsoleDiagnosticConsumer>(true, true));
  diagnostics.reset();

  if (options.verbose) {
    std::cout << "Compiling: " << options.input_file << std::endl;
  }

  // Lexing
  if (options.verbose)
    std::cout << "  Lexing..." << std::endl;
  Lexer lexer(source_file->content());
  std::vector<Token> tokens;
  try {
    tokens = lexer.tokenize();
  } catch (const std::exception& e) {
    diagnostics.error(e.what());
    return false;
  }

  if (options.verbose) {
    std::cout << "  Tokens: " << tokens.size() << std::endl;
  }

  // Parsing
  if (options.verbose)
    std::cout << "  Parsing..." << std::endl;
  Parser parser(tokens);
  Program program;
  try {
    program = parser.parse_program();
  } catch (const ParseError& e) {
    diagnostics.error(e.what(), SourceLocation(e.line, e.column), source_file);
    return false;
  } catch (const std::exception& e) {
    diagnostics.error(e.what());
    return false;
  }

  if (options.verbose) {
    std::cout << "  Declarations: " << program.declarations.size() << std::endl;
  }

  // Resolve types and/or function overloads
  try {
    Resolver resolver(program);
    resolver.resolve();
  } catch (const std::exception& e) {
    diagnostics.error(e.what());
    return false;
  }

  // Code generation
  if (options.verbose)
    std::cout << "  Generating LLVM IR..." << std::endl;
  CodeGenerator codegen;
  try {
    codegen.generate(program);
  } catch (const CodeGenError& e) {
    if (e.has_location()) {
      diagnostics.error(e.what(), e.location(), source_file);
    } else {
      diagnostics.error(e.what());
    }
    return false;
  } catch (const std::exception& e) {
    diagnostics.error(e.what());
    return false;
  }

  // Output
  if (options.emit_llvm) {
    std::string ll_file = options.output_file + ".ll";
    if (options.verbose)
      std::cout << "  Writing LLVM IR to: " << ll_file << std::endl;
    codegen.dump_ir(ll_file);
    return true;
  }

  // Compile to object file
  std::string obj_file = options.output_file + ".o";
  if (options.verbose)
    std::cout << "  Generating object file: " << obj_file << std::endl;
  codegen.compile_to_object(obj_file);

  if (options.emit_object) {
    return true;
  }

  // Link to executable
  return link_object(obj_file, options);
}

bool Driver::link_object(const std::string& obj_file, const CompileOptions& options) {
  if (options.verbose)
    std::cout << "  Linking..." << std::endl;

  // Build link command
  std::string cmd = "clang " + obj_file + " -o " + options.output_file;

  // Add optimization flags
  if (options.optimize) {
    cmd += " -O" + std::to_string(options.opt_level);
  }

  // Add library paths
  for (const auto& path : options.library_paths) {
    cmd += " -L" + path;
  }

  // Add libraries
  for (const auto& lib : options.libraries) {
    cmd += " -l" + lib;
  }

  // On windows c library is linked automatically 
  #ifndef _WIN32
    // Link with standard C library
    cmd += " -lc";
  #endif

  if (options.verbose)
    std::cout << "  Command: " << cmd << std::endl;

  int result = std::system(cmd.c_str());

  // Clean up object file
  std::remove(obj_file.c_str());

  if (result != 0) {
    std::cerr << "Linking failed" << std::endl;
    return false;
  }

  if (options.verbose) {
    std::cout << "Output: " << options.output_file << std::endl;
  }

  return true;
}

static void print_usage(const char* program) {
  std::cout << "Usage: " << program << " [options] <input file>" << std::endl;
  std::cout << std::endl;
  std::cout << "Options:" << std::endl;
  std::cout << "  -o <file>     Output file name (default: a.out)" << std::endl;
  std::cout << "  -S            Emit LLVM IR only" << std::endl;
  std::cout << "  -c            Emit object file only" << std::endl;
  std::cout << "  -O<level>     Optimization level (0-3)" << std::endl;
  std::cout << "  -v            Verbose output" << std::endl;
  std::cout << "  -L<path>      Add library search path" << std::endl;
  std::cout << "  -l<lib>       Link with library" << std::endl;
  std::cout << "  -h, --help    Show this help message" << std::endl;
}

int Driver::run(int argc, char** argv) {
  if (argc < 2) {
    print_usage(argv[0]);
    return 1;
  }

  CompileOptions options;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];

    if (arg == "-h" || arg == "--help") {
      print_usage(argv[0]);
      return 0;
    } else if (arg == "-o" && i + 1 < argc) {
      options.output_file = argv[++i];
    } else if (arg == "-S") {
      options.emit_llvm = true;
    } else if (arg == "-c") {
      options.emit_object = true;
    } else if (arg == "-v") {
      options.verbose = true;
    } else if (arg.size() > 2 && arg[0] == '-' && arg[1] == 'O') {
      options.optimize = true;
      options.opt_level = std::stoi(arg.substr(2));
    } else if (arg.size() > 2 && arg[0] == '-' && arg[1] == 'L') {
      options.library_paths.push_back(arg.substr(2));
    } else if (arg.size() > 2 && arg[0] == '-' && arg[1] == 'l') {
      options.libraries.push_back(arg.substr(2));
    } else if (arg[0] != '-') {
      options.input_file = arg;
    } else {
      std::cerr << "Unknown option: " << arg << std::endl;
      return 1;
    }
  }

  if (options.input_file.empty()) {
    std::cerr << "Error: No input file specified" << std::endl;
    return 1;
  }

  bool success = compile(options);
  return success ? 0 : 1;
}

} // namespace tuz
