#pragma once

#include <string>
#include <vector>

namespace tuz {

// Compiler options
struct CompileOptions {
    std::string input_file;
    std::string output_file = "a.out";
    bool emit_llvm = false;
    bool emit_object = false;
    bool optimize = false;
    int opt_level = 0;  // 0-3
    bool verbose = false;
    std::vector<std::string> library_paths;
    std::vector<std::string> libraries;
    std::string target_triple;
};

class Driver {
public:
    // Compile a source file to an executable
    static bool compile(const CompileOptions& options);
    
    // Run the compiler with command line arguments
    static int run(int argc, char** argv);
    
private:
    // Helper to link object files
    static bool link_object(const std::string& obj_file, const CompileOptions& options);
};

} // namespace tuz
