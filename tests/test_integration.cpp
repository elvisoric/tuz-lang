#include "test_framework.h"
#include "tuz/codegen.h"
#include "tuz/diagnostic.h"
#include "tuz/driver.h"
#include "tuz/lexer.h"
#include "tuz/parser.h"

#include <cstdio>
#include <fstream>

using namespace tuz;
using namespace tuz::test;

// Helper to create temp files
class TempFile {
public:
  TempFile(const std::string& content) {
    filename_ = "/tmp/tuz_test_" + std::to_string(rand()) + ".tz";
    std::ofstream f(filename_);
    f << content;
  }

  ~TempFile() { std::remove(filename_.c_str()); }

  const std::string& path() const { return filename_; }

private:
  std::string filename_;
};

// =============================================================================
// Lexer Integration Tests
// =============================================================================

TEST(lexer_tokenizes_simple_program) {
  std::string source = "fn main() -> int { return 0; }";
  Lexer lexer(source);

  auto tokens = lexer.tokenize();

  // Should have tokens: fn, main, (, ), ->, int, {, return, 0, ;, }, EOF
  TEST_ASSERT_TRUE(tokens.size() >= 10);

  // Check first token is FN
  TEST_ASSERT_TRUE(tokens[0].type == TokenType::FN);
}

TEST(lexer_handles_operators) {
  std::string source = "+ - * / % == != < > <= >= && ||";
  Lexer lexer(source);

  auto tokens = lexer.tokenize();

  // Should have all operators plus EOF
  TEST_ASSERT_TRUE(tokens.size() >= 13);
}

TEST(lexer_handles_comments) {
  std::string source = "// this is a comment\nfn main() {}";
  Lexer lexer(source);

  auto tokens = lexer.tokenize();

  // First token after comment should be FN
  TEST_ASSERT_TRUE(tokens[0].type == TokenType::FN);
}

TEST(lexer_tracks_line_numbers) {
  std::string source = "fn\nmain\n()";
  Lexer lexer(source);

  auto tokens = lexer.tokenize();

  // fn is on line 1, main on line 2, ( on line 3
  TEST_ASSERT_EQ(1u, tokens[0].line);
  TEST_ASSERT_EQ(2u, tokens[1].line);
  TEST_ASSERT_EQ(3u, tokens[2].line);
}

// =============================================================================
// Parser Integration Tests
// =============================================================================

TEST(parser_parses_function_declaration) {
  std::string source = "fn main() -> int { return 0; }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(1u, program.declarations.size());
  TEST_ASSERT_TRUE(program.declarations[0]->kind == DeclKind::Function);
}

TEST(parser_parses_multiple_functions) {
  std::string source = R"(
        fn add(a: int, b: int) -> int { return a + b; }
        fn main() -> int { return add(1, 2); }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(2u, program.declarations.size());
}

TEST(parser_parses_variable_declaration) {
  std::string source = "fn main() -> int { let x = 5; return x; }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(1u, program.declarations.size());
}

TEST(parser_parses_if_statement) {
  std::string source = "fn main() -> int { if true { return 1; } else { return 0; } }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(1u, program.declarations.size());
}

TEST(parser_parses_while_loop) {
  std::string source = "fn main() -> int { while false { return 0; } return 1; }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(1u, program.declarations.size());
}

TEST(parser_parses_for_loop) {
  std::string source = "fn main() -> int { for i = 0, 10 { return i; } return 0; }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(1u, program.declarations.size());
}

TEST(parser_parses_extern_function) {
  std::string source = "extern fn puts(s: *u8) -> i32;";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  auto program = parser.parse_program();

  TEST_ASSERT_EQ(1u, program.declarations.size());
  auto& func = static_cast<FunctionDecl&>(*program.declarations[0]);
  TEST_ASSERT_TRUE(func.is_extern);
}

TEST(parser_throws_on_missing_semicolon) {
  std::string source = "fn main() -> int { let x = 5 return x; }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  TEST_ASSERT_THROW(parser.parse_program(), ParseError);
}

TEST(parser_throws_on_unexpected_token) {
  std::string source = "fn main() -> int { let }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  TEST_ASSERT_THROW(parser.parse_program(), ParseError);
}

TEST(parser_error_has_location) {
  std::string source = "fn main() -> int { let }";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);

  try {
    parser.parse_program();
    TEST_ASSERT_FALSE(true); // Should have thrown
  } catch (const ParseError& e) {
    TEST_ASSERT_TRUE(e.line > 0);
    TEST_ASSERT_TRUE(e.column > 0);
  }
}

// =============================================================================
// CodeGen Integration Tests
// =============================================================================

TEST(codegen_generates_valid_program) {
  std::string source = R"(
        fn main() -> int {
            return 42;
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(codegen_generates_function_call) {
  std::string source = R"(
        fn add(a: int, b: int) -> int {
            return a + b;
        }
        fn main() -> int {
            return add(10, 20);
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(codegen_generates_variable_use) {
  std::string source = R"(
        fn main() -> int {
            let x = 10;
            let y = 20;
            return x + y;
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(codegen_generates_if_else) {
  std::string source = R"(
        fn main() -> int {
            if true {
                return 1;
            } else {
                return 0;
            }
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(codegen_generates_while_loop) {
  std::string source = R"(
        fn main() -> int {
            let mut i = 0;
            while i < 10 {
                i = i + 1;
            }
            return i;
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(codegen_throws_on_undefined_variable) {
  std::string source = R"(
        fn main() -> int {
            return undefined_var;
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_THROW(codegen.generate(program), CodeGenError);
}

TEST(codegen_throws_on_undefined_function) {
  std::string source = R"(
        fn main() -> int {
            return undefined_func();
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  TEST_ASSERT_THROW(codegen.generate(program), CodeGenError);
}

TEST(codegen_error_has_location_for_undefined_variable) {
  std::string source = R"(
        fn main() -> int {
            return undefined_var;
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  try {
    codegen.generate(program);
    TEST_ASSERT_FALSE(true); // Should have thrown
  } catch (const CodeGenError& e) {
    TEST_ASSERT_TRUE(e.has_location());
    TEST_ASSERT_TRUE(e.location().line > 0);
    TEST_ASSERT_TRUE(e.location().column > 0);
  }
}

TEST(codegen_error_has_location_for_undefined_function) {
  std::string source = R"(
        fn main() -> int {
            return foo();
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  try {
    codegen.generate(program);
    TEST_ASSERT_FALSE(true); // Should have thrown
  } catch (const CodeGenError& e) {
    TEST_ASSERT_TRUE(e.has_location());
    TEST_ASSERT_TRUE(e.location().line > 0);
    TEST_ASSERT_TRUE(e.location().column > 0);
  }
}

TEST(codegen_error_includes_name_in_message) {
  std::string source = R"(
        fn main() -> int {
            return my_undefined_var;
        }
    )";
  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();

  CodeGenerator codegen;
  try {
    codegen.generate(program);
    TEST_ASSERT_FALSE(true); // Should have thrown
  } catch (const CodeGenError& e) {
    std::string msg = e.what();
    TEST_ASSERT_TRUE(msg.find("my_undefined_var") != std::string::npos);
  }
}

// =============================================================================
// Full Pipeline Tests
// =============================================================================

TEST(full_pipeline_fibonacci) {
  std::string source = R"(
        fn factorial(n: int) -> int {
            if n <= 1 {
                return 1;
            }
            return n * factorial(n - 1);
        }
        
        fn main() -> int {
            return factorial(5);
        }
    )";

  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();
  CodeGenerator codegen;

  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(full_pipeline_pointers) {
  std::string source = R"(
        fn main() -> int {
            let x = 42;
            let ptr: *int = &x;
            return x;
        }
    )";

  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();
  CodeGenerator codegen;

  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST(full_pipeline_extern) {
  std::string source = R"(
        extern fn puts(s: *u8) -> i32;
        
        fn main() -> int {
            return 0;
        }
    )";

  Lexer lexer(source);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto program = parser.parse_program();
  CodeGenerator codegen;

  TEST_ASSERT_NO_THROW(codegen.generate(program));
}

TEST_MAIN()
