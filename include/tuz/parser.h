#pragma once

#include "ast.h"
#include "token.h"
#include "type.h"

#include <stdexcept>
#include <string>
#include <vector>

namespace tuz {

class ParseError : public std::runtime_error {
public:
  uint32_t line;
  uint32_t column;

  ParseError(const std::string& msg, uint32_t ln, uint32_t col)
      : std::runtime_error(msg), line(ln), column(col) {}
};

class Parser {
public:
  explicit Parser(std::vector<Token> tokens);

  // Parse a complete program
  Program parse_program();

  // Parse individual elements (for REPL/testing)
  DeclPtr parse_declaration();
  StmtPtr parse_statement();
  ExprPtr parse_expression();
  TypePtr parse_type_annotation();

private:
  std::vector<Token> tokens_;
  size_t position_;

  // Token access
  Token& current();
  Token& peek(size_t offset = 0);
  Token& previous();
  bool is_at_end();

  // Consumption
  Token& advance();
  Token& expect(TokenType type, const std::string& message);
  bool match(TokenType type);
  bool match(std::initializer_list<TokenType> types);
  bool check(TokenType type);

  // Error handling
  ParseError error(const std::string& message);
  ParseError error(const Token& token, const std::string& message);

  // Synchronization for error recovery
  void synchronize();

  // Declarations
  DeclPtr parse_extern_decl();
  DeclPtr parse_function_decl();
  DeclPtr parse_struct_decl();
  DeclPtr parse_global_decl();

  // Statements
  StmtPtr parse_stmt();
  StmtPtr parse_expr_stmt();
  StmtPtr parse_let_stmt();
  StmtPtr parse_assign_or_expr_stmt();
  StmtPtr parse_block_stmt();
  StmtPtr parse_if_stmt();
  StmtPtr parse_while_stmt();
  StmtPtr parse_for_stmt();
  StmtPtr parse_return_stmt();

  // Expressions - using Pratt parser / precedence climbing
  ExprPtr parse_expr();
  ExprPtr parse_assignment();
  ExprPtr parse_or();
  ExprPtr parse_and();
  ExprPtr parse_equality();
  ExprPtr parse_comparison();
  ExprPtr parse_term();
  ExprPtr parse_factor();
  ExprPtr parse_unary();
  ExprPtr parse_postfix();
  ExprPtr parse_primary();

  // Helpers
  TypePtr parse_type();
  std::vector<Param> parse_param_list();
  std::vector<ExprPtr> parse_arg_list();
  std::vector<Field> parse_field_list();

  // Precedence levels for expression parsing
  static int get_precedence(TokenType type);
};

} // namespace tuz
