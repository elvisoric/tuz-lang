#include "tuz/parser.h"

#include <iostream>

namespace tuz {

Parser::Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)), position_(0) {
}

Program Parser::parse_program() {
  Program program;

  while (!is_at_end()) {
    program.declarations.push_back(parse_declaration());
  }

  return program;
}

DeclPtr Parser::parse_declaration() {
  try {
    if (match(TokenType::EXTERN)) {
      return parse_extern_decl();
    }
    if (match(TokenType::FN)) {
      return parse_function_decl();
    }
    if (match(TokenType::STRUCT)) {
      return parse_struct_decl();
    }
    if (match(TokenType::LET)) {
      return parse_global_decl();
    }

    throw error("Expected declaration (extern, fn, struct, or let)");
  } catch (const ParseError& e) {
    synchronize();
    throw;
  }
}

// =============================================================================
// Declarations
// =============================================================================

DeclPtr Parser::parse_extern_decl() {
  auto loc = previous().location();

  expect(TokenType::FN, "Expected 'fn' after 'extern'");

  Token& name_token = expect(TokenType::IDENTIFIER, "Expected function name");
  std::string name(name_token.text);

  expect(TokenType::LPAREN, "Expected '(' after function name");
  std::vector<Param> params = parse_param_list();
  expect(TokenType::RPAREN, "Expected ')' after parameters");

  TypePtr return_type = get_void_type();
  if (match(TokenType::ARROW)) {
    return_type = parse_type();
  }

  expect(TokenType::SEMICOLON, "Expected ';' after extern function declaration");

  return std::make_shared<FunctionDecl>(name, std::move(params), return_type, nullptr, true, loc);
}

DeclPtr Parser::parse_function_decl() {
  auto loc = previous().location();

  Token& name_token = expect(TokenType::IDENTIFIER, "Expected function name");
  std::string name(name_token.text);

  expect(TokenType::LPAREN, "Expected '(' after function name");
  std::vector<Param> params = parse_param_list();
  expect(TokenType::RPAREN, "Expected ')' after parameters");

  TypePtr return_type = get_void_type();
  if (match(TokenType::ARROW)) {
    return_type = parse_type();
  }

  bool is_extern = false;
  StmtPtr body = nullptr;

  if (check(TokenType::SEMICOLON)) {
    // Extern function declaration
    is_extern = true;
    advance();
  } else {
    body = parse_block_stmt();
  }

  return std::make_shared<FunctionDecl>(name, std::move(params), return_type, std::move(body),
                                        is_extern, loc);
}

DeclPtr Parser::parse_struct_decl() {
  auto loc = previous().location();

  Token& name_token = expect(TokenType::IDENTIFIER, "Expected struct name");
  std::string name(name_token.text);

  expect(TokenType::LBRACE, "Expected '{' after struct name");
  std::vector<Field> fields = parse_field_list();
  expect(TokenType::RBRACE, "Expected '}' after struct fields");

  return std::make_shared<StructDecl>(name, std::move(fields), loc);
}

DeclPtr Parser::parse_global_decl() {
  auto loc = previous().location();

  bool is_mutable = match(TokenType::MUT);

  Token& name_token = expect(TokenType::IDENTIFIER, "Expected variable name");
  std::string name(name_token.text);

  TypePtr type = nullptr;
  if (match(TokenType::COLON)) {
    type = parse_type();
  }

  ExprPtr initializer = nullptr;
  if (match(TokenType::ASSIGN)) {
    initializer = parse_expression();
  }

  expect(TokenType::SEMICOLON, "Expected ';' after global declaration");

  return std::make_shared<GlobalDecl>(name, type, std::move(initializer), is_mutable, loc);
}

// =============================================================================
// Statements
// =============================================================================

StmtPtr Parser::parse_statement() {
  if (check(TokenType::LBRACE)) {
    return parse_block_stmt();
  }
  if (match(TokenType::LET)) {
    return parse_let_stmt();
  }
  if (match(TokenType::IF)) {
    return parse_if_stmt();
  }
  if (match(TokenType::WHILE)) {
    return parse_while_stmt();
  }
  if (match(TokenType::FOR)) {
    return parse_for_stmt();
  }
  if (match(TokenType::RETURN)) {
    return parse_return_stmt();
  }

  return parse_assign_or_expr_stmt();
}

StmtPtr Parser::parse_block_stmt() {
  auto loc = previous().location();

  // If we didn't consume the '{' in match, consume it now
  if (previous().type != TokenType::LBRACE) {
    expect(TokenType::LBRACE, "Expected '{'");
  }

  std::vector<StmtPtr> statements;
  while (!check(TokenType::RBRACE) && !is_at_end()) {
    statements.push_back(parse_statement());
  }

  expect(TokenType::RBRACE, "Expected '}' after block");

  return std::make_shared<BlockStmt>(std::move(statements), loc);
}

StmtPtr Parser::parse_let_stmt() {
  auto loc = previous().location();

  bool is_mutable = match(TokenType::MUT);

  Token& name_token = expect(TokenType::IDENTIFIER, "Expected variable name");
  std::string name(name_token.text);

  TypePtr type = nullptr;
  if (match(TokenType::COLON)) {
    type = parse_type();
  }

  ExprPtr initializer = nullptr;
  if (match(TokenType::ASSIGN)) {
    initializer = parse_expression();
  }

  expect(TokenType::SEMICOLON, "Expected ';' after let statement");

  return std::make_shared<LetStmt>(name, type, std::move(initializer), is_mutable, loc);
}

StmtPtr Parser::parse_assign_or_expr_stmt() {
  auto loc = current().location();

  ExprPtr expr = parse_expression();

  if (match(TokenType::ASSIGN)) {
    ExprPtr value = parse_expression();
    expect(TokenType::SEMICOLON, "Expected ';' after assignment");
    return std::make_shared<AssignStmt>(std::move(expr), std::move(value), loc);
  }

  expect(TokenType::SEMICOLON, "Expected ';' after expression");
  return std::make_shared<ExprStmt>(std::move(expr), loc);
}

StmtPtr Parser::parse_if_stmt() {
  auto loc = previous().location();

  ExprPtr condition = parse_expression();
  StmtPtr then_branch = parse_statement();

  StmtPtr else_branch = nullptr;
  if (match(TokenType::ELSE)) {
    else_branch = parse_statement();
  }

  return std::make_shared<IfStmt>(std::move(condition), std::move(then_branch),
                                  std::move(else_branch), loc);
}

StmtPtr Parser::parse_while_stmt() {
  auto loc = previous().location();

  ExprPtr condition = parse_expression();
  StmtPtr body = parse_statement();

  return std::make_shared<WhileStmt>(std::move(condition), std::move(body), loc);
}

StmtPtr Parser::parse_for_stmt() {
  auto loc = previous().location();

  Token& var_token = expect(TokenType::IDENTIFIER, "Expected loop variable name");
  std::string var_name(var_token.text);

  expect(TokenType::ASSIGN, "Expected '=' after loop variable");
  ExprPtr range_start = parse_expression();

  expect(TokenType::COMMA, "Expected ',' after range start");
  ExprPtr range_end = parse_expression();

  StmtPtr body = parse_statement();

  return std::make_shared<ForStmt>(var_name, std::move(range_start), std::move(range_end),
                                   std::move(body), loc);
}

StmtPtr Parser::parse_return_stmt() {
  auto loc = previous().location();

  ExprPtr value = nullptr;
  if (!check(TokenType::SEMICOLON)) {
    value = parse_expression();
  }

  expect(TokenType::SEMICOLON, "Expected ';' after return");

  return std::make_shared<ReturnStmt>(std::move(value), loc);
}

// =============================================================================
// Expressions (using precedence climbing)
// =============================================================================

ExprPtr Parser::parse_expression() {
  return parse_assignment();
}

ExprPtr Parser::parse_assignment() {
  ExprPtr expr = parse_or();
  return expr;
}

ExprPtr Parser::parse_or() {
  ExprPtr expr = parse_and();

  while (match(TokenType::OR)) {
    auto loc = previous().location();
    ExprPtr right = parse_and();
    expr = std::make_shared<BinaryOpExpr>(BinaryOp::Or, std::move(expr), std::move(right), loc);
  }

  return expr;
}

ExprPtr Parser::parse_and() {
  ExprPtr expr = parse_equality();

  while (match(TokenType::AND)) {
    auto loc = previous().location();
    ExprPtr right = parse_equality();
    expr = std::make_shared<BinaryOpExpr>(BinaryOp::And, std::move(expr), std::move(right), loc);
  }

  return expr;
}

ExprPtr Parser::parse_equality() {
  ExprPtr expr = parse_comparison();

  while (true) {
    auto loc = previous().location();

    if (match(TokenType::EQ)) {
      ExprPtr right = parse_comparison();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Eq, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::NEQ)) {
      ExprPtr right = parse_comparison();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Neq, std::move(expr), std::move(right), loc);
    } else {
      break;
    }
  }

  return expr;
}

ExprPtr Parser::parse_comparison() {
  ExprPtr expr = parse_term();

  while (true) {
    auto loc = current().location();

    if (match(TokenType::LT)) {
      ExprPtr right = parse_term();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Lt, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::GT)) {
      ExprPtr right = parse_term();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Gt, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::LEQ)) {
      ExprPtr right = parse_term();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Leq, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::GEQ)) {
      ExprPtr right = parse_term();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Geq, std::move(expr), std::move(right), loc);
    } else {
      break;
    }
  }

  return expr;
}

ExprPtr Parser::parse_term() {
  ExprPtr expr = parse_factor();

  while (true) {
    auto loc = current().location();

    if (match(TokenType::PLUS)) {
      ExprPtr right = parse_factor();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Add, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::MINUS)) {
      ExprPtr right = parse_factor();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Sub, std::move(expr), std::move(right), loc);
    } else {
      break;
    }
  }

  return expr;
}

ExprPtr Parser::parse_factor() {
  ExprPtr expr = parse_unary();

  while (true) {
    auto loc = current().location();

    if (match(TokenType::STAR)) {
      ExprPtr right = parse_unary();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Mul, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::SLASH)) {
      ExprPtr right = parse_unary();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Div, std::move(expr), std::move(right), loc);
    } else if (match(TokenType::PERCENT)) {
      ExprPtr right = parse_unary();
      expr = std::make_shared<BinaryOpExpr>(BinaryOp::Mod, std::move(expr), std::move(right), loc);
    } else {
      break;
    }
  }

  return expr;
}

ExprPtr Parser::parse_unary() {
  auto loc = current().location();

  if (match(TokenType::NOT)) {
    ExprPtr operand = parse_unary();
    return std::make_shared<UnaryOpExpr>(UnaryOp::Not, std::move(operand), loc);
  }

  if (match(TokenType::MINUS)) {
    ExprPtr operand = parse_unary();
    return std::make_shared<UnaryOpExpr>(UnaryOp::Neg, std::move(operand), loc);
  }

  if (match(TokenType::AMPERSAND)) {
    ExprPtr operand = parse_unary();
    return std::make_shared<UnaryOpExpr>(UnaryOp::AddrOf, std::move(operand), loc);
  }

  if (match(TokenType::STAR)) {
    ExprPtr operand = parse_unary();
    return std::make_shared<UnaryOpExpr>(UnaryOp::Deref, std::move(operand), loc);
  }

  return parse_postfix();
}

ExprPtr Parser::parse_postfix() {
  ExprPtr expr = parse_primary();

  while (true) {
    auto loc = current().location();

    if (match(TokenType::LPAREN)) {
      // Function call
      std::vector<ExprPtr> args = parse_arg_list();
      expect(TokenType::RPAREN, "Expected ')' after arguments");
      expr = std::make_shared<CallExpr>(std::move(expr), std::move(args), loc);
    } else if (match(TokenType::LBRACKET)) {
      // Array index
      ExprPtr index = parse_expression();
      expect(TokenType::RBRACKET, "Expected ']' after index");
      expr = std::make_shared<IndexExpr>(std::move(expr), std::move(index), loc);
    } else if (match(TokenType::DOT)) {
      // Field access
      Token& field_token = expect(TokenType::IDENTIFIER, "Expected field name");
      std::string field_name(field_token.text);
      expr = std::make_shared<FieldAccessExpr>(std::move(expr), field_name, loc);
    } else {
      break;
    }
  }

  return expr;
}

ExprPtr Parser::parse_primary() {
  auto loc = current().location();

  if (match(TokenType::INTEGER_LITERAL)) {
    int64_t value = std::stoll(std::string(previous().text));
    return std::make_shared<IntegerLiteralExpr>(value, loc);
  }

  if (match(TokenType::FLOAT_LITERAL)) {
    double value = std::stod(std::string(previous().text));
    return std::make_shared<FloatLiteralExpr>(value, loc);
  }

  if (match(TokenType::TRUE)) {
    return std::make_shared<BoolLiteralExpr>(true, loc);
  }

  if (match(TokenType::FALSE)) {
    return std::make_shared<BoolLiteralExpr>(false, loc);
  }

  if (match(TokenType::STRING_LITERAL)) {
    return std::make_shared<StringLiteralExpr>(std::string(previous().text), loc);
  }

  if (match(TokenType::IDENTIFIER)) {
    return std::make_shared<VariableExpr>(std::string(previous().text), loc);
  }

  if (match(TokenType::LPAREN)) {
    ExprPtr expr = parse_expression();
    expect(TokenType::RPAREN, "Expected ')' after expression");
    return expr;
  }

  throw error("Expected expression");
}

// =============================================================================
// Type parsing
// =============================================================================

TypePtr Parser::parse_type() {

  // Handle pointer types with prefix *
  if (match(TokenType::STAR)) {
    TypePtr pointee = parse_type();
    return PointerType::get(pointee);
  }

  TypePtr base_type = nullptr;

  if (match(TokenType::INT)) {
    base_type = get_int32_type();
  } else if (match(TokenType::I8)) {
    base_type = get_int8_type();
  } else if (match(TokenType::I16)) {
    base_type = get_int16_type();
  } else if (match(TokenType::I32)) {
    base_type = get_int32_type();
  } else if (match(TokenType::I64)) {
    base_type = get_int64_type();
  } else if (match(TokenType::U8)) {
    base_type = get_uint8_type();
  } else if (match(TokenType::U16)) {
    base_type = get_uint16_type();
  } else if (match(TokenType::U32)) {
    base_type = get_uint32_type();
  } else if (match(TokenType::U64)) {
    base_type = get_uint64_type();
  } else if (match(TokenType::FLOAT)) {
    base_type = get_float64_type();
  } else if (match(TokenType::F32)) {
    base_type = get_float32_type();
  } else if (match(TokenType::F64)) {
    base_type = get_float64_type();
  } else if (match(TokenType::BOOL)) {
    base_type = get_bool_type();
  } else if (match(TokenType::VOID)) {
    base_type = get_void_type();
  } else if (match(TokenType::IDENTIFIER)) {

    std::string name(previous().text);
    base_type = make_shared<TypeName>(name);
  } else {
    throw error("Expected type");
  }

  // Handle pointer types
  while (match(TokenType::STAR)) {
    base_type = PointerType::get(base_type);
  }

  return base_type;
}

std::vector<Param> Parser::parse_param_list() {
  std::vector<Param> params;

  if (!check(TokenType::RPAREN)) {
    do {
      Token& name_token = expect(TokenType::IDENTIFIER, "Expected parameter name");
      std::string name(name_token.text);
      expect(TokenType::COLON, "Expected ':' after parameter name");
      TypePtr type = parse_type();
      params.emplace_back(name, type);
    } while (match(TokenType::COMMA));
  }

  return params;
}

std::vector<ExprPtr> Parser::parse_arg_list() {
  std::vector<ExprPtr> args;

  if (!check(TokenType::RPAREN)) {
    do {
      args.push_back(parse_expression());
    } while (match(TokenType::COMMA));
  }

  return args;
}

std::vector<Field> Parser::parse_field_list() {
  std::vector<Field> fields;

  if (!check(TokenType::RBRACE)) {
    do {
      Token& name_token = expect(TokenType::IDENTIFIER, "Expected field name");
      std::string name(name_token.text);
      expect(TokenType::COLON, "Expected ':' after field name");
      TypePtr type = parse_type();
      fields.emplace_back(name, type);
    } while (match(TokenType::COMMA));
  }

  return fields;
}

// =============================================================================
// Helpers
// =============================================================================

Token& Parser::current() {
  return tokens_[position_];
}

Token& Parser::peek(size_t offset) {
  size_t idx = position_ + offset;
  if (idx >= tokens_.size()) {
    return tokens_.back();
  }
  return tokens_[idx];
}

Token& Parser::previous() {
  return tokens_[position_ - 1];
}

bool Parser::is_at_end() {
  return current().type == TokenType::END_OF_FILE;
}

Token& Parser::advance() {
  if (!is_at_end())
    position_++;
  return previous();
}

Token& Parser::expect(TokenType type, const std::string& message) {
  if (current().type == type) {
    return advance();
  }
  throw error(message);
}

bool Parser::match(TokenType type) {
  if (check(type)) {
    advance();
    return true;
  }
  return false;
}

bool Parser::match(std::initializer_list<TokenType> types) {
  for (TokenType type : types) {
    if (match(type))
      return true;
  }
  return false;
}

bool Parser::check(TokenType type) {
  if (is_at_end())
    return false;
  return current().type == type;
}

ParseError Parser::error(const std::string& message) {
  return ParseError(message, current().line, current().column);
}

ParseError Parser::error(const Token& token, const std::string& message) {
  return ParseError(message, token.line, token.column);
}

void Parser::synchronize() {
  advance();

  while (!is_at_end()) {
    if (previous().type == TokenType::SEMICOLON)
      return;

    switch (current().type) {
    case TokenType::FN:
    case TokenType::LET:
    case TokenType::FOR:
    case TokenType::IF:
    case TokenType::WHILE:
    case TokenType::RETURN:
    case TokenType::STRUCT:
      return;
    default:
      break;
    }

    advance();
  }
}

} // namespace tuz
