#include "tuz/token.h"

namespace tuz {

const std::vector<TokenDefinition> Keywords = {
    {TokenType::FN, "fn"},         {TokenType::LET, "let"},       {TokenType::MUT, "mut"},
    {TokenType::IF, "if"},         {TokenType::ELSE, "else"},     {TokenType::WHILE, "while"},
    {TokenType::FOR, "for"},       {TokenType::RETURN, "return"}, {TokenType::STRUCT, "struct"},
    {TokenType::EXTERN, "extern"}, {TokenType::TRUE, "true"},     {TokenType::FALSE, "false"},
    {TokenType::INT, "int"},       {TokenType::FLOAT, "float"},   {TokenType::BOOL, "bool"},
    {TokenType::VOID, "void"},     {TokenType::I8, "i8"},         {TokenType::I16, "i16"},
    {TokenType::I32, "i32"},       {TokenType::I64, "i64"},       {TokenType::U8, "u8"},
    {TokenType::U16, "u16"},       {TokenType::U32, "u32"},       {TokenType::U64, "u64"},
    {TokenType::F32, "f32"},       {TokenType::F64, "f64"},
};

const std::vector<TokenDefinition> Tokens = {
    // Double char
    {TokenType::EQ, "=="},
    {TokenType::NEQ, "!="},
    {TokenType::LEQ, "<="},
    {TokenType::GEQ, ">="},
    {TokenType::AND, "&&"},
    {TokenType::OR, "||"},
    {TokenType::ARROW, "->"},

    // Single char
    {TokenType::NOT, "!"},
    {TokenType::AMPERSAND, "&"},
    {TokenType::LT, "<"},
    {TokenType::GT, ">"},
    {TokenType::PLUS, "+"},
    {TokenType::MINUS, "-"},
    {TokenType::STAR, "*"},
    {TokenType::SLASH, "/"},
    {TokenType::PERCENT, "%"},
    {TokenType::ASSIGN, "="},
    {TokenType::LPAREN, "("},
    {TokenType::RPAREN, ")"},
    {TokenType::LBRACE, "{"},
    {TokenType::RBRACE, "}"},
    {TokenType::LBRACKET, "["},
    {TokenType::SEMICOLON, ";"},
    {TokenType::RBRACKET, "]"},
    {TokenType::SEMICOLON, ";"},
    {TokenType::COLON, ":"},
    {TokenType::COMMA, ","},
    {TokenType::DOT, "."},
};

const std::vector<TokenDefinition> SpecialTokens = {
    {TokenType::END_OF_FILE, "EOF"},
    {TokenType::INTEGER_LITERAL, "INTEGER_LITERAL"},
    {TokenType::FLOAT_LITERAL, "FLOAT_LITERAL"},
    {TokenType::STRING_LITERAL, "STRING_LITERAL"},
    {TokenType::BOOLEAN_LITERAL, "BOOLEAN_LITERAL"},
    {TokenType::IDENTIFIER, "IDENTIFIER"},
};

TokenGroup get_token_group(TokenType type) {
  switch (type) {
  // Keywords
  case TokenType::FN:
  case TokenType::LET:
  case TokenType::MUT:
  case TokenType::IF:
  case TokenType::ELSE:
  case TokenType::WHILE:
  case TokenType::FOR:
  case TokenType::RETURN:
  case TokenType::STRUCT:
  case TokenType::EXTERN:
  case TokenType::TRUE:
  case TokenType::FALSE:
    return TokenGroup::KEYWORD;

  // Types
  case TokenType::INT:
  case TokenType::FLOAT:
  case TokenType::BOOL:
  case TokenType::VOID:
  case TokenType::I8:
  case TokenType::I16:
  case TokenType::I32:
  case TokenType::I64:
  case TokenType::U8:
  case TokenType::U16:
  case TokenType::U32:
  case TokenType::U64:
  case TokenType::F32:
  case TokenType::F64:
    return TokenGroup::TYPE;

  // Operators
  case TokenType::PLUS:
  case TokenType::MINUS:
  case TokenType::STAR:
  case TokenType::SLASH:
  case TokenType::PERCENT:
  case TokenType::ASSIGN:
  case TokenType::EQ:
  case TokenType::NEQ:
  case TokenType::LT:
  case TokenType::LEQ:
  case TokenType::GT:
  case TokenType::GEQ:
  case TokenType::AND:
  case TokenType::OR:
  case TokenType::NOT:
  case TokenType::ARROW:
  case TokenType::AMPERSAND:
    return TokenGroup::OPERATOR;

  // Delimiters
  case TokenType::LPAREN:
  case TokenType::RPAREN:
  case TokenType::LBRACE:
  case TokenType::RBRACE:
  case TokenType::LBRACKET:
  case TokenType::RBRACKET:
  case TokenType::SEMICOLON:
  case TokenType::COLON:
  case TokenType::COMMA:
  case TokenType::DOT:
    return TokenGroup::DELIMITER;

  // Literals
  case TokenType::INTEGER_LITERAL:
  case TokenType::FLOAT_LITERAL:
  case TokenType::STRING_LITERAL:
    return TokenGroup::LITERAL;

  // Other
  case TokenType::IDENTIFIER:
  case TokenType::END_OF_FILE:
    return TokenGroup::OTHER;

  default:
    return TokenGroup::OTHER;
  }
}

auto create_token_type_map() {
  std::unordered_map<TokenType, std::string_view> map;
  for (const auto& token : Keywords) {
    map[token.type] = token.value;
  }
  for (const auto& token : Tokens) {
    map[token.type] = token.value;
  }
  for (const auto& token : SpecialTokens) {
    map[token.type] = token.value;
  }
  return map;
}

auto create_keyword_to_token_type_map() {
  std::unordered_map<std::string_view, TokenType> map;
  for (const auto& token : Keywords) {
    map[token.value] = token.type;
  }
  return map;
}

static const auto TokenTypeMap = create_token_type_map();
static const auto KeywordToTokenTypeMap = create_keyword_to_token_type_map();

const char* token_type_to_string(TokenType type) {
  auto it = TokenTypeMap.find(type);
  if (it != TokenTypeMap.end()) {
    return it->second.data();
  }
  return nullptr;
}

std::optional<TokenType> get_keyword_token_type(std::string_view token) {
  auto it = KeywordToTokenTypeMap.find(token);
  return it != KeywordToTokenTypeMap.end() ? std::optional{it->second} : std::nullopt;
}

} // namespace tuz
