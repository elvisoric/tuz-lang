#pragma once

#include "token.h"

#include <optional>
#include <string_view>
#include <vector>

namespace tuz {

class Lexer {
public:
  explicit Lexer(std::string_view source);

  // Tokenize the entire source and return all tokens
  std::vector<Token> tokenize();

  // Get next token (for streaming)
  Token next_token();

  // Peek at current character
  char peek() const { return current_; }

private:
  std::string_view source_;
  size_t position_;
  uint32_t line_;
  uint32_t column_;
  char current_;

  void advance();
  void skip_whitespace();
  void skip_comment();

  Token make_token(TokenType type);
  Token make_token(TokenType type, std::string_view text);

  Token identifier();
  Token number();
  Token string();

  static bool is_alpha(char c);
  static bool is_digit(char c);
  static bool is_alphanumeric(char c);
  static std::optional<TokenType> get_keyword(std::string_view text);
};

} // namespace tuz
