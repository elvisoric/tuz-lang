#include "tuz/lexer.h"

#include <cctype>
#include <unordered_map>

namespace tuz {

Lexer::Lexer(std::string_view source)
    : source_(source), position_(0), line_(1), column_(1), current_(0) {
  if (!source_.empty()) {
    current_ = source_[0];
  }
}

std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;

  while (current_ != '\0') {
    tokens.push_back(next_token());
  }

  tokens.emplace_back(TokenType::END_OF_FILE, "", line_, column_);
  return tokens;
}

Token Lexer::next_token() {
  skip_whitespace();

  if (current_ == '\0') {
    return make_token(TokenType::END_OF_FILE);
  }

  uint32_t start_line = line_;
  uint32_t start_col = column_;

  // Comments
  if (current_ == '/' && position_ + 1 < source_.size()) {
    char next = source_[position_ + 1];
    if (next == '/' || next == '*') {
      skip_comment();
      return next_token();
    }
  }

  // Identifiers and keywords
  if (is_alpha(current_) || current_ == '_') {
    return identifier();
  }

  // Numbers
  if (is_digit(current_)) {
    return number();
  }

  // Strings
  if (current_ == '"') {
    return string();
  }

  // Multi-character operators
  switch (current_) {
  case '=':
    advance();
    if (current_ == '=') {
      advance();
      return Token(TokenType::EQ, "==", start_line, start_col);
    }
    return Token(TokenType::ASSIGN, "=", start_line, start_col);

  case '!':
    advance();
    if (current_ == '=') {
      advance();
      return Token(TokenType::NEQ, "!=", start_line, start_col);
    }
    return Token(TokenType::NOT, "!", start_line, start_col);

  case '<':
    advance();
    if (current_ == '=') {
      advance();
      return Token(TokenType::LEQ, "<=", start_line, start_col);
    }
    return Token(TokenType::LT, "<", start_line, start_col);

  case '>':
    advance();
    if (current_ == '=') {
      advance();
      return Token(TokenType::GEQ, ">=", start_line, start_col);
    }
    return Token(TokenType::GT, ">", start_line, start_col);

  case '&':
    advance();
    if (current_ == '&') {
      advance();
      return Token(TokenType::AND, "&&", start_line, start_col);
    }
    return Token(TokenType::AMPERSAND, "&", start_line, start_col);

  case '|':
    advance();
    if (current_ == '|') {
      advance();
      return Token(TokenType::OR, "||", start_line, start_col);
    }
    // Single | is not used in our language
    return make_token(TokenType::END_OF_FILE);

  case '-':
    advance();
    if (current_ == '>') {
      advance();
      return Token(TokenType::ARROW, "->", start_line, start_col);
    }
    return Token(TokenType::MINUS, "-", start_line, start_col);
  }

  // Single-character tokens
  switch (current_) {
  case '+':
    advance();
    return Token(TokenType::PLUS, "+", start_line, start_col);
  case '*':
    advance();
    return Token(TokenType::STAR, "*", start_line, start_col);
  case '/':
    advance();
    return Token(TokenType::SLASH, "/", start_line, start_col);
  case '%':
    advance();
    return Token(TokenType::PERCENT, "%", start_line, start_col);
  case '(':
    advance();
    return Token(TokenType::LPAREN, "(", start_line, start_col);
  case ')':
    advance();
    return Token(TokenType::RPAREN, ")", start_line, start_col);
  case '{':
    advance();
    return Token(TokenType::LBRACE, "{", start_line, start_col);
  case '}':
    advance();
    return Token(TokenType::RBRACE, "}", start_line, start_col);
  case '[':
    advance();
    return Token(TokenType::LBRACKET, "[", start_line, start_col);
  case ']':
    advance();
    return Token(TokenType::RBRACKET, "]", start_line, start_col);
  case ';':
    advance();
    return Token(TokenType::SEMICOLON, ";", start_line, start_col);
  case ':':
    advance();
    return Token(TokenType::COLON, ":", start_line, start_col);
  case ',':
    advance();
    return Token(TokenType::COMMA, ",", start_line, start_col);
  case '.':
    advance();
    return Token(TokenType::DOT, ".", start_line, start_col);
  }

  // Unknown character
  advance();
  return make_token(TokenType::END_OF_FILE);
}

void Lexer::advance() {
  if (current_ == '\n') {
    line_++;
    column_ = 1;
  } else {
    column_++;
  }

  position_++;
  if (position_ < source_.size()) {
    current_ = source_[position_];
  } else {
    current_ = '\0';
  }
}

void Lexer::skip_whitespace() {
  while (current_ == ' ' || current_ == '\t' || current_ == '\n' || current_ == '\r') {
    advance();
  }
}

void Lexer::skip_comment() {
  if (current_ == '/' && position_ + 1 < source_.size()) {
    if (source_[position_ + 1] == '/') {
      // Line comment
      while (current_ != '\0' && current_ != '\n') {
        advance();
      }
    } else if (source_[position_ + 1] == '*') {
      // Block comment
      advance(); // skip /
      advance(); // skip *
      while (current_ != '\0') {
        if (current_ == '*' && position_ + 1 < source_.size() && source_[position_ + 1] == '/') {
          advance();
          advance();
          break;
        }
        advance();
      }
    }
  }
}

Token Lexer::make_token(TokenType type) {
  return Token(type, std::string_view(&source_[position_ - 1], 1), line_, column_ - 1);
}

Token Lexer::make_token(TokenType type, std::string_view text) {
  return Token(type, text, line_, column_ - static_cast<uint32_t>(text.length()));
}

Token Lexer::identifier() {
  uint32_t start_line = line_;
  uint32_t start_col = column_;
  size_t start_pos = position_;

  while (is_alphanumeric(current_) || current_ == '_') {
    advance();
  }

  std::string_view text(source_.data() + start_pos, position_ - start_pos);

  auto keyword = get_keyword(text);
  if (keyword) {
    return Token(*keyword, text, start_line, start_col);
  }

  return Token(TokenType::IDENTIFIER, text, start_line, start_col);
}

Token Lexer::number() {
  uint32_t start_line = line_;
  uint32_t start_col = column_;
  size_t start_pos = position_;
  bool is_float = false;

  while (is_digit(current_)) {
    advance();
  }

  if (current_ == '.') {
    is_float = true;
    advance();
    while (is_digit(current_)) {
      advance();
    }
  }

  // Exponent
  if (current_ == 'e' || current_ == 'E') {
    is_float = true;
    advance();
    if (current_ == '+' || current_ == '-') {
      advance();
    }
    while (is_digit(current_)) {
      advance();
    }
  }

  std::string_view text(source_.data() + start_pos, position_ - start_pos);

  if (is_float) {
    return Token(TokenType::FLOAT_LITERAL, text, start_line, start_col);
  }
  return Token(TokenType::INTEGER_LITERAL, text, start_line, start_col);
}

Token Lexer::string() {
  uint32_t start_line = line_;
  uint32_t start_col = column_;
  advance(); // skip opening quote

  std::string value;
  while (current_ != '"' && current_ != '\0') {
    if (current_ == '\\') {
      advance();
      switch (current_) {
      case 'n':
        value += '\n';
        break;
      case 't':
        value += '\t';
        break;
      case 'r':
        value += '\r';
        break;
      case '\\':
        value += '\\';
        break;
      case '"':
        value += '"';
        break;
      case '0':
        value += '\0';
        break;
      default:
        value += current_;
        break;
      }
    } else {
      value += current_;
    }
    advance();
  }

  if (current_ == '"') {
    advance(); // skip closing quote
  }

  return Token(TokenType::STRING_LITERAL, value, start_line, start_col);
}

bool Lexer::is_alpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

bool Lexer::is_digit(char c) {
  return c >= '0' && c <= '9';
}

bool Lexer::is_alphanumeric(char c) {
  return is_alpha(c) || is_digit(c);
}

std::optional<TokenType> Lexer::get_keyword(std::string_view text) {
  static const std::unordered_map<std::string_view, TokenType> keywords = {
      {"fn", TokenType::FN},     {"let", TokenType::LET},       {"mut", TokenType::MUT},
      {"if", TokenType::IF},     {"else", TokenType::ELSE},     {"while", TokenType::WHILE},
      {"for", TokenType::FOR},   {"return", TokenType::RETURN}, {"struct", TokenType::STRUCT},
      {"true", TokenType::TRUE}, {"false", TokenType::FALSE},   {"extern", TokenType::EXTERN},
      {"int", TokenType::INT},   {"float", TokenType::FLOAT},   {"bool", TokenType::BOOL},
      {"void", TokenType::VOID}, {"i8", TokenType::I8},         {"i16", TokenType::I16},
      {"i32", TokenType::I32},   {"i64", TokenType::I64},       {"u8", TokenType::U8},
      {"u16", TokenType::U16},   {"u32", TokenType::U32},       {"u64", TokenType::U64},
      {"f32", TokenType::F32},   {"f64", TokenType::F64},
  };

  auto it = keywords.find(text);
  if (it != keywords.end()) {
    return it->second;
  }
  return std::nullopt;
}

} // namespace tuz
