#pragma once

#include <cstdint>
#include <string>

namespace tuz {

// Token types for our language
enum class TokenType {
  // End of file
  END_OF_FILE,

  // Literals
  INTEGER_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,
  BOOLEAN_LITERAL,

  // Identifiers
  IDENTIFIER,

  // Keywords
  FN,     // fn
  LET,    // let
  MUT,    // mut
  IF,     // if
  ELSE,   // else
  WHILE,  // while
  FOR,    // for
  RETURN, // return
  STRUCT, // struct
  TRUE,   // true
  FALSE,  // false
  EXTERN, // extern

  // Types
  INT,   // int
  FLOAT, // float
  BOOL,  // bool
  VOID,  // void
  I8,
  I16,
  I32,
  I64, // i8, i16, i32, i64
  U8,
  U16,
  U32,
  U64, // u8, u16, u32, u64
  F32,
  F64, // f32, f64

  // Operators
  PLUS,      // +
  MINUS,     // -
  STAR,      // *
  SLASH,     // /
  PERCENT,   // %
  ASSIGN,    // =
  EQ,        // ==
  NEQ,       // !=
  LT,        // <
  GT,        // >
  LEQ,       // <=
  GEQ,       // >=
  AND,       // &&
  OR,        // ||
  NOT,       // !
  AMPERSAND, // &

  // Delimiters
  LPAREN,    // (
  RPAREN,    // )
  LBRACE,    // {
  RBRACE,    // }
  LBRACKET,  // [
  RBRACKET,  // ]
  SEMICOLON, // ;
  COLON,     // :
  COMMA,     // ,
  ARROW,     // ->
  DOT,       // .
};

struct Token {
  TokenType type;
  std::string text;
  uint32_t line;
  uint32_t column;

  Token(TokenType t, std::string_view txt, uint32_t ln, uint32_t col)
      : type(t), text(txt), line(ln), column(col) {}

  bool is(TokenType t) const { return type == t; }
  bool is_one_of(TokenType t1, TokenType t2) const { return is(t1) || is(t2); }
  template <typename... Ts> bool is_one_of(TokenType t1, TokenType t2, Ts... ts) const {
    return is(t1) || is_one_of(t2, ts...);
  }
};

const char* token_type_to_string(TokenType type);

} // namespace tuz
