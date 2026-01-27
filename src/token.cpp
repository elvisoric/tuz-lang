#include "tuz/token.h"

namespace tuz {

const char* token_type_to_string(TokenType type) {
    switch (type) {
        case TokenType::END_OF_FILE: return "EOF";
        case TokenType::INTEGER_LITERAL: return "INTEGER_LITERAL";
        case TokenType::FLOAT_LITERAL: return "FLOAT_LITERAL";
        case TokenType::STRING_LITERAL: return "STRING_LITERAL";
        case TokenType::BOOLEAN_LITERAL: return "BOOLEAN_LITERAL";
        case TokenType::IDENTIFIER: return "IDENTIFIER";
        case TokenType::FN: return "fn";
        case TokenType::LET: return "let";
        case TokenType::MUT: return "mut";
        case TokenType::IF: return "if";
        case TokenType::ELSE: return "else";
        case TokenType::WHILE: return "while";
        case TokenType::FOR: return "for";
        case TokenType::RETURN: return "return";
        case TokenType::STRUCT: return "struct";
        case TokenType::TRUE: return "true";
        case TokenType::FALSE: return "false";
        case TokenType::EXTERN: return "extern";
        case TokenType::INT: return "int";
        case TokenType::FLOAT: return "float";
        case TokenType::BOOL: return "bool";
        case TokenType::VOID: return "void";
        case TokenType::I8: return "i8";
        case TokenType::I16: return "i16";
        case TokenType::I32: return "i32";
        case TokenType::I64: return "i64";
        case TokenType::U8: return "u8";
        case TokenType::U16: return "u16";
        case TokenType::U32: return "u32";
        case TokenType::U64: return "u64";
        case TokenType::F32: return "f32";
        case TokenType::F64: return "f64";
        case TokenType::PLUS: return "+";
        case TokenType::MINUS: return "-";
        case TokenType::STAR: return "*";
        case TokenType::SLASH: return "/";
        case TokenType::PERCENT: return "%";
        case TokenType::ASSIGN: return "=";
        case TokenType::EQ: return "==";
        case TokenType::NEQ: return "!=";
        case TokenType::LT: return "<";
        case TokenType::GT: return ">";
        case TokenType::LEQ: return "<=";
        case TokenType::GEQ: return ">=";
        case TokenType::AND: return "&&";
        case TokenType::OR: return "||";
        case TokenType::NOT: return "!";
        case TokenType::AMPERSAND: return "&";
        case TokenType::LPAREN: return "(";
        case TokenType::RPAREN: return ")";
        case TokenType::LBRACE: return "{";
        case TokenType::RBRACE: return "}";
        case TokenType::LBRACKET: return "[";
        case TokenType::RBRACKET: return "]";
        case TokenType::SEMICOLON: return ";";
        case TokenType::COLON: return ":";
        case TokenType::COMMA: return ",";
        case TokenType::ARROW: return "->";
        case TokenType::DOT: return ".";
        default: return "UNKNOWN";
    }
}

} // namespace tuz
