#pragma once

#include "tuz/token.h"

#include <memory>
#include <string>
#include <vector>

namespace tuz {

// Forward declarations
class Type;
struct Symbol;

using TypePtr = std::shared_ptr<Type>;
using SymbolPtr = std::shared_ptr<Symbol>;

// Forward declarations for AST nodes
struct Expr;
struct Stmt;
struct Decl;

using ExprPtr = std::shared_ptr<Expr>;
using StmtPtr = std::shared_ptr<Stmt>;
using DeclPtr = std::shared_ptr<Decl>;

template <typename T> using OptionalRef = std::optional<std::reference_wrapper<const T>>;

// =============================================================================
// Expressions
// =============================================================================

enum class ExprKind {
  IntegerLiteral,
  FloatLiteral,
  BoolLiteral,
  StringLiteral,
  Variable,
  BinaryOp,
  UnaryOp,
  Call,
  Index,
  FieldAccess,
  Cast,
};

struct Expr {
  ExprKind kind;
  TypePtr type; // Filled during type checking
  uint32_t line;
  uint32_t column;

  Expr(ExprKind k, Location loc) : kind(k), line(loc.line), column(loc.column) {}
  virtual ~Expr() = default;
};

struct IntegerLiteralExpr : Expr {
  int64_t value;
  IntegerLiteralExpr(int64_t v, Location loc) : Expr(ExprKind::IntegerLiteral, loc), value(v) {}
};

struct FloatLiteralExpr : Expr {
  double value;
  FloatLiteralExpr(double v, Location loc) : Expr(ExprKind::FloatLiteral, loc), value(v) {}
};

struct BoolLiteralExpr : Expr {
  bool value;
  BoolLiteralExpr(bool v, Location loc) : Expr(ExprKind::BoolLiteral, loc), value(v) {}
};

struct StringLiteralExpr : Expr {
  std::string value;
  StringLiteralExpr(std::string v, Location loc)
      : Expr(ExprKind::StringLiteral, loc), value(std::move(v)) {}
};

struct VariableExpr : Expr {
  std::string name;
  SymbolPtr symbol;
  VariableExpr(std::string n, Location loc) : Expr(ExprKind::Variable, loc), name(std::move(n)) {}
};

enum class BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Eq,
  Neq,
  Lt,
  Gt,
  Leq,
  Geq,
  And,
  Or,
  BitAnd,
  BitOr,
  BitXor,
};

struct BinaryOpExpr : Expr {
  BinaryOp op;
  ExprPtr left;
  ExprPtr right;
  BinaryOpExpr(BinaryOp o, ExprPtr l, ExprPtr r, Location loc)
      : Expr(ExprKind::BinaryOp, loc), op(o), left(std::move(l)), right(std::move(r)) {}
};

enum class UnaryOp {
  Neg,
  Not,
  Deref,
  AddrOf,
};

struct UnaryOpExpr : Expr {
  UnaryOp op;
  ExprPtr operand;
  UnaryOpExpr(UnaryOp o, ExprPtr expr, Location loc)
      : Expr(ExprKind::UnaryOp, loc), op(o), operand(std::move(expr)) {}
};

struct CallExpr : Expr {
  ExprPtr callee;
  std::vector<ExprPtr> arguments;
  CallExpr(ExprPtr c, std::vector<ExprPtr> args, Location loc)
      : Expr(ExprKind::Call, loc), callee(std::move(c)), arguments(std::move(args)) {}
};

struct IndexExpr : Expr {
  ExprPtr array;
  ExprPtr index;
  IndexExpr(ExprPtr arr, ExprPtr idx, Location loc)
      : Expr(ExprKind::Index, loc), array(std::move(arr)), index(std::move(idx)) {}
};

struct FieldAccessExpr : Expr {
  ExprPtr object;
  std::string field;
  FieldAccessExpr(ExprPtr obj, std::string f, Location loc)
      : Expr(ExprKind::FieldAccess, loc), object(std::move(obj)), field(std::move(f)) {}
};

struct CastExpr : Expr {
  TypePtr target_type;
  ExprPtr expr;
  CastExpr(TypePtr t, ExprPtr e, Location loc)
      : Expr(ExprKind::Cast, loc), target_type(std::move(t)), expr(std::move(e)) {}
};

// =============================================================================
// Statements
// =============================================================================

enum class StmtKind {
  Expr,
  Let,
  Assign,
  Block,
  If,
  While,
  For,
  Return,
};

struct Stmt {
  StmtKind kind;
  uint32_t line;
  uint32_t column;

  Stmt(StmtKind k, Location loc) : kind(k), line(loc.line), column(loc.column) {}
  virtual ~Stmt() = default;
};

struct ExprStmt : Stmt {
  ExprPtr expr;
  ExprStmt(ExprPtr e, Location loc) : Stmt(StmtKind::Expr, loc), expr(std::move(e)) {}
};

struct LetStmt : Stmt {
  std::string name;
  TypePtr declared_type;
  ExprPtr initializer;
  bool is_mutable;
  SymbolPtr symbol;
  LetStmt(std::string n, TypePtr t, ExprPtr init, bool mut, Location loc)
      : Stmt(StmtKind::Let, loc), name(std::move(n)), declared_type(std::move(t)),
        initializer(std::move(init)), is_mutable(mut) {}
};

struct AssignStmt : Stmt {
  ExprPtr target;
  ExprPtr value;
  AssignStmt(ExprPtr tgt, ExprPtr val, Location loc)
      : Stmt(StmtKind::Assign, loc), target(std::move(tgt)), value(std::move(val)) {}
};

struct BlockStmt : Stmt {
  std::vector<StmtPtr> statements;
  BlockStmt(std::vector<StmtPtr> stmts, Location loc)
      : Stmt(StmtKind::Block, loc), statements(std::move(stmts)) {}
};

struct IfStmt : Stmt {
  ExprPtr condition;
  StmtPtr then_branch;
  StmtPtr else_branch; // Can be nullptr
  IfStmt(ExprPtr cond, StmtPtr then_br, StmtPtr else_br, Location loc)
      : Stmt(StmtKind::If, loc), condition(std::move(cond)), then_branch(std::move(then_br)),
        else_branch(std::move(else_br)) {}
};

struct WhileStmt : Stmt {
  ExprPtr condition;
  StmtPtr body;
  WhileStmt(ExprPtr cond, StmtPtr b, Location loc)
      : Stmt(StmtKind::While, loc), condition(std::move(cond)), body(std::move(b)) {}
};

struct ForStmt : Stmt {
  std::string var_name;
  ExprPtr range_start;
  ExprPtr range_end;
  StmtPtr body;
  SymbolPtr symbol;
  ForStmt(std::string var, ExprPtr start, ExprPtr end, StmtPtr b, Location loc)
      : Stmt(StmtKind::For, loc), var_name(std::move(var)), range_start(std::move(start)),
        range_end(std::move(end)), body(std::move(b)) {}
};

struct ReturnStmt : Stmt {
  ExprPtr value; // Can be nullptr for void return
  ReturnStmt(ExprPtr val, Location loc) : Stmt(StmtKind::Return, loc), value(std::move(val)) {}
};

// =============================================================================
// Declarations
// =============================================================================

enum class DeclKind {
  Function,
  Struct,
  Global,
};

struct Decl {
  DeclKind kind;
  std::string name;
  uint32_t line;
  uint32_t column;

  Decl(DeclKind k, std::string n, Location loc)
      : kind(k), name(std::move(n)), line(loc.line), column(loc.column) {}
  virtual ~Decl() = default;
};

struct Param {
  std::string name;
  TypePtr type;
  SymbolPtr symbol;
  Param(std::string n, TypePtr t) : name(std::move(n)), type(std::move(t)) {}
};

struct FunctionDecl : Decl {
  std::vector<Param> params;
  TypePtr return_type;
  StmtPtr body; // BlockStmt, can be nullptr for extern
  bool is_extern;

  FunctionDecl(std::string n, std::vector<Param> p, TypePtr ret, StmtPtr b, bool ext, Location loc)
      : Decl(DeclKind::Function, std::move(n), loc), params(std::move(p)),
        return_type(std::move(ret)), body(std::move(b)), is_extern(ext) {}
};

struct Field {
  std::string name;
  TypePtr type;
  Field(std::string n, TypePtr t) : name(std::move(n)), type(std::move(t)) {}
};

struct StructDecl : Decl {
  std::vector<Field> fields;
  StructDecl(std::string n, std::vector<Field> f, Location loc)
      : Decl(DeclKind::Struct, std::move(n), loc), fields(std::move(f)) {}
};

struct GlobalDecl : Decl {
  TypePtr type;
  ExprPtr initializer;
  bool is_mutable;
  GlobalDecl(std::string n, TypePtr t, ExprPtr init, bool mut, Location loc)
      : Decl(DeclKind::Global, std::move(n), loc), type(std::move(t)), initializer(std::move(init)),
        is_mutable(mut) {}
};

// =============================================================================
// Scope
// =============================================================================

enum class SymbolKind { Variable, Function, Struct, Field };

struct Symbol {
  SymbolKind kind;
  std::string name;
  Symbol(SymbolKind k, std::string n) : kind(k), name(std::move(n)) {}
  virtual ~Symbol() = default;
};

struct VariableSymbol : public Symbol {
  TypePtr type;
  VariableSymbol(std::string n, TypePtr t)
      : Symbol(SymbolKind::Variable, std::move(n)), type(std::move(t)) {}
};

struct FunctionSymbol : public Symbol {
  TypePtr type;
  FunctionSymbol(std::string n, TypePtr t)
      : Symbol(SymbolKind::Function, std::move(n)), type(std::move(t)) {}
};

struct StructSymbol : public Symbol {
  TypePtr type;
  StructSymbol(std::string name, TypePtr type)
      : Symbol(SymbolKind::Struct, std::move(name)), type(std::move(type)) {}
};

class Scope {
public:
  Scope* parent;

  std::unordered_map<std::string, std::vector<SymbolPtr>> symbols;

  explicit Scope(Scope* p = nullptr) : parent(p) {}

  void declare(SymbolPtr symbol) { symbols[symbol->name].push_back(std::move(symbol)); }

  OptionalRef<SymbolPtr> lookup_first(const std::string name) const {
    if (auto symbols = lookup(name)) {
      const auto& vec = symbols->get();

      if (!vec.empty())
        return std::cref(vec.front());
    }
    return std::nullopt;
  }

  OptionalRef<SymbolPtr> lookup_first_local(const std::string name) const {
    if (auto symbols = lookup_local(name)) {
      const auto& vec = symbols->get();

      if (!vec.empty())
        return std::cref(vec.front());
    }
    return std::nullopt;
  }

  OptionalRef<std::vector<SymbolPtr>> lookup_local(const std::string& name) const {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
      return std::cref(it->second);
    }
    return std::nullopt;
  }

  OptionalRef<std::vector<SymbolPtr>> lookup(const std::string& name) const {
    for (const Scope* s = this; s != nullptr; s = s->parent) {
      if (auto result = s->lookup_local(name)) {
        return result;
      }
    }
    return std::nullopt;
  }
};

// =============================================================================
// Program
// =============================================================================

struct Program {
  std::vector<DeclPtr> declarations;
  Scope scope;
};

// =============================================================================
// Visitor interface for AST traversal
// =============================================================================

class ASTVisitor {
public:
  virtual ~ASTVisitor() = default;

  // Expressions
  virtual void visit(IntegerLiteralExpr& expr) = 0;
  virtual void visit(FloatLiteralExpr& expr) = 0;
  virtual void visit(BoolLiteralExpr& expr) = 0;
  virtual void visit(StringLiteralExpr& expr) = 0;
  virtual void visit(VariableExpr& expr) = 0;
  virtual void visit(BinaryOpExpr& expr) = 0;
  virtual void visit(UnaryOpExpr& expr) = 0;
  virtual void visit(CallExpr& expr) = 0;
  virtual void visit(IndexExpr& expr) = 0;
  virtual void visit(FieldAccessExpr& expr) = 0;
  virtual void visit(CastExpr& expr) = 0;

  // Statements
  virtual void visit(ExprStmt& stmt) = 0;
  virtual void visit(LetStmt& stmt) = 0;
  virtual void visit(AssignStmt& stmt) = 0;
  virtual void visit(BlockStmt& stmt) = 0;
  virtual void visit(IfStmt& stmt) = 0;
  virtual void visit(WhileStmt& stmt) = 0;
  virtual void visit(ForStmt& stmt) = 0;
  virtual void visit(ReturnStmt& stmt) = 0;

  // Declarations
  virtual void visit(FunctionDecl& decl) = 0;
  virtual void visit(StructDecl& decl) = 0;
  virtual void visit(GlobalDecl& decl) = 0;
};

// Helper functions for visiting
void visit_expr(ASTVisitor& visitor, Expr& expr);
void visit_stmt(ASTVisitor& visitor, Stmt& stmt);
void visit_decl(ASTVisitor& visitor, Decl& decl);

class ASTVisitorDelux : public ASTVisitor {
public:
  void visit_node(ExprPtr& node) {
    if (node)
      visit_expr(*this, *node);
  }

  void visit_node(StmtPtr& node) {
    if (node)
      visit_stmt(*this, *node);
  }

  void visit_node(DeclPtr& node) {
    if (node)
      visit_decl(*this, *node);
  }
};

} // namespace tuz
