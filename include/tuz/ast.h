#pragma once

#include <memory>
#include <string>
#include <vector>

namespace tuz {

// Forward declarations
class Type;
using TypePtr = std::shared_ptr<Type>;

// Forward declarations for AST nodes
struct Expr;
struct Stmt;
struct Decl;

using ExprPtr = std::shared_ptr<Expr>;
using StmtPtr = std::shared_ptr<Stmt>;
using DeclPtr = std::shared_ptr<Decl>;

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

  Expr(ExprKind k, uint32_t ln, uint32_t col) : kind(k), line(ln), column(col) {}
  virtual ~Expr() = default;
};

struct IntegerLiteralExpr : Expr {
  int64_t value;
  IntegerLiteralExpr(int64_t v, uint32_t ln, uint32_t col)
      : Expr(ExprKind::IntegerLiteral, ln, col), value(v) {}
};

struct FloatLiteralExpr : Expr {
  double value;
  FloatLiteralExpr(double v, uint32_t ln, uint32_t col)
      : Expr(ExprKind::FloatLiteral, ln, col), value(v) {}
};

struct BoolLiteralExpr : Expr {
  bool value;
  BoolLiteralExpr(bool v, uint32_t ln, uint32_t col)
      : Expr(ExprKind::BoolLiteral, ln, col), value(v) {}
};

struct StringLiteralExpr : Expr {
  std::string value;
  StringLiteralExpr(std::string v, uint32_t ln, uint32_t col)
      : Expr(ExprKind::StringLiteral, ln, col), value(std::move(v)) {}
};

struct VariableExpr : Expr {
  std::string name;
  VariableExpr(std::string n, uint32_t ln, uint32_t col)
      : Expr(ExprKind::Variable, ln, col), name(std::move(n)) {}
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
  BinaryOpExpr(BinaryOp o, ExprPtr l, ExprPtr r, uint32_t ln, uint32_t col)
      : Expr(ExprKind::BinaryOp, ln, col), op(o), left(std::move(l)), right(std::move(r)) {}
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
  UnaryOpExpr(UnaryOp o, ExprPtr expr, uint32_t ln, uint32_t col)
      : Expr(ExprKind::UnaryOp, ln, col), op(o), operand(std::move(expr)) {}
};

struct CallExpr : Expr {
  ExprPtr callee;
  std::vector<ExprPtr> arguments;
  CallExpr(ExprPtr c, std::vector<ExprPtr> args, uint32_t ln, uint32_t col)
      : Expr(ExprKind::Call, ln, col), callee(std::move(c)), arguments(std::move(args)) {}
};

struct IndexExpr : Expr {
  ExprPtr array;
  ExprPtr index;
  IndexExpr(ExprPtr arr, ExprPtr idx, uint32_t ln, uint32_t col)
      : Expr(ExprKind::Index, ln, col), array(std::move(arr)), index(std::move(idx)) {}
};

struct FieldAccessExpr : Expr {
  ExprPtr object;
  std::string field;
  FieldAccessExpr(ExprPtr obj, std::string f, uint32_t ln, uint32_t col)
      : Expr(ExprKind::FieldAccess, ln, col), object(std::move(obj)), field(std::move(f)) {}
};

struct CastExpr : Expr {
  TypePtr target_type;
  ExprPtr expr;
  CastExpr(TypePtr t, ExprPtr e, uint32_t ln, uint32_t col)
      : Expr(ExprKind::Cast, ln, col), target_type(std::move(t)), expr(std::move(e)) {}
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

  Stmt(StmtKind k, uint32_t ln, uint32_t col) : kind(k), line(ln), column(col) {}
  virtual ~Stmt() = default;
};

struct ExprStmt : Stmt {
  ExprPtr expr;
  ExprStmt(ExprPtr e, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::Expr, ln, col), expr(std::move(e)) {}
};

struct LetStmt : Stmt {
  std::string name;
  TypePtr declared_type;
  ExprPtr initializer;
  bool is_mutable;
  LetStmt(std::string n, TypePtr t, ExprPtr init, bool mut, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::Let, ln, col), name(std::move(n)), declared_type(std::move(t)),
        initializer(std::move(init)), is_mutable(mut) {}
};

struct AssignStmt : Stmt {
  ExprPtr target;
  ExprPtr value;
  AssignStmt(ExprPtr tgt, ExprPtr val, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::Assign, ln, col), target(std::move(tgt)), value(std::move(val)) {}
};

struct BlockStmt : Stmt {
  std::vector<StmtPtr> statements;
  BlockStmt(std::vector<StmtPtr> stmts, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::Block, ln, col), statements(std::move(stmts)) {}
};

struct IfStmt : Stmt {
  ExprPtr condition;
  StmtPtr then_branch;
  StmtPtr else_branch; // Can be nullptr
  IfStmt(ExprPtr cond, StmtPtr then_br, StmtPtr else_br, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::If, ln, col), condition(std::move(cond)), then_branch(std::move(then_br)),
        else_branch(std::move(else_br)) {}
};

struct WhileStmt : Stmt {
  ExprPtr condition;
  StmtPtr body;
  WhileStmt(ExprPtr cond, StmtPtr b, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::While, ln, col), condition(std::move(cond)), body(std::move(b)) {}
};

struct ForStmt : Stmt {
  std::string var_name;
  ExprPtr range_start;
  ExprPtr range_end;
  StmtPtr body;
  ForStmt(std::string var, ExprPtr start, ExprPtr end, StmtPtr b, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::For, ln, col), var_name(std::move(var)), range_start(std::move(start)),
        range_end(std::move(end)), body(std::move(b)) {}
};

struct ReturnStmt : Stmt {
  ExprPtr value; // Can be nullptr for void return
  ReturnStmt(ExprPtr val, uint32_t ln, uint32_t col)
      : Stmt(StmtKind::Return, ln, col), value(std::move(val)) {}
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

  Decl(DeclKind k, std::string n, uint32_t ln, uint32_t col)
      : kind(k), name(std::move(n)), line(ln), column(col) {}
  virtual ~Decl() = default;
};

struct Param {
  std::string name;
  TypePtr type;
  Param(std::string n, TypePtr t) : name(std::move(n)), type(std::move(t)) {}
};

struct FunctionDecl : Decl {
  std::vector<Param> params;
  TypePtr return_type;
  StmtPtr body; // BlockStmt, can be nullptr for extern
  bool is_extern;

  FunctionDecl(std::string n, std::vector<Param> p, TypePtr ret, StmtPtr b, bool ext, uint32_t ln,
               uint32_t col)
      : Decl(DeclKind::Function, std::move(n), ln, col), params(std::move(p)),
        return_type(std::move(ret)), body(std::move(b)), is_extern(ext) {}
};

struct Field {
  std::string name;
  TypePtr type;
  Field(std::string n, TypePtr t) : name(std::move(n)), type(std::move(t)) {}
};

struct StructDecl : Decl {
  std::vector<Field> fields;
  StructDecl(std::string n, std::vector<Field> f, uint32_t ln, uint32_t col)
      : Decl(DeclKind::Struct, std::move(n), ln, col), fields(std::move(f)) {}
};

struct GlobalDecl : Decl {
  TypePtr type;
  ExprPtr initializer;
  bool is_mutable;
  GlobalDecl(std::string n, TypePtr t, ExprPtr init, bool mut, uint32_t ln, uint32_t col)
      : Decl(DeclKind::Global, std::move(n), ln, col), type(std::move(t)),
        initializer(std::move(init)), is_mutable(mut) {}
};

// =============================================================================
// Program
// =============================================================================

struct Program {
  std::vector<DeclPtr> declarations;
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

} // namespace tuz
