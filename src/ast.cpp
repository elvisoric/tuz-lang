#include "tuz/ast.h"

namespace tuz {

// Expression visitor dispatch
void visit_expr(ASTVisitor& visitor, Expr& expr) {
  switch (expr.kind) {
  case ExprKind::IntegerLiteral:
    visitor.visit(static_cast<IntegerLiteralExpr&>(expr));
    break;
  case ExprKind::FloatLiteral:
    visitor.visit(static_cast<FloatLiteralExpr&>(expr));
    break;
  case ExprKind::BoolLiteral:
    visitor.visit(static_cast<BoolLiteralExpr&>(expr));
    break;
  case ExprKind::StringLiteral:
    visitor.visit(static_cast<StringLiteralExpr&>(expr));
    break;
  case ExprKind::Variable:
    visitor.visit(static_cast<VariableExpr&>(expr));
    break;
  case ExprKind::BinaryOp:
    visitor.visit(static_cast<BinaryOpExpr&>(expr));
    break;
  case ExprKind::UnaryOp:
    visitor.visit(static_cast<UnaryOpExpr&>(expr));
    break;
  case ExprKind::Call:
    visitor.visit(static_cast<CallExpr&>(expr));
    break;
  case ExprKind::Index:
    visitor.visit(static_cast<IndexExpr&>(expr));
    break;
  case ExprKind::FieldAccess:
    visitor.visit(static_cast<FieldAccessExpr&>(expr));
    break;
  case ExprKind::Cast:
    visitor.visit(static_cast<CastExpr&>(expr));
    break;
  }
}

// Statement visitor dispatch
void visit_stmt(ASTVisitor& visitor, Stmt& stmt) {

  switch (stmt.kind) {
  case StmtKind::Expr:
    visitor.visit(static_cast<ExprStmt&>(stmt));
    break;
  case StmtKind::Let:
    visitor.visit(static_cast<LetStmt&>(stmt));
    break;
  case StmtKind::Assign:
    visitor.visit(static_cast<AssignStmt&>(stmt));
    break;
  case StmtKind::Block:
    visitor.visit(static_cast<BlockStmt&>(stmt));
    break;
  case StmtKind::If:
    visitor.visit(static_cast<IfStmt&>(stmt));
    break;
  case StmtKind::While:
    visitor.visit(static_cast<WhileStmt&>(stmt));
    break;
  case StmtKind::For:
    visitor.visit(static_cast<ForStmt&>(stmt));
    break;
  case StmtKind::Return:
    visitor.visit(static_cast<ReturnStmt&>(stmt));
    break;
  }
}

// Declaration visitor dispatch
void visit_decl(ASTVisitor& visitor, Decl& decl) {
  switch (decl.kind) {
  case DeclKind::Function:
    visitor.visit(static_cast<FunctionDecl&>(decl));
    break;
  case DeclKind::Struct:
    visitor.visit(static_cast<StructDecl&>(decl));
    break;
  case DeclKind::Global:
    visitor.visit(static_cast<GlobalDecl&>(decl));
    break;
  }
}

} // namespace tuz
