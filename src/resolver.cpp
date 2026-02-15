#include "tuz/resolver.h"

#include <iostream>

namespace tuz {

void Resolver::resolve() {
  std::cout << "Resolving symbols " << std::endl;
  for (auto& decl : program.declarations) {
    visit_decl(*this, *decl);
  }
}

// Expressions
void Resolver::visit(IntegerLiteralExpr& expr) {
}
void Resolver::visit(FloatLiteralExpr& expr) {
}
void Resolver::visit(BoolLiteralExpr& expr) {
}
void Resolver::visit(StringLiteralExpr& expr) {
}

void Resolver::visit(VariableExpr& expr) {
}

void Resolver::visit(BinaryOpExpr& expr) {
  visit_node(expr.left);
  visit_node(expr.right);
}

void Resolver::visit(UnaryOpExpr& expr) {
  visit_node(expr.operand);
}

void Resolver::visit(CallExpr& expr) {
  visit_node(expr.callee);

  for (auto& a : expr.arguments) {
    visit_node(a);
  }
}

void Resolver::visit(IndexExpr& expr) {
  visit_node(expr.array);
  visit_node(expr.index);
}

void Resolver::visit(FieldAccessExpr& expr) {
  visit_node(expr.object);
}

void Resolver::visit(CastExpr& expr) {
  expr.target_type = resolve_type(expr.target_type);
  visit_node(expr.expr);
}

// Statements
void Resolver::visit(ExprStmt& stmt) {
  visit_node(stmt.expr);
}

void Resolver::visit(LetStmt& stmt) {

  stmt.declared_type = resolve_type(stmt.declared_type);
  visit_node(stmt.initializer);
}

void Resolver::visit(AssignStmt& stmt) {
  //
}

void Resolver::visit(BlockStmt& stmt) {

  for (auto& s : stmt.statements) {
    visit_node(s);
  }
}

void Resolver::visit(IfStmt& stmt) {

  visit_node(stmt.then_branch);
  visit_node(stmt.else_branch);
}

void Resolver::visit(WhileStmt& stmt) {

  visit_node(stmt.body);
}

void Resolver::visit(ForStmt& stmt) {

  visit_node(stmt.body);
}

void Resolver::visit(ReturnStmt& stmt) {
}

// Declarations
void Resolver::visit(FunctionDecl& decl) {

  for (auto& param : decl.params) {
    param.type = resolve_type(param.type);
  }

  decl.return_type = resolve_type(decl.return_type);

  if (decl.body != nullptr) {
    visit_node(decl.body);
  }
}

void Resolver::visit(StructDecl& decl) {
  for (auto& field : decl.fields) {
    field.type = resolve_type(field.type);
  }
}
void Resolver::visit(GlobalDecl& decl) {

  decl.type = resolve_type(decl.type);
}

TypePtr Resolver::resolve_type(TypePtr type) {

  std::cout << "Resolving type for " << type->to_string() << std::endl;

  return type;
}

FunctionDecl* Resolver::resolve_function(std::string_view fn_name) {
  return nullptr;
}
} // namespace tuz