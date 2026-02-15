#pragma once

#include "ast.h"
#include "diagnostic.h"
#include "type.h"

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace tuz {

class Resolver : public ASTVisitorDelux {
private:  
  
  std::unordered_map<std::string, TypePtr> types;

public:
  Program& program;
  

  explicit Resolver(Program& program) : program(program) {}

  void resolve();

  // Expressions
  void visit(IntegerLiteralExpr& expr) override;
  void visit(FloatLiteralExpr& expr) override;
  void visit(BoolLiteralExpr& expr) override;
  void visit(StringLiteralExpr& expr) override;
  void visit(VariableExpr& expr) override;
  void visit(BinaryOpExpr& expr) override;
  void visit(UnaryOpExpr& expr) override;
  void visit(CallExpr& expr) override;
  void visit(IndexExpr& expr) override;
  void visit(FieldAccessExpr& expr) override;
  void visit(CastExpr& expr) override;

  // Statements
  void visit(ExprStmt& stmt) override;
  void visit(LetStmt& stmt) override;
  void visit(AssignStmt& stmt) override;
  void visit(BlockStmt& stmt) override;
  void visit(IfStmt& stmt) override;
  void visit(WhileStmt& stmt) override;
  void visit(ForStmt& stmt) override;
  void visit(ReturnStmt& stmt) override;

  // Declarations
  void visit(FunctionDecl& decl) override;
  void visit(StructDecl& decl) override;
  void visit(GlobalDecl& decl) override;

private:
  TypePtr resolve_type(TypePtr type);
  FunctionDecl* resolve_function(std::string_view fn_name);
};

} // namespace tuz
