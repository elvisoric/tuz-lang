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
  std::vector<std::unique_ptr<Scope>> scope_stack;

public:
  Program& program;

  explicit Resolver(Program& program) : program(program) { push_scope(); }

  void push_scope() {
    Scope* parent = current_scope();
    scope_stack.push_back(std::make_unique<Scope>(parent));
  }

  void pop_scope() {
    if (scope_stack.size() <= 1) {
      throw std::runtime_error("Cannot pop global scope");
    }
    scope_stack.pop_back();
  }

  Scope* current_scope() {
    if (scope_stack.empty())
      return &program.scope;

    return scope_stack.back().get();
  }

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

  void declare_structs();
  void declare_functions();
};

class ScopeGuard {
  Resolver& resolver;

public:
  ScopeGuard(Resolver& r) : resolver(r) { resolver.push_scope(); }

  ~ScopeGuard() { resolver.pop_scope(); }
};

} // namespace tuz
