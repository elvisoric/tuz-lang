#include "tuz/resolver.h"

#include <iostream>

namespace tuz {

void Resolver::resolve() {


  auto scope = current_scope();

  // Create emtpy struct type entry 
  for (auto& decl : program.declarations) {
    if (decl->kind == DeclKind::Struct) {
      auto* struct_decl = static_cast<StructDecl*>(decl.get());

      auto struct_type = std::make_shared<StructType>(
        struct_decl->name,
        std::vector<std::pair<std::string, TypePtr>>{}
      );

      auto symbol = std::make_shared<StructSymbol>(
          struct_decl->name,
          struct_type
      );

      scope->declare(symbol);
    }
  }

  // Fill up struct type fields
  for (auto& decl : program.declarations) {
      if (decl->kind == DeclKind::Struct) {
          auto* struct_decl = static_cast<StructDecl*>(decl.get());

          auto symbols = scope->lookup(struct_decl->name);

          if (!symbols)
              throw std::runtime_error("Struct not found in scope: " + struct_decl->name);

          auto& symbol = symbols->front();

          if (symbol->kind != SymbolKind::Struct)
              throw std::runtime_error("Name is not a struct: " + struct_decl->name); 
              
          auto* struct_symbol = static_cast<StructSymbol*>(symbol.get());
          auto struct_type = std::static_pointer_cast<StructType>(struct_symbol->type);
              
          for (auto& field : struct_decl->fields) {
              auto resolved = resolve_type(field.type);
              struct_type->fields.emplace_back(field.name, resolved);
          }
      }
  }  

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

  auto symbols = current_scope()->lookup(expr.name);

  if (!symbols || symbols->empty()) {
      throw std::runtime_error("Unknown identifier: " + expr.name);
  }

  auto& sym = symbols->front();

  if (sym->kind != SymbolKind::Variable) {
      throw std::runtime_error(expr.name + " is not a variable");
  }  

  auto* var_symbol = static_cast<VariableSymbol*>(sym.get());  

  //expr.resolved_symbol = var_symbol;
  expr.type = var_symbol->type;  

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

  visit_node(stmt.initializer);

  // Resolve type (TODO: Type inference)
  stmt.declared_type = resolve_type(stmt.declared_type);

  // Check if we have same symbol 
  if (current_scope()->lookup_local(stmt.name)) {
      throw std::runtime_error("Variable already declared: " + stmt.name);
  }  

  // Define new symbol in the current scope
  auto symbol = std::make_shared<VariableSymbol>(
    stmt.name,
    stmt.declared_type
  );

  current_scope()->declare(symbol);
}

void Resolver::visit(AssignStmt& stmt) {
  //
}

void Resolver::visit(BlockStmt& stmt) {
  ScopeGuard guard(*this);

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

  ScopeGuard guard(*this);


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

  if (type->kind == TypeKind::TypeName) {

    auto* ref = static_cast<TypeName*>(type.get());

    if (ref->resolved_type)
      return ref->resolved_type;    

    auto symbols = current_scope()->lookup_local(ref->type_name);

    if (!symbols || symbols->empty()) {
        throw std::runtime_error("Failed to resolve type: " + ref->type_name);
    }     

    auto& sym = symbols->front();

    if (sym->kind != SymbolKind::Struct) {
      throw std::runtime_error(ref->type_name + " is not a struct type");
    }

    auto* struct_symbol = static_cast<StructSymbol*>(sym.get());

    ref->resolved_type = struct_symbol->type;
    return struct_symbol->type;
  }

  return type;
}

FunctionDecl* Resolver::resolve_function(std::string_view fn_name) {
  return nullptr;
}
} // namespace tuz