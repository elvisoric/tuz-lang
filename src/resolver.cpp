#include "tuz/resolver.h"

#include <iostream>

namespace tuz {

void Resolver::resolve() {
  std::cout << "Resolving symbols " << std::endl;

  // Create emtpy struct type entry 
  for (auto& decl : program.declarations) {
    if (decl->kind == DeclKind::Struct) {
      auto* struct_decl = static_cast<StructDecl*>(decl.get());

      auto struct_type = std::make_shared<StructType>(
        struct_decl->name,
        std::vector<std::pair<std::string, TypePtr>>{}
      );

      types[struct_decl->name] = struct_type;
    }
  }

  for (auto& decl : program.declarations) {
      if (decl->kind == DeclKind::Struct) {

          auto* struct_decl = static_cast<StructDecl*>(decl.get());

          auto struct_type = std::static_pointer_cast<StructType>(
              types[struct_decl->name]
          );

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

  if (type->kind == TypeKind::TypeName) {

    auto* ref = static_cast<TypeName*>(type.get());

    if (ref->resolved_type)
      return ref->resolved_type;    

    auto it = types.find(ref->type_name);

    if (it == types.end()) {
      throw std::runtime_error(
          "Failed to resolve type: " + ref->type_name
      );
    }
    
    ref->resolved_type = it->second;
    auto resolved = static_cast<StructType*>(it->second.get());

    if (resolved) {
      std::cout << "Type " 
                << resolved->name
                << " resolved: fields=" 
                << resolved->fields.size()
                << ", size=" 
                << resolved->size()
                << ", alignment=" 
                << resolved->alignment()
                << std::endl;
    }


    return ref->resolved_type;
  }


  return type;
}

FunctionDecl* Resolver::resolve_function(std::string_view fn_name) {
  return nullptr;
}
} // namespace tuz