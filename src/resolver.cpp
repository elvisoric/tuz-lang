#include "tuz/resolver.h"

#include <iostream>

namespace tuz {

void Resolver::resolve() {

  declare_structs();
  declare_functions();

  for (auto& decl : program.declarations) {
    visit_node(decl);
  }
}

void Resolver::declare_structs() {

  auto scope = current_scope();

  // First pass to initialize symbol names
  for (auto& it : program.declarations) {
    if (it->kind == DeclKind::Struct) {
      auto* decl = static_cast<StructDecl*>(it.get());

      auto type =
          std::make_shared<StructType>(decl->name, std::vector<std::pair<std::string, TypePtr>>{});

      auto symbol = std::make_shared<StructSymbol>(decl->name, type);
      scope->declare(symbol);
    }
  }

  // Second pass to fill up struct fields and resolve type names
  for (auto& it : program.declarations) {
    if (it->kind == DeclKind::Struct) {
      auto* decl = static_cast<StructDecl*>(it.get());
      auto symbol_opt = scope->lookup_first(decl->name);

      if (!symbol_opt) {
        throw std::runtime_error("Internal error: struct not found after declaration: " +
                                 decl->name);
      }

      auto symbol = symbol_opt->get();
      if (symbol->kind == SymbolKind::Struct) {
        auto* sym = static_cast<StructSymbol*>(symbol.get());
        auto type = std::static_pointer_cast<StructType>(sym->type);

        for (auto& field : decl->fields) {
          auto resolved = resolve_type(field.type);
          type->fields.emplace_back(field.name, resolved);
        }
      }
    }
  }
}

void Resolver::declare_functions() {

  auto scope = current_scope();

  for (auto& it : program.declarations) {
    if (it->kind == DeclKind::Function) {
      auto* func = static_cast<FunctionDecl*>(it.get());

      std::vector<TypePtr> args;

      for (auto& p : func->params) {
        args.push_back(p.type);
      }

      auto type = std::make_shared<FunctionType>(args, func->return_type);
      auto symbol = std::make_shared<FunctionSymbol>(func->name, type);

      scope->declare(symbol);
    }
  }
}

// Expressions
void Resolver::visit(IntegerLiteralExpr& expr) {
  expr.type = get_int32_type();
}

void Resolver::visit(FloatLiteralExpr& expr) {
  expr.type = get_float32_type();
}

void Resolver::visit(BoolLiteralExpr& expr) {
  expr.type = get_bool_type();
}

void Resolver::visit(StringLiteralExpr& expr) {
}

void Resolver::visit(VariableExpr& expr) {

  auto symbol = current_scope()->lookup_first(expr.name);

  if (!symbol) {
    throw std::runtime_error("Unknown identifier: " + expr.name);
  }

  auto& sym = symbol->get();

  expr.symbol = sym;

  if (sym->kind == SymbolKind::Variable) {
    auto* symbol = static_cast<VariableSymbol*>(sym.get());
    expr.type = symbol->type;
  } else if (sym->kind == SymbolKind::Function) {
    auto* symbol = static_cast<FunctionSymbol*>(sym.get());
    expr.type = symbol->type;
  }
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

  TypePtr base_type = expr.object->type;

  if (!base_type) {
    throw std::runtime_error("field access on unknown type");
  }

  if (base_type->kind == TypeKind::Pointer) {
    base_type = static_cast<PointerType*>(base_type.get())->pointee;
  }

  if (base_type->kind != TypeKind::Struct) {
    throw std::runtime_error("field access on non-struct type: " + base_type->to_string());
  }

  auto struct_type = std::static_pointer_cast<StructType>(base_type);
  auto field_type = struct_type->get_field_type(expr.field);

  if (!field_type) {
    throw std::runtime_error("unknown field '" + expr.field + "' in struct " + struct_type->name);
  }

  expr.type = field_type;
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
  stmt.declared_type = resolve_type(stmt.declared_type);

  if (current_scope()->lookup_local(stmt.name)) {
    throw std::runtime_error("Variable already declared: " + stmt.name);
  }
  auto symbol = std::make_shared<VariableSymbol>(stmt.name, stmt.declared_type);
  stmt.symbol = symbol;
  current_scope()->declare(symbol);
}

void Resolver::visit(AssignStmt& stmt) {
  visit_node(stmt.target);
  visit_node(stmt.value);
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
  ScopeGuard guard(*this);

  // Check if we have same symbol
  if (current_scope()->lookup_local(stmt.var_name)) {
    throw std::runtime_error("Variable already declared: " + stmt.var_name);
  }

  // Define new symbol in the current scope
  auto symbol = std::make_shared<VariableSymbol>(stmt.var_name, get_int32_type());
  stmt.symbol = symbol;
  current_scope()->declare(symbol);

  visit_node(stmt.body);
}

void Resolver::visit(ReturnStmt& stmt) {
}

// Declarations
void Resolver::visit(FunctionDecl& decl) {

  ScopeGuard guard(*this);

  auto scope = current_scope();

  for (auto& param : decl.params) {
    param.type = resolve_type(param.type);

    auto symbol = std::make_shared<VariableSymbol>(param.name, param.type);
    param.symbol = symbol;
    scope->declare(symbol);
  }

  decl.return_type = resolve_type(decl.return_type);
  visit_node(decl.body);
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
    auto symbol_opt = current_scope()->lookup_first(ref->type_name);

    if (!symbol_opt) {
      throw std::runtime_error("Failed to resolve type: " + ref->type_name);
    }

    auto symbol = symbol_opt.value().get();

    if (symbol->kind == SymbolKind::Struct) {
      auto* sym = static_cast<StructSymbol*>(symbol.get());
      return sym->type;
    }
  }

  return type;
}

FunctionDecl* Resolver::resolve_function(std::string_view fn_name) {
  return nullptr;
}
} // namespace tuz