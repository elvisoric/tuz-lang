#include "tuz/codegen.h"

#include "tuz/diagnostic.h"

#include <iostream>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

namespace tuz {

CodeGenerator::CodeGenerator()
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("tuz_module", *context_)),
      builder_(std::make_unique<llvm::IRBuilder<>>(*context_)), current_function_(nullptr) {

  // Initialize LLVM
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();
}

void CodeGenerator::generate(Program& program) {
  // First pass: declare all structs
  for (auto& decl : program.declarations) {
    if (decl->kind == DeclKind::Struct) {
      codegen_decl(*decl);
    }
  }

  // Second pass: declare all functions (without bodies - just prototypes)
  for (auto& decl : program.declarations) {
    if (decl->kind == DeclKind::Function) {
      auto& fn = static_cast<FunctionDecl&>(*decl);
      // Only declare the function prototype, don't generate body yet
      declare_function(fn);
    }
  }

  // Third pass: generate function bodies and globals
  for (auto& decl : program.declarations) {
    if (decl->kind == DeclKind::Function) {
      auto& fn = static_cast<FunctionDecl&>(*decl);
      if (fn.body) {
        generate_function_body(fn);
      }
    } else if (decl->kind == DeclKind::Global) {
      codegen_decl(*decl);
    }
  }
}

std::unique_ptr<llvm::Module> CodeGenerator::get_module() {
  return std::move(module_);
}

// =============================================================================
// Type conversion
// =============================================================================

llvm::Type* CodeGenerator::convert_type(TypePtr type) {
  switch (type->kind) {
  case TypeKind::Void:
    return llvm::Type::getVoidTy(*context_);
  case TypeKind::Int8:
    return llvm::Type::getInt8Ty(*context_);
  case TypeKind::Int16:
    return llvm::Type::getInt16Ty(*context_);
  case TypeKind::Int32:
    return llvm::Type::getInt32Ty(*context_);
  case TypeKind::Int64:
    return llvm::Type::getInt64Ty(*context_);
  case TypeKind::UInt8:
    return llvm::Type::getInt8Ty(*context_);
  case TypeKind::UInt16:
    return llvm::Type::getInt16Ty(*context_);
  case TypeKind::UInt32:
    return llvm::Type::getInt32Ty(*context_);
  case TypeKind::UInt64:
    return llvm::Type::getInt64Ty(*context_);
  case TypeKind::Float32:
    return llvm::Type::getFloatTy(*context_);
  case TypeKind::Float64:
    return llvm::Type::getDoubleTy(*context_);
  case TypeKind::Bool:
    return llvm::Type::getInt1Ty(*context_);
  case TypeKind::Pointer: {
    // Opaque pointers - just return pointer type without pointee
    return llvm::PointerType::get(*context_, 0);
  }
  case TypeKind::Array: {
    auto arr_type = static_cast<ArrayType*>(type.get());
    llvm::Type* elem = convert_type(arr_type->element_type);
    return llvm::ArrayType::get(elem, arr_type->size_val);
  }
  default:
    return llvm::Type::getInt32Ty(*context_);
  }
}

// =============================================================================
// Value stack helpers
// =============================================================================

llvm::Value* CodeGenerator::pop_value() {
  if (value_stack_.empty()) {
    throw CodeGenError("Value stack underflow");
  }
  llvm::Value* val = value_stack_.back();
  value_stack_.pop_back();
  return val;
}

void CodeGenerator::push_value(llvm::Value* val) {
  value_stack_.push_back(val);
}

// =============================================================================
// Variable management
// =============================================================================

void CodeGenerator::enter_scope() {
  named_values_.emplace_back();
}

void CodeGenerator::exit_scope() {
  if (!named_values_.empty()) {
    named_values_.pop_back();
  }
}

void CodeGenerator::set_variable(const std::string& name, llvm::Value* alloca, bool is_mutable) {
  if (named_values_.empty()) {
    named_values_.emplace_back();
  }
  named_values_.back()[name] = VariableInfo{alloca, is_mutable};
}

llvm::Value* CodeGenerator::get_variable(const std::string& name) {
  // Search from innermost scope outward
  for (auto it = named_values_.rbegin(); it != named_values_.rend(); ++it) {
    auto found = it->find(name);
    if (found != it->end()) {
      return found->second.value;
    }
  }
  // Check for global
  llvm::GlobalVariable* global = module_->getNamedGlobal(name);
  if (global) {
    return global;
  }
  return nullptr;
}

bool CodeGenerator::is_variable_mutable(const std::string& name) {
  // Search from innermost scope outward for local variables
  for (auto it = named_values_.rbegin(); it != named_values_.rend(); ++it) {
    auto found = it->find(name);
    if (found != it->end()) {
      return found->second.is_mutable;
    }
  }
  // Globals are always considered mutable for now
  llvm::GlobalVariable* global = module_->getNamedGlobal(name);
  if (global) {
    return true;
  }
  return false;
}

llvm::AllocaInst* CodeGenerator::create_alloca(llvm::Type* type, const std::string& name) {
  llvm::IRBuilder<> tmp_builder(&current_function_->getEntryBlock(),
                                current_function_->getEntryBlock().begin());
  return tmp_builder.CreateAlloca(type, nullptr, name);
}

// =============================================================================
// Expression visitors
// =============================================================================

void CodeGenerator::visit(IntegerLiteralExpr& expr) {
  llvm::Type* type = convert_type(expr.type ? expr.type : get_int32_type());
  llvm::Value* val = llvm::ConstantInt::get(type, expr.value, true);
  push_value(val);
}

void CodeGenerator::visit(FloatLiteralExpr& expr) {
  llvm::Type* type = convert_type(expr.type ? expr.type : get_float64_type());
  llvm::Value* val = llvm::ConstantFP::get(type, expr.value);
  push_value(val);
}

void CodeGenerator::visit(BoolLiteralExpr& expr) {
  llvm::Value* val = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), expr.value ? 1 : 0);
  push_value(val);
}

void CodeGenerator::visit(StringLiteralExpr& expr) {
  llvm::Value* str = builder_->CreateGlobalString(expr.value);
  push_value(str);
}

void CodeGenerator::visit(VariableExpr& expr) {
  llvm::Value* var = get_variable(expr.name);
  if (!var) {
    throw CodeGenError("unknown variable: '" + expr.name + "'",
                       SourceLocation(expr.line, expr.column, expr.name.length()));
  }

  // Load the value (unless it's a pointer we're taking the address of)
  // For now, always load
  llvm::Value* loaded =
      builder_->CreateLoad(convert_type(expr.type ? expr.type : get_int32_type()), var, expr.name);
  push_value(loaded);
}

void CodeGenerator::visit(BinaryOpExpr& expr) {
  // Generate left and right
  llvm::Value* left = codegen_expr(*expr.left);
  llvm::Value* right = codegen_expr(*expr.right);

  llvm::Value* result = codegen_binary_op(expr.op, left, right, expr.type);
  push_value(result);
}

void CodeGenerator::visit(UnaryOpExpr& expr) {
  llvm::Value* operand = codegen_expr(*expr.operand);

  llvm::Value* result = nullptr;

  switch (expr.op) {
  case UnaryOp::Neg:
    if (operand->getType()->isFloatingPointTy()) {
      result = builder_->CreateFNeg(operand, "negtmp");
    } else {
      result = builder_->CreateNeg(operand, "negtmp");
    }
    break;
  case UnaryOp::Not:
    result = builder_->CreateNot(operand, "nottmp");
    break;
  case UnaryOp::Deref:
    result = builder_->CreateLoad(convert_type(expr.type), operand, "deref");
    break;
  case UnaryOp::AddrOf: {
    // This is tricky - we need the alloca, not the loaded value
    // For now, this won't work correctly with our current approach
    // We need to handle this specially in the parser/codegen
    auto& var_expr = static_cast<VariableExpr&>(*expr.operand);
    llvm::Value* var = get_variable(var_expr.name);
    result = var; // Return the pointer directly
    break;
  }
  }

  push_value(result);
}

void CodeGenerator::visit(CallExpr& expr) {
  // Get function
  auto& var_expr = static_cast<VariableExpr&>(*expr.callee);
  llvm::Function* callee = module_->getFunction(var_expr.name);
  if (!callee) {
    throw CodeGenError("unknown function: '" + var_expr.name + "'",
                       SourceLocation(expr.line, expr.column, var_expr.name.length()));
  }

  // Generate arguments
  std::vector<llvm::Value*> args;
  for (auto& arg : expr.arguments) {
    args.push_back(codegen_expr(*arg));
  }

  llvm::Value* result = builder_->CreateCall(callee, args, "calltmp");
  push_value(result);
}

void CodeGenerator::visit(IndexExpr& expr) {
  llvm::Value* array = codegen_expr(*expr.array);
  llvm::Value* index = codegen_expr(*expr.index);

  // For arrays: GEP to get element pointer, then load
  std::vector<llvm::Value*> indices = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0),
                                       index};
  llvm::Value* elem_ptr = get_element_ptr(array, indices, convert_type(expr.type));
  llvm::Value* loaded = builder_->CreateLoad(convert_type(expr.type), elem_ptr, "elem");
  push_value(loaded);
}

void CodeGenerator::visit(FieldAccessExpr& expr) {
  llvm::Value* obj = codegen_expr(*expr.object);

  // This would need struct type info to work properly
  // For now, just push a placeholder
  push_value(obj);
}

void CodeGenerator::visit(CastExpr& expr) {
  llvm::Value* val = codegen_expr(*expr.expr);

  llvm::Type* target = convert_type(expr.target_type);
  llvm::Type* source = val->getType();

  llvm::Value* result = nullptr;

  if (source->isIntegerTy() && target->isFloatingPointTy()) {
    result = builder_->CreateSIToFP(val, target, "sitofp");
  } else if (source->isFloatingPointTy() && target->isIntegerTy()) {
    result = builder_->CreateFPToSI(val, target, "fptosi");
  } else if (source->isIntegerTy() && target->isIntegerTy()) {
    result = builder_->CreateIntCast(val, target, true, "intcast");
  } else if (source->isFloatingPointTy() && target->isFloatingPointTy()) {
    result = builder_->CreateFPCast(val, target, "fpcast");
  } else {
    result = val; // No conversion needed or unknown
  }

  push_value(result);
}

// =============================================================================
// Statement visitors
// =============================================================================

void CodeGenerator::visit(ExprStmt& stmt) {
  codegen_expr(*stmt.expr); // Discard result
}

void CodeGenerator::visit(LetStmt& stmt) {
  // Determine type
  TypePtr var_type = stmt.declared_type;
  if (!var_type && stmt.initializer) {
    var_type = stmt.initializer->type;
  }
  if (!var_type) {
    var_type = get_int32_type();
  }

  llvm::Type* llvm_type = convert_type(var_type);
  llvm::AllocaInst* alloca = create_alloca(llvm_type, stmt.name);

  if (stmt.initializer) {
    llvm::Value* init_val = codegen_expr(*stmt.initializer);
    builder_->CreateStore(init_val, alloca);
  }

  set_variable(stmt.name, alloca, stmt.is_mutable);
}

void CodeGenerator::visit(AssignStmt& stmt) {
  llvm::Value* val = codegen_expr(*stmt.value);

  // Get the target (should be a variable or field access)
  if (stmt.target->kind == ExprKind::Variable) {
    auto& var = static_cast<VariableExpr&>(*stmt.target);
    llvm::Value* ptr = get_variable(var.name);
    if (!ptr) {
      throw CodeGenError("unknown variable: '" + var.name + "'",
                         SourceLocation(var.line, var.column, var.name.length()));
    }
    // Check if variable is mutable
    if (!is_variable_mutable(var.name)) {
      throw CodeGenError("cannot assign to immutable variable '" + var.name + "'",
                         SourceLocation(var.line, var.column, var.name.length()));
    }
    builder_->CreateStore(val, ptr);
  } else if (stmt.target->kind == ExprKind::Index) {
    // Array assignment
    auto& idx = static_cast<IndexExpr&>(*stmt.target);
    llvm::Value* array = codegen_expr(*idx.array);
    llvm::Value* index = codegen_expr(*idx.index);

    std::vector<llvm::Value*> indices = {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0), index};
    llvm::Value* elem_ptr = get_element_ptr(array, indices, convert_type(stmt.value->type));
    builder_->CreateStore(val, elem_ptr);
  }
}

void CodeGenerator::visit(BlockStmt& stmt) {
  enter_scope();
  for (auto& s : stmt.statements) {
    codegen_stmt(*s);
  }
  exit_scope();
}

void CodeGenerator::visit(IfStmt& stmt) {
  llvm::Function* function = builder_->GetInsertBlock()->getParent();

  llvm::BasicBlock* then_bb = llvm::BasicBlock::Create(*context_, "then", function);
  llvm::BasicBlock* else_bb = llvm::BasicBlock::Create(*context_, "else", function);
  llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(*context_, "ifcont", function);

  llvm::Value* cond = codegen_expr(*stmt.condition);

  builder_->CreateCondBr(cond, then_bb, else_bb);

  // Then block
  builder_->SetInsertPoint(then_bb);
  codegen_stmt(*stmt.then_branch);

  // Only branch to merge if no terminator
  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(merge_bb);
  }

  // Else block
  builder_->SetInsertPoint(else_bb);

  if (stmt.else_branch) {
    codegen_stmt(*stmt.else_branch);
  }

  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(merge_bb);
  }

  // Merge block
  builder_->SetInsertPoint(merge_bb);
}

void CodeGenerator::visit(WhileStmt& stmt) {
  llvm::Function* function = builder_->GetInsertBlock()->getParent();

  llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*context_, "whilecond", function);
  llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "whilebody", function);
  llvm::BasicBlock* end_bb = llvm::BasicBlock::Create(*context_, "whileend", function);

  // Push loop targets for break/continue
  loop_stack_.push_back({cond_bb, end_bb});

  builder_->CreateBr(cond_bb);

  // Condition
  builder_->SetInsertPoint(cond_bb);
  llvm::Value* cond = codegen_expr(*stmt.condition);
  builder_->CreateCondBr(cond, body_bb, end_bb);

  // Body
  builder_->SetInsertPoint(body_bb);
  codegen_stmt(*stmt.body);

  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(cond_bb);
  }

  // End
  builder_->SetInsertPoint(end_bb);

  loop_stack_.pop_back();
}

void CodeGenerator::visit(ForStmt& stmt) {
  llvm::Function* function = builder_->GetInsertBlock()->getParent();

  llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*context_, "forcond", function);
  llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "forbody", function);
  llvm::BasicBlock* step_bb = llvm::BasicBlock::Create(*context_, "forstep", function);
  llvm::BasicBlock* end_bb = llvm::BasicBlock::Create(*context_, "forend", function);

  loop_stack_.push_back({step_bb, end_bb});

  enter_scope();

  // Initialize loop variable
  llvm::AllocaInst* loop_var = create_alloca(llvm::Type::getInt32Ty(*context_), stmt.var_name);
  llvm::Value* start_val = codegen_expr(*stmt.range_start);
  builder_->CreateStore(start_val, loop_var);
  set_variable(stmt.var_name, loop_var);

  llvm::Value* end_val = codegen_expr(*stmt.range_end);

  builder_->CreateBr(cond_bb);

  // Condition
  builder_->SetInsertPoint(cond_bb);
  llvm::Value* current =
      builder_->CreateLoad(llvm::Type::getInt32Ty(*context_), loop_var, stmt.var_name);
  llvm::Value* cond = builder_->CreateICmpSLT(current, end_val, "forcond");
  builder_->CreateCondBr(cond, body_bb, end_bb);

  // Body
  builder_->SetInsertPoint(body_bb);
  codegen_stmt(*stmt.body);
  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(step_bb);
  }

  // Step
  builder_->SetInsertPoint(step_bb);
  llvm::Value* step_val =
      builder_->CreateLoad(llvm::Type::getInt32Ty(*context_), loop_var, stmt.var_name);
  llvm::Value* next_val = builder_->CreateAdd(
      step_val, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1), "next");
  builder_->CreateStore(next_val, loop_var);
  builder_->CreateBr(cond_bb);

  // End
  builder_->SetInsertPoint(end_bb);

  exit_scope();
  loop_stack_.pop_back();
}

void CodeGenerator::visit(ReturnStmt& stmt) {
  if (stmt.value) {
    llvm::Value* val = codegen_expr(*stmt.value);
    builder_->CreateRet(val);
  } else {
    builder_->CreateRetVoid();
  }
}

// =============================================================================
// Declaration visitors
// =============================================================================

void CodeGenerator::declare_function(FunctionDecl& decl) {
  // Create function type
  std::vector<llvm::Type*> param_types;
  for (auto& param : decl.params) {
    param_types.push_back(convert_type(param.type));
  }

  llvm::Type* ret_type = convert_type(decl.return_type);
  llvm::FunctionType* func_type = llvm::FunctionType::get(ret_type, param_types, false);

  // Create function
  llvm::Function* function =
      llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, decl.name, module_.get());

  // Set parameter names
  size_t idx = 0;
  for (auto& arg : function->args()) {
    arg.setName(decl.params[idx++].name);
  }

  functions_[decl.name] = function;
}

void CodeGenerator::generate_function_body(FunctionDecl& decl) {
  llvm::Function* function = functions_[decl.name];
  if (!function) {
    throw CodeGenError("Function not declared: " + decl.name);
  }

  if (!decl.body || decl.body->kind != StmtKind::Block) {
    return; // No body to generate (extern function)
  }

  current_function_ = function;

  llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", function);
  builder_->SetInsertPoint(entry);

  enter_scope();

  // Create allocas for parameters
  for (auto& arg : function->args()) {
    llvm::AllocaInst* alloca = create_alloca(arg.getType(), std::string(arg.getName()));
    builder_->CreateStore(&arg, alloca);
    set_variable(std::string(arg.getName()), alloca);
  }

  // Generate body
  auto& block = static_cast<BlockStmt&>(*decl.body);
  for (auto& stmt : block.statements) {
    codegen_stmt(*stmt);
  }

  // Add default return if needed
  if (!builder_->GetInsertBlock()->getTerminator()) {
    if (decl.return_type->is_void()) {
      builder_->CreateRetVoid();
    } else {
      builder_->CreateRet(llvm::ConstantInt::get(convert_type(decl.return_type), 0));
    }
  }

  exit_scope();

  // Verify function
  llvm::verifyFunction(*function);

  current_function_ = nullptr;
}

void CodeGenerator::visit(FunctionDecl& decl) {
  // For external use - declare and generate body in one call
  declare_function(decl);
  if (decl.body) {
    generate_function_body(decl);
  }
}

void CodeGenerator::visit(StructDecl& decl) {
  std::vector<llvm::Type*> field_types;
  for (auto& field : decl.fields) {
    field_types.push_back(convert_type(field.type));
  }

  auto* struct_type = llvm::StructType::create(*context_, field_types, decl.name);
  struct_types_[decl.name] = struct_type;
}

void CodeGenerator::visit(GlobalDecl& decl) {
  TypePtr type = decl.type;
  if (!type && decl.initializer) {
    type = decl.initializer->type;
  }
  if (!type) {
    type = get_int32_type();
  }

  llvm::Type* llvm_type = convert_type(type);
  llvm::Constant* init = nullptr;

  if (decl.initializer) {
    // For simple literals, we can evaluate them
    if (decl.initializer->kind == ExprKind::IntegerLiteral) {
      auto& lit = static_cast<IntegerLiteralExpr&>(*decl.initializer);
      init = llvm::ConstantInt::get(llvm_type, lit.value);
    } else if (decl.initializer->kind == ExprKind::FloatLiteral) {
      auto& lit = static_cast<FloatLiteralExpr&>(*decl.initializer);
      init = llvm::ConstantFP::get(llvm_type, lit.value);
    } else if (decl.initializer->kind == ExprKind::BoolLiteral) {
      auto& lit = static_cast<BoolLiteralExpr&>(*decl.initializer);
      init = llvm::ConstantInt::get(llvm_type, lit.value ? 1 : 0);
    }
  }

  if (!init) {
    init = llvm::Constant::getNullValue(llvm_type);
  }

  new llvm::GlobalVariable(*module_, llvm_type, !decl.is_mutable,
                           llvm::GlobalValue::ExternalLinkage, init, decl.name);
}

// =============================================================================
// Codegen helpers
// =============================================================================

llvm::Value* CodeGenerator::codegen_expr(Expr& expr) {
  visit_expr(*this, expr);
  return pop_value();
}

void CodeGenerator::codegen_stmt(Stmt& stmt) {
  visit_stmt(*this, stmt);
}

void CodeGenerator::codegen_decl(Decl& decl) {
  visit_decl(*this, decl);
}

llvm::Value* CodeGenerator::codegen_binary_op(BinaryOp op, llvm::Value* left, llvm::Value* right,
                                              TypePtr result_type) {
  llvm::Type* type = left->getType();
  bool is_fp = type->isFloatingPointTy();

  switch (op) {
  case BinaryOp::Add:
    return is_fp ? builder_->CreateFAdd(left, right, "addtmp")
                 : builder_->CreateAdd(left, right, "addtmp");
  case BinaryOp::Sub:
    return is_fp ? builder_->CreateFSub(left, right, "subtmp")
                 : builder_->CreateSub(left, right, "subtmp");
  case BinaryOp::Mul:
    return is_fp ? builder_->CreateFMul(left, right, "multmp")
                 : builder_->CreateMul(left, right, "multmp");
  case BinaryOp::Div:
    return is_fp ? builder_->CreateFDiv(left, right, "divtmp")
                 : builder_->CreateSDiv(left, right, "divtmp");
  case BinaryOp::Mod:
    return is_fp ? builder_->CreateFRem(left, right, "modtmp")
                 : builder_->CreateSRem(left, right, "modtmp");
  case BinaryOp::Eq:
    return is_fp ? builder_->CreateFCmpOEQ(left, right, "eqtmp")
                 : builder_->CreateICmpEQ(left, right, "eqtmp");
  case BinaryOp::Neq:
    return is_fp ? builder_->CreateFCmpONE(left, right, "neqtmp")
                 : builder_->CreateICmpNE(left, right, "neqtmp");
  case BinaryOp::Lt:
    return is_fp ? builder_->CreateFCmpOLT(left, right, "lttmp")
                 : builder_->CreateICmpSLT(left, right, "lttmp");
  case BinaryOp::Gt:
    return is_fp ? builder_->CreateFCmpOGT(left, right, "gttmp")
                 : builder_->CreateICmpSGT(left, right, "gttmp");
  case BinaryOp::Leq:
    return is_fp ? builder_->CreateFCmpOLE(left, right, "leqtmp")
                 : builder_->CreateICmpSLE(left, right, "leqtmp");
  case BinaryOp::Geq:
    return is_fp ? builder_->CreateFCmpOGE(left, right, "geqtmp")
                 : builder_->CreateICmpSGE(left, right, "geqtmp");
  case BinaryOp::And:
    return builder_->CreateAnd(left, right, "andtmp");
  case BinaryOp::Or:
    return builder_->CreateOr(left, right, "ortmp");
  default:
    return left;
  }
}

llvm::Value* CodeGenerator::get_element_ptr(llvm::Value* ptr,
                                            const std::vector<llvm::Value*>& indices,
                                            llvm::Type* elem_type) {
  return builder_->CreateGEP(elem_type, ptr, indices, "gep");
}

// =============================================================================
// Output
// =============================================================================

void CodeGenerator::dump_ir(const std::string& filename) {
  std::error_code ec;
  llvm::raw_fd_ostream os(filename, ec, llvm::sys::fs::OF_None);
  if (ec) {
    std::cerr << "Error opening file: " << ec.message() << std::endl;
    return;
  }
  module_->print(os, nullptr);
}

void CodeGenerator::compile_to_object(const std::string& filename) {
  std::string target_triple_str = llvm::sys::getDefaultTargetTriple();
  llvm::Triple target_triple(target_triple_str);
  module_->setTargetTriple(target_triple.str());

  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple_str, error);
  if (!target) {
    std::cerr << "Error: " << error << std::endl;
    return;
  }

  auto cpu = "generic";
  auto features = "";

  llvm::TargetOptions opt;
  auto rm = llvm::Reloc::Model::PIC_;
  auto target_machine = target->createTargetMachine(target_triple_str, cpu, features, opt, rm);

  module_->setDataLayout(target_machine->createDataLayout());

  std::error_code ec;
  llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);
  if (ec) {
    std::cerr << "Could not open file: " << ec.message() << std::endl;
    return;
  }

  llvm::legacy::PassManager pass;
  auto file_type = llvm::CodeGenFileType::ObjectFile;

  if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
    std::cerr << "Target machine can't emit file of this type" << std::endl;
    return;
  }

  pass.run(*module_);
  dest.flush();
}

} // namespace tuz
