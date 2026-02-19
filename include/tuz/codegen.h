#pragma once

#include "ast.h"
#include "type.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace tuz {

class CodeGenerator : public ASTVisitor {
public:
  CodeGenerator();

  // Generate LLVM IR for a complete program
  void generate(Program& program);

  // Get the generated module (ownership transferred to caller)
  std::unique_ptr<llvm::Module> get_module();

  // Compile and execute JIT (for REPL)
  int32_t execute_jit(const std::string& entry_function = "main");

  // Write LLVM IR to file
  void dump_ir(const std::string& filename);

  // Compile to object file
  void compile_to_object(const std::string& filename);

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
  // LLVM context and module
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;

  // Value stack for expression results
  std::vector<llvm::Value*> value_stack_;

  // Variable info including mutability
  struct VariableInfo {
    llvm::Value* value;
    bool is_mutable;
  };

  // Named values (variables) - maps name to variable info
  std::vector<std::unordered_map<std::string, VariableInfo>> named_values_;

  // Function and struct definitions
  std::unordered_map<std::string, llvm::Function*> functions_;
  std::unordered_map<std::string, llvm::StructType*> struct_types_;

  // Current function (for return statements)
  llvm::Function* current_function_;

  // Break/continue target blocks (for loops)
  struct LoopTargets {
    llvm::BasicBlock* continue_target;
    llvm::BasicBlock* break_target;
  };
  std::vector<LoopTargets> loop_stack_;

  // Helper methods
  llvm::Value* pop_value();
  void push_value(llvm::Value* val);

  // Type conversion
  llvm::Type* convert_type(TypePtr type);

  // Variable management
  llvm::Value* get_variable(const std::string& name);
  bool is_variable_mutable(const std::string& name);
  void set_variable(const std::string& name, llvm::Value* alloca, bool is_mutable = false);
  void enter_scope();
  void exit_scope();

  // Expression code generation (returns Value*)
  llvm::Value* codegen_expr(Expr& expr);
  llvm::Value* codegen_integer_literal(int64_t value, TypePtr type);
  llvm::Value* codegen_float_literal(double value, TypePtr type);
  llvm::Value* codegen_bool_literal(bool value);
  llvm::Value* codegen_binary_op(BinaryOp op, llvm::Value* left, llvm::Value* right,
                                 TypePtr result_type);
  llvm::Value* codegen_unary_op(UnaryOp op, llvm::Value* operand, TypePtr result_type);

  llvm::Value* codegen_expr_address(Expr& expr);

  // Statement code generation
  void codegen_stmt(Stmt& stmt);

  // Declaration code generation
  void codegen_decl(Decl& decl);

  // Function two-phase generation (for forward references)
  void declare_function(FunctionDecl& decl);
  void generate_function_body(FunctionDecl& decl);

  // Create entry block alloca
  llvm::AllocaInst* create_alloca(llvm::Type* type, const std::string& name);

  // Get pointer to element (for array/struct access)
  llvm::Value* get_element_ptr(llvm::Value* ptr, const std::vector<llvm::Value*>& indices,
                               llvm::Type* elem_type);
};

} // namespace tuz
