#pragma once
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <map>
#include <vector>

#include "parser.h"

namespace generator {

struct SymbolTable {
  std::map<std::string, llvm::Value*> table;
  void clear();
};

void execute(parser::Program program);

struct CodeGenerator {
  llvm::LLVMContext context;
  llvm::Module mod;
  llvm::IRBuilder<> builder;
  SymbolTable global_symbol_table;  // contains symbols for the general program
                                    // (global variables and functions)
  SymbolTable
      current_symbol_table;  // This contains stuff for the current function
  CodeGenerator();

  llvm::Function* generate_function_statement(
      parser::FunctionStatement* func_stmt);
  llvm::Type* generate_type(parser::Type type);
  llvm::Function* generate_prototype(parser::Prototype prototype);
  llvm::Value* generate_block(parser::Block block,
                                       llvm::Type* ret_type);
  llvm::Value* generate_global_variable(
      parser::VariableDeclerationStatement* var_stmt);
  llvm::Value* generate_return_statement(parser::ReturnStatement* stmt,
                                         llvm::Type* ret_type);
  llvm::Value* generate_expression(parser::Expression* expr, llvm::Type* type);
  llvm::Value* generate_binop_expression(
      parser::BinaryOperatorExpression* binop, llvm::Type* type);
  llvm::Value* generate_identifier_expression(
      parser::IdentifierExpression* ident);
  llvm::Value* symb_tbls_search(std::string ident);
  llvm::Value* generate_literal_expression(
      parser::LiteralExpression* literal_expr);
  llvm::Value* generate_function_call(
      parser::FunctionCallExpression* func_call);
  llvm::Value* generate_function_call_statement(
      parser::FunctionCallStatement* func_stmt);
  llvm::Value* generate_variable_statement(
      parser::VariableDeclerationStatement* var_stmt);
	llvm::Value* generate_if_statement(parser::IfStatement* stmt);
};
}  // namespace generator
