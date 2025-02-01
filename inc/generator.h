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

struct CodeGenerator {
  llvm::LLVMContext context;
  llvm::Module mod;
  llvm::IRBuilder<> builder;
  SymbolTable global_symbol_table;  // contains symbols for the general program
                                    // (global variables and functions)
  SymbolTable
      current_symbol_table;  // This contains stuff for the current function
  CodeGenerator();

  void execute(parser::Program program);
  llvm::Function* generate_function_statement(
      parser::FunctionStatement* func_stmt);
  llvm::Type* generate_type(parser::Type type);
  llvm::Function* generate_prototype(parser::Prototype prototype);
  llvm::Value* generate_function_block(parser::Block block);
  llvm::Value* generate_global_variable(
      parser::VariableDeclerationStatement* var_stmt);
  llvm::Value* generate_expression(parser::Expression* expr);
  llvm::Value* generate_binop_expression(
      parser::BinaryOperatorExpression* binop);
  llvm::Value* generate_identifier_expression(
      parser::IdentifierExpression* ident);
  llvm::Value* symb_tbls_search(std::string ident);
  llvm::Value* generate_literal_expression(
      parser::LiteralExpression* literal_expr);
};
}  // namespace generator
