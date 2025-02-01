#include "generator.h"

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <cwchar>
#include <map>
#include <memory>
#include <stdexcept>
#include <variant>

#include "lexer.h"
#include "parser.h"

// -----------------------------------------------------------------------------
// Generator genreates LLVM IR from the AST Tree
// -----------------------------------------------------------------------------

namespace generator {

void SymbolTable::clear() { table.clear(); }

CodeGenerator::CodeGenerator()
    : mod("<translation_unit>", context), builder(context) {}

// Executes the generation process on a program
void execute(parser::Program program) {
  CodeGenerator generator = CodeGenerator{};
  for (parser::Statement stmt : program.block.statements) {
    switch (stmt.type) {
      case parser::StatementType::FUNCTION_DECLERATION:
        generator.generate_function_statement(
            (parser::FunctionStatement*)(&stmt));
        break;
      case parser::StatementType::VARIABLE_DECLERATION:
        generator.generate_global_variable(
            (parser::VariableDeclerationStatement*)(&stmt));
        break;
      default:
        throw std::runtime_error(
            "code generation for statement is currently not implemented");
    }
  }
}

llvm::Value* CodeGenerator::generate_global_variable(
    parser::VariableDeclerationStatement* var_stmt) {
  //	return new llvm::GlobalVariable(mod, generate_type(var_stmt->type),
  //true, llvm::GlobalValue::ExternalLinkage, , var_stmt->name);
}

llvm::Value* CodeGenerator::generate_binop_expression(
    parser::BinaryOperatorExpression* binop) {}

llvm::Value* CodeGenerator::generate_identifier_expression(
    parser::IdentifierExpression* ident) {
  llvm::Value* val = symb_tbls_search(ident->identifier);
  if (val == nullptr) {
    throw std::runtime_error("Identifier does not exist");
  }
  return val;
}

llvm::Value* CodeGenerator::generate_literal_expression(
    parser::LiteralExpression* literal_expr) {
  switch (literal_expr->type) {
    case parser::Type::Kind::FLOAT:
      return llvm::ConstantFP::get(
          context,
          llvm::APFloat(
              ((parser::FloatLiteral*)(literal_expr->literal))->literal));
    case parser::Type::Kind::INT:
      return llvm::ConstantInt::get(
          context,
          llvm::APInt(
              32,
              (int)((parser::IntLiteral*)(literal_expr->literal))->literal));
    default:
      throw std::runtime_error(
          "Void cant be a litteral expression, and custom literal expressions "
          "are currently disabled");
  }
}

llvm::Value* CodeGenerator::generate_expression(parser::Expression* expr) {
  llvm::Value* val;
  if (expr == nullptr || expr->expression == nullptr) {
    return nullptr;
  }
  switch (expr->type) {
    case parser::IdentifierExpressionType: {
      parser::IdentifierExpression* ident_expr =
          (parser::IdentifierExpression*)(expr->expression);
      val = generate_identifier_expression(ident_expr);
    } break;
    case parser::LiteralExpressionType: {
      parser::LiteralExpression* lit_expr =
          (parser::LiteralExpression*)(expr->expression);
      val = generate_literal_expression(lit_expr);
    } break;
    case parser::BinaryOperatorExpressionType: {
      parser::BinaryOperatorExpression* bin_expr =
          (parser::BinaryOperatorExpression*)(expr->expression);
      val = generate_binop_expression(bin_expr);
    } break;
  }
  return val;
}

// Returns nullptr if the symbol doesn't exist
llvm::Value* CodeGenerator::symb_tbls_search(std::string ident) {
  if (current_symbol_table.table.count(ident)) {
    return current_symbol_table.table[ident];
  }
  if (global_symbol_table.table.count(ident)) {
    return global_symbol_table.table[ident];
  }
  return nullptr;
}
// Adds a function into the
llvm::Function* CodeGenerator::generate_function_statement(
    parser::FunctionStatement* func_stmt) {
  generate_prototype(func_stmt->prototype);
  llvm::Function* func = mod.getFunction(func_stmt->prototype.identifier);
  if (!func) {
    return nullptr;
  }
  llvm::BasicBlock* BB = llvm::BasicBlock::Create(context, "entry", func);
  builder.SetInsertPoint(BB);
  current_symbol_table.clear();
  for (auto& arg : func->args()) {
    current_symbol_table.table[std::string(arg.getName())] = &arg;
  }
  generate_function_block(func_stmt->block);
  return func;
}

llvm::Value* CodeGenerator::generate_function_block(parser::Block block) {}

// generates a tpye from a basic type
// Implement custom types later
llvm::Type* CodeGenerator::generate_type(parser::Type type) {
  switch (type.kind) {
    case parser::Type::INT:
      return llvm::Type::getInt32Ty(context);
    case parser::Type::FLOAT:
      return llvm::Type::getFloatTy(context);
    case parser::Type::VOID:
      return llvm::Type::getVoidTy(context);
    case parser::Type::CUSTOM:
      throw std::runtime_error("Custom types currently not implmeented");
  }
}

llvm::Function* CodeGenerator::generate_prototype(parser::Prototype prototype) {
  std::vector<llvm::Type*> param_types = {};
  for (parser::Type type : prototype.types) {
    param_types.push_back(generate_type(type));
  }
  llvm::Type* return_type = generate_type(prototype.return_type);
  llvm::FunctionType* func_type =
      llvm::FunctionType::get(return_type, param_types, false);
  llvm::Function* function = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, prototype.identifier, mod);
  int idx = 0;
  for (auto& arg : function->args()) {
    arg.setName(prototype.vars[idx++]);
  }
  return function;
}

}  // namespace generator
