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
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include <cwchar>
#include <map>
#include <memory>
#include <optional>
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
            (parser::FunctionStatement*)(stmt.statement));
        break;
      case parser::StatementType::VARIABLE_DECLERATION:
        generator.generate_global_variable(
            (parser::VariableDeclerationStatement*)(stmt.statement));
        break;
      default:
        throw std::runtime_error(
            "code generation for statement is currently not implemented");
    }
  }
  generator.mod.print(llvm::errs(), nullptr);
}

llvm::Value* CodeGenerator::generate_global_variable(
    parser::VariableDeclerationStatement* var_stmt) {
  llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(
      generate_expression(var_stmt->expression, generate_type(var_stmt->type)));
  global_symbol_table.table[var_stmt->name] = constant;
  std::string name = var_stmt->name;
  return new llvm::GlobalVariable(mod, generate_type(var_stmt->type), false,
                                  llvm::GlobalValue::ExternalLinkage, constant,
                                  name);
}

llvm::Value* CodeGenerator::generate_variable_statement(
    parser::VariableDeclerationStatement* var_stmt) {};

llvm::Value* CodeGenerator::generate_function_call(
    parser::FunctionCallExpression* func_call) {
  llvm::Function* func = mod.getFunction(func_call->identifier);
  if (!func) {
    throw std::runtime_error("Invalid function call, function does not exist");
  }
  std::vector<llvm::Value*> values = {};
  int idx = 0;
  for (parser::Expression* expr : func_call->expressions.expressions) {
    values.push_back(generate_expression(
        expr, func->getFunctionType()->getParamType(idx++)));
  }
  return builder.CreateCall(func, values, "calltmp");
}

llvm::Value* CodeGenerator::generate_binop_expression(
    parser::BinaryOperatorExpression* binop, llvm::Type* type) {
  llvm::Value* lhs = generate_expression(binop->left, type);
  llvm::Value* rhs = generate_expression(binop->right, type);

  if (type->isFloatTy()) {
    switch (binop->op) {
      case parser::InfixOperator::ADDITION:
        return builder.CreateFAdd(lhs, rhs, "addtmp");
      case parser::InfixOperator::MULTIPLICATION:
        return builder.CreateFMul(lhs, rhs, "multmp");
      case parser::InfixOperator::SUBTRACTION:
        return builder.CreateFSub(lhs, rhs, "subtmp");
      case parser::InfixOperator::DIVISION:
        return builder.CreateFDiv(lhs, rhs, "divtmp");
      case parser::InfixOperator::GREATER_THAN:
        return builder.CreateFCmpOGT(lhs, rhs, "gtcmp");
      case parser::InfixOperator::LESS_THAN:
        return builder.CreateFCmpOLT(lhs, rhs, "ltcmp");
      case parser::InfixOperator::EQUAL:
        return builder.CreateFCmpOEQ(lhs, rhs, "equaltmp");
    }
  }
  if (type->isIntegerTy()) {
    switch (binop->op) {
      case parser::InfixOperator::ADDITION:
        return builder.CreateAdd(lhs, rhs, "addtmp");
      case parser::InfixOperator::MULTIPLICATION:
        return builder.CreateMul(lhs, rhs, "multmp");
      case parser::InfixOperator::SUBTRACTION:
        return builder.CreateSub(lhs, rhs, "subtmp");
      case parser::InfixOperator::DIVISION:
        return builder.CreateSDiv(lhs, rhs, "divtmp");
      case parser::InfixOperator::GREATER_THAN:
        return builder.CreateICmpSGT(lhs, rhs, "gttmp");
      case parser::InfixOperator::LESS_THAN:
        return builder.CreateICmpSLT(lhs, rhs, "lttmp");
      case parser::InfixOperator::EQUAL:
        return builder.CreateICmpEQ(lhs, rhs, "equaltmp");
    }
  }
  throw std::runtime_error("Bro what");
}

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

llvm::Value* CodeGenerator::generate_expression(parser::Expression* expr,
                                                llvm::Type* type) {
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
    case parser::FunctionCallExpressionType: {
      parser::FunctionCallExpression* call_expr =
          (parser::FunctionCallExpression*)(expr->expression);
      val = generate_function_call(call_expr);
    } break;
    case parser::BinaryOperatorExpressionType: {
      parser::BinaryOperatorExpression* bin_expr =
          (parser::BinaryOperatorExpression*)(expr->expression);
      val = generate_binop_expression(bin_expr, type);
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

llvm::Value* CodeGenerator::generate_function_call_statement(
    parser::FunctionCallStatement* func_stmt) {
  return generate_function_call(new parser::FunctionCallExpression{
      func_stmt->identifier, func_stmt->expressions});
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
  generate_block(func_stmt->block, func->getReturnType());
  return func;
}

llvm::Value* CodeGenerator::generate_return_statement(
    parser::ReturnStatement* stmt, llvm::Type* ret_type) {
  return builder.CreateRet(generate_expression(stmt->expr, ret_type));
}

// The return value is meaningless and shouldn't really be used for anything tbh
llvm::Value* CodeGenerator::generate_if_statement(parser::IfStatement* stmt) {
  llvm::Value* conditional =
      generate_expression(stmt->cond, llvm::Type::getInt1Ty(context));
  llvm::Function* func = builder.GetInsertBlock()->getParent();
  ;
  llvm::BasicBlock* than_block =
      llvm::BasicBlock::Create(context, "than", func);
  llvm::BasicBlock* else_block =
      llvm::BasicBlock::Create(context, "else", func);
  llvm::BasicBlock* merge_block =
      llvm::BasicBlock::Create(context, "merge", func);

  if (stmt->else_block != std::nullopt) {
    builder.CreateCondBr(conditional, than_block, else_block);
  } else {
    builder.CreateCondBr(conditional, than_block, merge_block);
  }

  builder.SetInsertPoint(than_block);
  generate_block(stmt->than_block, func->getReturnType());
  builder.CreateBr(merge_block);

  if (stmt->else_block != std::nullopt) {
    builder.SetInsertPoint(else_block);
    generate_block(stmt->else_block.value(), func->getReturnType());
    builder.CreateBr(merge_block);
  }
  builder.SetInsertPoint(merge_block);
  return conditional;
}

llvm::Value* CodeGenerator::generate_block(parser::Block block,
                                           llvm::Type* ret_type) {
  for (parser::Statement stmt : block.statements) {
    switch (stmt.type) {
      case parser::StatementType::RETURN_STATEMENT:
        generate_return_statement((parser::ReturnStatement*)(stmt.statement),
                                  ret_type);
        break;
      case parser::StatementType::IF_STATEMENT:
        generate_if_statement((parser::IfStatement*)(stmt.statement));
        break;
      case parser::StatementType::FUNC_CALL_STATEMENT:
        generate_function_call_statement(
            (parser::FunctionCallStatement*)(stmt.statement));
        break;
      default:
        throw std::runtime_error(
            "code generation for statement is currently not implemented");
    }
  }
  return nullptr;
}

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
    case parser::Type::BOOL:
      return llvm::Type::getInt1Ty(context);
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
