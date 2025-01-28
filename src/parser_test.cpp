#include "parser.h"

#include <string>
#include <vector>

#include "lexer.h"
#include "parser_test.h"

namespace parser_testing {

std::string bool_to_status(bool val) {
  if (val) {
    return "suceeded";
  }
  return "failed";
}

void test_should_fail() {
  std::string sourcecode = "int myint = -6;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint",
          new parser::Expression{parser::PrefixExpressionType, nullptr}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test should_fail: " << bool_to_status(suceeded) << std::endl;
}

void test_literal() {
  std::string sourcecode = "int myint = -5;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint",
          new parser::Expression{
              parser::PrefixExpressionType,
              new parser::PrefixExpression{
                  parser::PrefixOp::MINUS,
                  new parser::Expression{parser::LiteralExpressionType,
                                         new parser::LiteralExpression{
                                             parser::Type::INT_T, "5"}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test literal: " << bool_to_status(suceeded) << std::endl;
}

void test_prefix() {
  std::string sourcecode = "int myint = -test;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint",
          new parser::Expression{
              parser::PrefixExpressionType,
              new parser::PrefixExpression{
                  parser::PrefixOp::MINUS,
                  new parser::Expression{
                      parser::IdentifierExpressionType,
                      new parser::IdentifierExpression{"test"}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_prefix: " << bool_to_status(suceeded) << std::endl;
}

void test_function_call() {
  std::string sourcecode = "int myint = -test(yay, test);";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint",
          new parser::Expression{
              parser::PrefixExpressionType,
              new parser::PrefixExpression{
                  parser::PrefixOp::MINUS,
                  new parser::Expression{
                      parser::FunctionCallExpressionType,
                      new parser::FunctionCallExpression{
                          "test",
                          parser::ExpressionList{
                              2,
                              {new parser::Expression{
                                   parser::IdentifierExpressionType,
                                   new parser::IdentifierExpression{"yay"}},
                               new parser::Expression{
                                   parser::IdentifierExpressionType,
                                   new parser::IdentifierExpression{
                                       "test"}}}}}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_function_call: " << bool_to_status(suceeded)
            << std::endl;
}

void test_operator_precedence() {
  std::string sourcecode = "int myint = 5*5+5+5*6+5;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint",
          new parser::Expression{
              parser::BinaryOperatorExpressionType,
              new parser::BinaryOperatorExpression{
                  parser::InfixOperator::ADDITION,
                  new parser::Expression{
                      parser::BinaryOperatorExpressionType,
                      new parser::BinaryOperatorExpression{
                          parser::InfixOperator::MULTIPLICATION,
                          new parser::Expression{parser::LiteralExpressionType,
                                                 new parser::LiteralExpression{
                                                     parser::Type::INT_T, "5"}},
                          new parser::Expression{
                              parser::LiteralExpressionType,
                              new parser::LiteralExpression{parser::Type::INT_T,
                                                            "5"}}}},
                  new parser::Expression{
                      parser::BinaryOperatorExpressionType,
                      new parser::BinaryOperatorExpression{
                          parser::InfixOperator::ADDITION,
                          new parser::Expression{parser::LiteralExpressionType,
                                                 new parser::LiteralExpression{
                                                     parser::Type::INT_T, "5"}},
                          new parser::Expression{
                              parser::BinaryOperatorExpressionType,
                              new parser::BinaryOperatorExpression{
                                  parser::InfixOperator::ADDITION,
                                  new parser::Expression{
                                      parser::BinaryOperatorExpressionType,
                                      new parser::BinaryOperatorExpression{
                                          parser::InfixOperator::MULTIPLICATION,
                                          new parser::Expression{
                                              parser::LiteralExpressionType,
                                              new parser::LiteralExpression{
                                                  parser::Type::INT_T, "5"}},
                                          new parser::Expression{
                                              parser::LiteralExpressionType,
                                              new parser::LiteralExpression{
                                                  parser::Type::INT_T, "6"}}}},
                                  new parser::Expression{
                                      parser::LiteralExpressionType,
                                      new parser::LiteralExpression{
                                          parser::Type::INT_T, "5"}}}}}},
              }}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_operator_precedence: " << bool_to_status(suceeded)
            << std::endl;
}

void test_add() {
  std::string sourcecode = "int myint = 5+5;";
  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint",
          new parser::Expression{
              parser::BinaryOperatorExpressionType,
              new parser::BinaryOperatorExpression{
                  parser::InfixOperator::ADDITION,
                  new parser::Expression{
                      parser::LiteralExpressionType,
                      new parser::LiteralExpression{parser::Type::INT_T, "5"}},
                  new parser::Expression{parser::LiteralExpressionType,
                                         new parser::LiteralExpression{
                                             parser::Type::INT_T, "5"}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_add: " << bool_to_status(suceeded) << std::endl;
}

void test_function_statement() {
  std::string sourcecode = 
		"\
		int myint(int myint){\
			int myint;\
		}";

  std::vector<parser::Statement> validation_program;

  parser::FunctionStatement *func_stmt = new parser::FunctionStatement{
      "myint",
      parser::Type{true, parser::Type::INT_T, ""},
      parser::Parameters{
          1, {parser::Type{true, parser::Type::INT_T, ""}}, {"myint"}},
      {
				parser::Statement{
					parser::VARIABLE_DECLERATION,

      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint", nullptr}
				}
			}};

  validation_program.push_back(parser::Statement{parser::FUNCTION, func_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test function_decleration: " << bool_to_status(suceeded)
            << std::endl;
}

void test_variable_decleration_no_initilization() {
  std::string sourcecode = "int myint;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{true, parser::Type::INT_T, ""}, "myint", nullptr};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_variable_decleration_no_initilization: "
            << bool_to_status(suceeded) << std::endl;
}

void test_struct_decleration() {
  std::string sourcecode =
      "\
			struct mystruct \
			{\
				int x; \
				float y;\
			}; \
		";

  std::vector<parser::Statement> validation_program;

  std::vector<parser::Type> struct_types = {
      parser::Type{true, parser::Type::INT_T, ""},
      parser::Type{true, parser::Type::FLOAT_T, ""}};

  std::vector<std::string> struct_identifiers = {"x", "y"};

  parser::StructDeclerationStatement *decl_stmt =
      new parser::StructDeclerationStatement{2, "mystruct", struct_types,
                                             struct_identifiers};
  validation_program.push_back(
      parser::Statement{parser::STRUCT_DECLERATION, decl_stmt});

  bool suceeded = ast_comparer::programs_equal(
      validation_program, parser::parse(lexer::tokenize(sourcecode)));

  std::cout << "test test_struct_decleration: " << bool_to_status(suceeded)
            << std::endl;
}

void test_prefix_expression() {}
}  // namespace parser_testing

/*
 * This is where the AST Comparison code is contained, used for validating the
 * parser (inside of parser_testing)
 */
namespace ast_comparer {

bool binary_operator_expressions_equal(
    parser::BinaryOperatorExpression *expr1,
    parser::BinaryOperatorExpression *expr2) {
  return (expr1->op == expr2->op) &&
         expressions_equal(expr1->left, expr2->left) &&
         expressions_equal(expr1->right, expr2->right);
}

bool expression_list_equal(parser::ExpressionList list1,
                           parser::ExpressionList list2) {
  if (list1.num != list2.num) {
    return false;
  }
  for (int i = 0; i < list1.expressions.size(); i++) {
    if (!expressions_equal(list1.expressions[i], list2.expressions[i])) {
      return false;
    }
  }
  return true;
}

bool function_call_expressions_equal(parser::FunctionCallExpression *expr1,
                                     parser::FunctionCallExpression *expr2) {
  return (expr1->identifier == expr2->identifier) &&
         expression_list_equal(expr1->expressions, expr2->expressions);
}

bool identifier_expressions_equal(parser::IdentifierExpression *expr1,
                                  parser::IdentifierExpression *expr2) {
  return (expr1->identifier == expr2->identifier);
}

bool literal_expressions_equal(parser::LiteralExpression *expr1,
                               parser::LiteralExpression *expr2) {
  return (expr1->type == expr2->type) && (expr1->value == expr2->value);
}

bool prefix_expressions_equal(parser::PrefixExpression *expr1,
                              parser::PrefixExpression *expr2) {
  return (expr1->prefix == expr2->prefix) &&
         expressions_equal(expr1->expression, expr2->expression);
}

// TODO: implement expression matching
bool expressions_equal(parser::Expression *expr1, parser::Expression *expr2) {
  bool equal = false;
  if (expr1 == nullptr && expr2 == nullptr) {
    return true;
  }
  if (expr1->expression == nullptr && expr2->expression == nullptr) {
    return true;
  }
  if (!(expr1 != nullptr && expr2 != nullptr)) {
    return false;
  }
  if (!(expr1->expression != nullptr && expr2->expression != nullptr)) {
    return false;
  }
  if (expr1->type != expr2->type) {
    return false;
  }
  switch (expr1->type) {
    case parser::IdentifierExpressionType: {
      parser::IdentifierExpression *ident_expr1 =
          (parser::IdentifierExpression *)(expr1->expression);
      parser::IdentifierExpression *ident_expr2 =
          (parser::IdentifierExpression *)(expr2->expression);
      equal = identifier_expressions_equal(ident_expr1, ident_expr2);
    } break;
    case parser::PrefixExpressionType: {
      parser::PrefixExpression *pre_expr1 =
          (parser::PrefixExpression *)(expr1->expression);
      parser::PrefixExpression *pre_expr2 =
          (parser::PrefixExpression *)(expr2->expression);
      equal = prefix_expressions_equal(pre_expr1, pre_expr2);
    } break;
    case parser::LiteralExpressionType: {
      parser::LiteralExpression *lit_expr1 =
          (parser::LiteralExpression *)(expr1->expression);
      parser::LiteralExpression *lit_expr2 =
          (parser::LiteralExpression *)(expr2->expression);
      equal = literal_expressions_equal(lit_expr1, lit_expr2);
    } break;
    case parser::FunctionCallExpressionType: {
      parser::FunctionCallExpression *func_expr1 =
          (parser::FunctionCallExpression *)(expr1->expression);
      parser::FunctionCallExpression *func_expr2 =
          (parser::FunctionCallExpression *)(expr2->expression);
      equal = function_call_expressions_equal(func_expr1, func_expr2);
    } break;
    case parser::BinaryOperatorExpressionType: {
      parser::BinaryOperatorExpression *bin_expr1 =
          (parser::BinaryOperatorExpression *)(expr1->expression);
      parser::BinaryOperatorExpression *bin_expr2 =
          (parser::BinaryOperatorExpression *)(expr2->expression);
      equal = binary_operator_expressions_equal(bin_expr1, bin_expr2);
    } break;
  }
  return equal;
}

bool types_equal(parser::Type type1, parser::Type type2) {
  return (type1.is_primitive == type2.is_primitive) &&
         (type1.identifier == type2.identifier) &&
         (type1.primitive == type2.primitive);
}

bool variable_declerations_equal(parser::VariableDeclerationStatement *decl1,
                                 parser::VariableDeclerationStatement *decl2) {
  return (decl1->name == decl2->name) &&
         types_equal(decl1->type, decl2->type) &&
         expressions_equal(decl1->expression, decl2->expression);
}

bool type_vector_equal(std::vector<parser::Type> vec1,
                       std::vector<parser::Type> vec2) {
  if (vec1.size() != vec2.size()) {
    return false;
  }
  for (int i = 0; i < vec1.size(); i++) {
    if (!types_equal(vec1[i], vec2[i])) {
      return false;
    }
  }
  return true;
}

bool string_vector_equal(std::vector<std::string> vec1,
                         std::vector<std::string> vec2) {
  if (vec1.size() != vec2.size()) {
    return false;
  }
  for (int i = 0; i < vec1.size(); i++) {
    if (vec1[i] != vec2[i]) {
      return false;
    }
  }
  return true;
}

bool struct_declerations_equal(parser::StructDeclerationStatement *decl1,
                               parser::StructDeclerationStatement *decl2) {
  return (decl1->member_count == decl2->member_count) &&
         (decl1->identifier == decl2->identifier) &&
         type_vector_equal(decl1->types, decl2->types) &&
         string_vector_equal(decl1->identifiers, decl2->identifiers);
}

bool parameters_equal(parser::Parameters param1, parser::Parameters param2) {
  return (param1.num == param2.num) &&
         type_vector_equal(param1.types, param2.types) &&
         string_vector_equal(param1.vars, param2.vars);
}

bool function_declerations_equal(parser::FunctionStatement *func1,
                                 parser::FunctionStatement *func2) {
  return (func1->identifier == func2->identifier) &&
         types_equal(func1->return_type, func2->return_type) &&
         parameters_equal(func1->parameters, func2->parameters) &&
         programs_equal(func1->statements, func2->statements);
}

bool statements_equal(parser::Statement stmt1, parser::Statement stmt2) {
  if (stmt1.type != stmt2.type) {
    return false;
  }
  bool equal = false;
  switch (stmt1.type) {
    case parser::VARIABLE_DECLERATION: {
      parser::VariableDeclerationStatement *decl_stmt1 =
          (parser::VariableDeclerationStatement *)(stmt1.statement);
      parser::VariableDeclerationStatement *decl_stmt2 =
          (parser::VariableDeclerationStatement *)(stmt2.statement);
      equal = variable_declerations_equal(decl_stmt1, decl_stmt2);
    } break;
    case parser::STRUCT_DECLERATION: {
      parser::StructDeclerationStatement *decl_stmt1 =
          (parser::StructDeclerationStatement *)(stmt1.statement);
      parser::StructDeclerationStatement *decl_stmt2 =
          (parser::StructDeclerationStatement *)(stmt2.statement);
      equal = struct_declerations_equal(decl_stmt1, decl_stmt2);
    } break;
    case parser::FUNCTION: {
      parser::FunctionStatement *func_stmt1 =
          (parser::FunctionStatement *)(stmt1.statement);
      parser::FunctionStatement *func_stmt2 =
          (parser::FunctionStatement *)(stmt2.statement);
      equal = function_declerations_equal(func_stmt1, func_stmt2);
    } break;
  }
  return equal;
}

bool programs_equal(std::vector<parser::Statement> ast1,
                    std::vector<parser::Statement> ast2) {
  for (int i = 0; i < ast1.size(); i++) {
    parser::Statement stmt1 = ast1[i];
    parser::Statement stmt2 = ast2[i];
    if (!statements_equal(stmt1, stmt2)) {
      return false;
    }
  }
  return true;
}
}  // namespace ast_comparer
// testing for the parser
