#pragma once
#include "parser.h"
namespace parser_testing {
void test_function_call();
void test_variable_decleration_no_initilization();
void test_struct_decleration();
void test_prefix();
void test_literal();
void test_add();
void test_should_fail();
void test_operator_precedence();
void test_function_statement();
}  // namespace parser_testing

namespace ast_comparer {
bool expressions_equal(parser::Expression *expr1, parser::Expression *expr2);
bool prefix_expressions_equal(parser::PrefixExpression *expr1,
                              parser::PrefixExpression *expr2);
bool identifier_expressions_equal(parser::IdentifierExpression *expr1,
                                  parser::IdentifierExpression *expr2);
bool literal_expressions_equal(parser::LiteralExpression *expr1,
                               parser::LiteralExpression *expr2);
bool binary_operator_expressions_equal(parser::BinaryOperatorExpression *expr1,
                                       parser::BinaryOperatorExpression *expr2);
bool types_equal(parser::Type type1, parser::Type type2);
bool function_call_expressions_equal(parser::FunctionCallExpression *expr1,
                                     parser::FunctionCallExpression *expr2);
bool expression_list_equal(parser::ExpressionList list1,
                           parser::ExpressionList list2);
bool programs_equal(parser::Program p1, parser::Program p2);
bool prototypes_equal(parser::Prototype proto1, parser::Prototype proto2);
bool blocks_equal(parser::Block block1, parser::Block block2);
bool function_declerations_equal(parser::FunctionStatement *func1,
                                 parser::FunctionStatement *func2);
}  // namespace ast_comparer
