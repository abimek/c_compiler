#include "parser.h"

#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

#include "generator.h"
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
          parser::Type{parser::Type::Kind::INT, ""}, "myint",
          new parser::Expression{parser::PrefixExpressionType, nullptr}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test should_fail: " << bool_to_status(suceeded) << std::endl;
}

void test_literal() {
  std::string sourcecode = "int myint = -5;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{parser::Type::Kind::INT, ""}, "myint",
          new parser::Expression{
              parser::PrefixExpressionType,
              new parser::PrefixExpression{
                  parser::PrefixOp::MINUS,
                  new parser::Expression{parser::LiteralExpressionType,
                                         new parser::LiteralExpression{
                                             parser::Type::Kind::INT,
                                             new parser::IntLiteral{5}}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test literal: " << bool_to_status(suceeded) << std::endl;
}

void test_prefix() {
  std::string sourcecode = "int myint = -test;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{parser::Type::Kind::INT, ""}, "myint",
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
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_prefix: " << bool_to_status(suceeded) << std::endl;
}

void test_function_call() {
  std::string sourcecode =
      "int myint = -test(yay, test);\
		test(yay);";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{parser::Type::Kind::INT, ""}, "myint",
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
  parser::FunctionCallStatement *func_stmt = new parser::FunctionCallStatement{
      "test", parser::ExpressionList{
                  1,
                  {new parser::Expression{
                      parser::ExpressionType::IdentifierExpressionType,
                      new parser::IdentifierExpression{"yay"}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  validation_program.push_back(
      parser::Statement{parser::FUNC_CALL_STATEMENT, func_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_function_call: " << bool_to_status(suceeded)
            << std::endl;
}

void test_generate_global() {
  std::string sourcecode =
      "\
			int mynumber = 25;\
			int useless(){\
			}\
		int myint(int myint){\
			if(myint == 6){\
				return 6;\
			}else{\
				useless();\
			}\
			return myint+80;\
		}\
		//testing\
		int main(){\
			return myint(5+30) + mynumber;\
		}\
	";
  generator::execute(parser::parse(lexer::tokenize(sourcecode)));
}

void test_operator_precedence() {
  std::string sourcecode = "int myint = 5*5+5+5*6+5;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{parser::Type::Kind::INT, ""}, "myint",
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
                                  parser::Type::Kind::INT,
                                  new parser::IntLiteral{5}}},
                          new parser::Expression{
                              parser::LiteralExpressionType,
                              new parser::LiteralExpression{
                                  parser::Type::Kind::INT,
                                  new parser::IntLiteral{5}}}}},
                  new parser::Expression{
                      parser::BinaryOperatorExpressionType,
                      new parser::BinaryOperatorExpression{
                          parser::InfixOperator::ADDITION,
                          new parser::Expression{
                              parser::LiteralExpressionType,
                              new parser::LiteralExpression{
                                  parser::Type::Kind::INT,
                                  new parser::IntLiteral{5}}},
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
                                                  parser::Type::Kind::INT,
                                                  new parser::IntLiteral{5}}},
                                          new parser::Expression{
                                              parser::LiteralExpressionType,
                                              new parser::LiteralExpression{
                                                  parser::Type::Kind::INT,
                                                  new parser::IntLiteral{6}}}}},
                                  new parser::Expression{
                                      parser::LiteralExpressionType,
                                      new parser::LiteralExpression{
                                          parser::Type::Kind::INT,
                                          new parser::IntLiteral{5}}}}}}},
              }}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_operator_precedence: " << bool_to_status(suceeded)
            << std::endl;
}

void test_add() {
  std::string sourcecode = "int myint = 5+5;";
  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{parser::Type::Kind::INT, ""}, "myint",
          new parser::Expression{
              parser::BinaryOperatorExpressionType,
              new parser::BinaryOperatorExpression{
                  parser::InfixOperator::ADDITION,
                  new parser::Expression{
                      parser::LiteralExpressionType,
                      new parser::LiteralExpression{parser::Type::Kind::INT,
                                                    new parser::IntLiteral{5}}},
                  new parser::Expression{parser::LiteralExpressionType,
                                         new parser::LiteralExpression{
                                             parser::Type::Kind::INT,
                                             new parser::IntLiteral{5}}}}}};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test test_add: " << bool_to_status(suceeded) << std::endl;
}

void test_if_statement() {
  std::string sourcecode =
      "\
		int myint(int myint){\
			if(5==5){\
				myint = 6;\
			}else{\
				myint = 7;\
			}\
			return myint;\
		}";

  std::vector<parser::Statement> validation_program;

  parser::FunctionStatement *func_stmt = new parser::FunctionStatement{
      parser::Prototype{1,
                        "myint",
                        parser::Type{parser::Type::Kind::INT, ""},
                        {parser::Type{parser::Type::Kind::INT, ""}},
                        {"myint"}},
      parser::Block{
          {parser::Statement{
               parser::IF_STATEMENT,
               new parser::IfStatement{
                   new parser::Expression{
                       parser::BinaryOperatorExpressionType,
                       new parser::BinaryOperatorExpression{
                           parser::InfixOperator::EQUAL,
                           new parser::Expression{
                               parser::LiteralExpressionType,
                               new parser::LiteralExpression{
                                   parser::Type::Kind::INT,
                                   new parser::IntLiteral{5}}},
                           new parser::Expression{
                               parser::LiteralExpressionType,
                               new parser::LiteralExpression{
                                   parser::Type::Kind::INT,
                                   new parser::IntLiteral{5}}}}},
                   parser::Block{{parser::Statement{
                       parser::ASSIGNMENT_STATEMENT,
                       new parser::VariableAssignmentStatement{
                           "myint",
                           new parser::Expression{
                               parser::LiteralExpressionType,
                               new parser::LiteralExpression{
                                   parser::Type::Kind::INT,
                                   new parser::IntLiteral{6}}}}}}},
                   parser::Block{{parser::Statement{
                       parser::ASSIGNMENT_STATEMENT,
                       new parser::VariableAssignmentStatement{
                           "myint",
                           new parser::Expression{
                               parser::LiteralExpressionType,
                               new parser::LiteralExpression{
                                   parser::Type::Kind::INT,
                                   new parser::IntLiteral{7}}}}}}}}},
           parser::Statement{parser::RETURN_STATEMENT,
                             new parser::ReturnStatement{new parser::Expression{
                                 parser::IdentifierExpressionType,
                                 new parser::IdentifierExpression{"myint"}}}}

          }}};

  validation_program.push_back(
      parser::Statement{parser::FUNCTION_DECLERATION, func_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test if_decleration: " << bool_to_status(suceeded) << std::endl;
}

void test_function_statement() {
  std::string sourcecode =
      "\
		int myint(int myint){\
			int myint;\
			return myint;\
		}";

  std::vector<parser::Statement> validation_program;

  parser::FunctionStatement *func_stmt = new parser::FunctionStatement{
      parser::Prototype{1,
                        "myint",
                        parser::Type{parser::Type::Kind::INT, ""},
                        {parser::Type{parser::Type::Kind::INT, ""}},
                        {"myint"}},
      parser::Block{
          {parser::Statement{parser::VARIABLE_DECLERATION,

                             new parser::VariableDeclerationStatement{
                                 parser::Type{parser::Type::Kind::INT, ""},
                                 "myint", nullptr}},
           parser::Statement{
               parser::RETURN_STATEMENT,
               new parser::ReturnStatement{new parser::Expression{
                   parser::IdentifierExpressionType,
                   new parser::IdentifierExpression{"myint"}}}}}}};

  validation_program.push_back(
      parser::Statement{parser::FUNCTION_DECLERATION, func_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test function_decleration: " << bool_to_status(suceeded)
            << std::endl;
}

void test_var_assignment() {
  std::string sourcecode =
      "\
		int myint(int myint){\
			myint = 5;\
			return myint;\
		}";

  std::vector<parser::Statement> validation_program;

  parser::FunctionStatement *func_stmt = new parser::FunctionStatement{
      parser::Prototype{1,
                        "myint",
                        parser::Type{parser::Type::Kind::INT, ""},
                        {parser::Type{parser::Type::Kind::INT, ""}},
                        {"myint"}},
      parser::Block{
          {parser::Statement{
               parser::ASSIGNMENT_STATEMENT,
               new parser::VariableAssignmentStatement{
                   "myint",
                   new parser::Expression{parser::LiteralExpressionType,
                                          new parser::LiteralExpression{
                                              parser::Type::Kind::INT,
                                              new parser::IntLiteral{5}}}}},
           parser::Statement{
               parser::RETURN_STATEMENT,
               new parser::ReturnStatement{new parser::Expression{
                   parser::IdentifierExpressionType,
                   new parser::IdentifierExpression{"myint"}}}}}}};

  validation_program.push_back(
      parser::Statement{parser::FUNCTION_DECLERATION, func_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
  std::cout << "test assignment_and_return: " << bool_to_status(suceeded)
            << std::endl;
}
void test_variable_decleration_no_initilization() {
  std::string sourcecode = "int myint;";

  std::vector<parser::Statement> validation_program;
  parser::VariableDeclerationStatement *decl_stmt =
      new parser::VariableDeclerationStatement{
          parser::Type{parser::Type::Kind::INT, ""}, "myint", nullptr};
  validation_program.push_back(
      parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));
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
      parser::Type{parser::Type::Kind::INT, ""},
      parser::Type{parser::Type::Kind::FLOAT, ""}};

  std::vector<std::string> struct_identifiers = {"x", "y"};

  parser::StructDeclerationStatement *decl_stmt =
      new parser::StructDeclerationStatement{2, "mystruct", struct_types,
                                             struct_identifiers};
  validation_program.push_back(
      parser::Statement{parser::STRUCT_DECLERATION, decl_stmt});

  bool suceeded = ast_comparer::programs_equal(
      parser::Program{parser::Block{validation_program}},
      parser::parse(lexer::tokenize(sourcecode)));

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

bool return_statements_equal(parser::ReturnStatement *ret_stmt1,
                             parser::ReturnStatement *ret_stmt2) {
  return expressions_equal(ret_stmt1->expr, ret_stmt2->expr);
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
  switch (expr1->type) {
    case parser::Type::Kind::INT:
      return ((parser::IntLiteral *)(expr1->literal))->literal ==
             ((parser::IntLiteral *)(expr2->literal))->literal;
    case parser::Type::Kind::FLOAT:
      return ((parser::FloatLiteral *)(expr1->literal))->literal ==
             ((parser::FloatLiteral *)(expr2->literal))->literal;
    case parser::Type::Kind::BOOL:
      return ((parser::BoolLiteral *)(expr1->literal))->literal ==
             ((parser::BoolLiteral *)(expr2->literal))->literal;
    default:
      throw std::runtime_error(
          "Custom types not implmenet - literal epxressions equal");
  }
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
  return (type1.identifier == type2.identifier) && (type1.kind == type2.kind);
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

bool prototype_equal(parser::Prototype proto1, parser::Prototype proto2) {
  return (proto1.num == proto2.num) &&
         (proto1.identifier == proto2.identifier) &&
         types_equal(proto1.return_type, proto2.return_type) &&
         type_vector_equal(proto1.types, proto2.types) &&
         string_vector_equal(proto1.vars, proto2.vars);
}

bool if_statements_equal(parser::IfStatement *if_stmt1,
                         parser::IfStatement *if_stmt2) {
  if (if_stmt1->else_block == std::nullopt &&
      if_stmt2->else_block == std::nullopt) {
    return expressions_equal(if_stmt1->cond, if_stmt2->cond) &&
           blocks_equal(if_stmt1->than_block, if_stmt2->than_block);
  }
  if (if_stmt1->else_block != std::nullopt &&
      if_stmt2->else_block != std::nullopt) {
    return expressions_equal(if_stmt1->cond, if_stmt2->cond) &&
           blocks_equal(if_stmt1->than_block, if_stmt2->than_block) &&
           blocks_equal(*if_stmt1->else_block, *if_stmt2->else_block);
  }
  return false;
}

bool function_declerations_equal(parser::FunctionStatement *func1,
                                 parser::FunctionStatement *func2) {
  return prototype_equal(func1->prototype, func2->prototype) &&
         blocks_equal(func1->block, func2->block);
}

bool assignment_statements_equal(parser::VariableAssignmentStatement *stmt1,
                                 parser::VariableAssignmentStatement *stmt2) {
  return (stmt1->identifier == stmt2->identifier) &&
         expressions_equal(stmt1->expression, stmt2->expression);
}

bool function_call_statements_equal(parser::FunctionCallStatement *call1,
                                    parser::FunctionCallStatement *call2) {
  return (call1->identifier == call2->identifier) &&
         expression_list_equal(call1->expressions, call2->expressions);
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
    case parser::IF_STATEMENT: {
      parser::IfStatement *if_stmt1 = (parser::IfStatement *)(stmt1.statement);
      parser::IfStatement *if_stmt2 = (parser::IfStatement *)(stmt2.statement);
      equal = if_statements_equal(if_stmt1, if_stmt2);
    } break;
    case parser::STRUCT_DECLERATION: {
      parser::StructDeclerationStatement *decl_stmt1 =
          (parser::StructDeclerationStatement *)(stmt1.statement);
      parser::StructDeclerationStatement *decl_stmt2 =
          (parser::StructDeclerationStatement *)(stmt2.statement);
      equal = struct_declerations_equal(decl_stmt1, decl_stmt2);
    } break;
    case parser::FUNCTION_DECLERATION: {
      parser::FunctionStatement *func_stmt1 =
          (parser::FunctionStatement *)(stmt1.statement);
      parser::FunctionStatement *func_stmt2 =
          (parser::FunctionStatement *)(stmt2.statement);
      equal = function_declerations_equal(func_stmt1, func_stmt2);
    } break;
    case parser::RETURN_STATEMENT: {
      parser::ReturnStatement *ret_stmt1 =
          (parser::ReturnStatement *)(stmt1.statement);
      parser::ReturnStatement *ret_stmt2 =
          (parser::ReturnStatement *)(stmt2.statement);

      equal = return_statements_equal(ret_stmt1, ret_stmt2);
    } break;
    case parser::ASSIGNMENT_STATEMENT: {
      parser::VariableAssignmentStatement *assign_stmt =
          (parser::VariableAssignmentStatement *)(stmt1.statement);
      parser::VariableAssignmentStatement *assign_stmt2 =
          (parser::VariableAssignmentStatement *)(stmt2.statement);
      equal = assignment_statements_equal(assign_stmt, assign_stmt2);
    } break;
    case parser::FUNC_CALL_STATEMENT: {
      parser::FunctionCallStatement *func_call_stmt =
          (parser::FunctionCallStatement *)(stmt1.statement);
      parser::FunctionCallStatement *func_call_stmt2 =
          (parser::FunctionCallStatement *)(stmt2.statement);
      equal = function_call_statements_equal(func_call_stmt, func_call_stmt2);
    } break;
  }
  return equal;
}

bool blocks_equal(parser::Block block1, parser::Block block2) {
  for (int i = 0; i < block1.statements.size(); i++) {
    parser::Statement stmt1 = block1.statements[i];
    parser::Statement stmt2 = block2.statements[i];
    if (!statements_equal(stmt1, stmt2)) {
      return false;
    }
  }
  return true;
}

bool programs_equal(parser::Program p1, parser::Program p2) {
  return blocks_equal(p1.block, p2.block);
}
}  // namespace ast_comparer
// testing for the parser
