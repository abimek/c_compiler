#include "parser.h"

#include <cmath>
#include <iterator>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "lexer.h"

namespace parser {

lexer::Token Parser::read_ahead() {
  lexer::Token token = tokens[index + 1];
  index++;
  return token;
}

lexer::Token Parser::consume() {
  lexer::Token token = tokens[index];
  index++;
  return token;
}

lexer::Token Parser::peek() { return tokens[index]; }

lexer::Token Parser::expect(lexer::TokenType type) {
  lexer::Token token = consume();
  if (token.type == type) {
    return token;
  }
  throw std::runtime_error("Unexpected token found, expected <" +
                           lexer::token_type_to_string(type) + ">, found: <" +
                           lexer::token_type_to_string(token.type) + ">");
}

bool Parser::ended() { return index >= tokens.size(); }

/*
 * Main parsing function, passing in a list of tokens to be parsed into an
 * AST represented via a vector of statements.
 */
std::vector<Statement> parse(std::vector<lexer::Token> tokens) {
  Parser parser = {0, tokens};
  while (!parser.ended()) {
    Statement statement = parse_statement(&parser);
    parser.expect(lexer::SEMICOLON);
    parser.program.push_back(statement);
  }
  return parser.program;
}

// This parses a statement
Statement parse_statement(Parser *parser) {
  switch (parser->peek().type) {
    case lexer::INT:
    case lexer::FLOAT:
      return parse_type_statement(parser);
    case lexer::STRUCT:
      return parse_struct_statement(parser);
    default:
      throw std::runtime_error("unexpected error");
  }
}

/*
 * Parses a statement that begins with a type defintion, typically a function
 * or variable decleration => This will include identifiers => might need to
 * rename this function
 */
Statement parse_type_statement(Parser *parser) {
  Type type = parse_type(parser);
  lexer::Token identifier = parser->expect(lexer::IDENTIFIER);
  lexer::TokenType token_type = parser->peek().type;

  // Parsing variable declerations (either with expresion or no expression)
  switch (token_type) {
    case lexer::SEMICOLON:
    case lexer::EQUALS:
      return parse_variable_decleration(parser, type, identifier.content);
    case lexer::LPAREN:
      parser->consume();
      // TODO: Parse Function
    default:
      throw std::runtime_error("Unexpected type");
  }
}

/*
 * This parses a variable decleration, which can be either a variable
 * decleration with an expression or a variable decleration without an
 * expression.
 */
Statement parse_variable_decleration(Parser *parser, Type type,
                                     std::string identifier) {
  if (parser->peek().type == lexer::EQUALS) {
    parser->consume();
    Expression *expr = parse_expression(parser, Precedence::Lowest);
    VariableDeclerationStatement *var =
        new VariableDeclerationStatement{type, identifier, expr};
    Statement stmt = {VARIABLE_DECLERATION, var};
    return stmt;
  }
  if (parser->peek().type == lexer::SEMICOLON) {
    VariableDeclerationStatement *var =
        new VariableDeclerationStatement{type, identifier, nullptr};
    Statement stmt = {VARIABLE_DECLERATION, var};
    return stmt;
  }
  throw std::runtime_error("Invalid variable decleration");
}

/*
 * Parses a type into a Type Struct, this will soon include identifiers when I
 * get to that stage of the program
 */
Type parse_type(Parser *parser) {
  lexer::Token t_type = parser->consume();
  Type type = {true, std::nullopt, ""};
  switch (t_type.type) {
    case lexer::INT:
      type.primitive = Type::INT_T;
      break;
    case lexer::FLOAT:
      type.primitive = Type::FLOAT_T;
      break;
    case lexer::STRING:
      type.primitive = Type::STRING_T;
      break;
    case lexer::IDENTIFIER:
      // IMPLEMENT LATER
    default:
      throw std::runtime_error("INVALID TYPE");
  }
  return type;
}

/*
 * This parses a function using a Pratt Parser (hopefully)
 *
 * 5*5+5+3*2
 * (5*5) + (5+3*2)
 * (5*5) + (5 + (3*2))
 */
Expression *parse_expression(Parser *parser, Precedence precedence) {
  Expression *expr = expr;
  switch (parser->peek().type) {
    case lexer::INT_DATA:
    case lexer::FLOAT_DATA:
      expr = parse_literal_expression(parser);
      break;
    // Parse prefix operators
    case lexer::PLUS:
    case lexer::MINUS:
      expr = parse_prefix(parser);
      break;
    case lexer::IDENTIFIER:
      // TODO: Should also attempt to parse a parameter call / read
      expr = parse_ident(parser);
      break;
    default:
      break;
  }

  // we must first check to see if we reached a SEMICOLON so we can later read
  // the precedence of the next operator
  switch (parser->peek().type) {
    case lexer::PLUS:
    case lexer::MINUS:
    case lexer::MULTIPLY:
    case lexer::DIVIDE:
      break;
    default:
      return expr;
  }

  Precedence next_prec = infix_operator_to_precendence(
      token_to_infix_operator(parser->peek().type));

  while (next_prec >= precedence) {
    switch (parser->peek().type) {
      case lexer::PLUS:
      case lexer::MINUS:
      case lexer::MULTIPLY:
      case lexer::DIVIDE:
        expr = parse_binary_expression(parser, expr, precedence);
        break;
    }
    if (parser->peek().type == lexer::SEMICOLON) {
      break;
    }
    next_prec = infix_operator_to_precendence(
        token_to_infix_operator(parser->peek().type));
  }
  return expr;
}

/*
 * Parse a binary expression such as addition or multiplication, handles the
 * appropriate opperator precedence
 */
Expression *parse_binary_expression(Parser *parser, Expression *left,
                                    Precedence precedence) {
  InfixOperator op = token_to_infix_operator(parser->consume().type);
  Precedence prec = infix_operator_to_precendence(op);
  return new Expression{
      BinaryOperatorExpressionType,
      new BinaryOperatorExpression{op, left, parse_expression(parser, prec)}};
}

/*
 * Utility function that turns a token to an ifnix operators
 * TODO: Move to a utilities file later
 */
InfixOperator token_to_infix_operator(lexer::TokenType token_t) {
  switch (token_t) {
    case lexer::PLUS:
      return InfixOperator::ADDITION;
    case lexer::MINUS:
      return InfixOperator::SUBTRACTION;
    case lexer::MULTIPLY:
      return InfixOperator::MULTIPLICATION;
    case lexer::DIVIDE:
      return InfixOperator::DIVISION;
    default:
      throw std::runtime_error("Unepxect infix opeartor");
  }
}

/*
 * Turns and InfixOperator to it's Precedence
 * TODO: Move to a utitlies file later
 */
Precedence infix_operator_to_precendence(InfixOperator op) {
  switch (op) {
    case InfixOperator::ADDITION:
    case InfixOperator::SUBTRACTION:
      return Precedence::Term;
    case InfixOperator::MULTIPLICATION:
    case InfixOperator::DIVISION:
      return Precedence::Factor;
    default:
      throw std::runtime_error(
          "Unexpected Infix Operator To Precedance Conversion");
  }
}

PrefixOp token_to_prefix(lexer::Token token) {
  switch (token.type) {
    case lexer::PLUS:
      return PrefixOp::PLUS;
    case lexer::MINUS:
      return PrefixOp::MINUS;
    default:
      throw std::runtime_error("Unexpected prefix: " +
                               lexer::token_type_to_string(token.type));
  }
}

/*
 * Parse Prefix Operators
 */
Expression *parse_prefix(Parser *parser) {
  lexer::Token prefix = parser->consume();
  Expression *expr = parse_expression(parser, Precedence::Lowest);
  return new Expression{PrefixExpressionType,
                        new PrefixExpression{token_to_prefix(prefix), expr}};
}

/*
 * Parses a literal / primitive (Integer, Float, soon to be string)
 * TODO: String
 */
Expression *parse_literal_expression(Parser *parser) {
  lexer::Token literal = parser->consume();
  Type::PrimitiveType type;
  switch (literal.type) {
    // Must add string check
    case lexer::INT_DATA:
      type = Type::INT_T;
      break;
    case lexer::FLOAT_DATA:
      type = Type::FLOAT_T;
      break;
    default:
      throw std::runtime_error("unexpected");
  }
  return new Expression{LiteralExpressionType,
                        new LiteralExpression{type, literal.content}};
}

/*
 * Parses somethign that begins with an identifier, might also parser the (.)
 * construct
 */
Expression *parse_ident(Parser *parser) {
  lexer::Token identifier = parser->consume();
  switch (parser->peek().type) {
    case lexer::LPAREN:
      return parse_function_call_expression(parser, identifier);
    default:
      return parse_identifier_expression(parser, identifier);
  }
}

/*
 * Parses an identifer expression, which is basically just a variable being
 * used
 */
Expression *parse_identifier_expression(Parser *parser,
                                        lexer::Token identifier_token) {
  return new Expression{IdentifierExpressionType,
                        new IdentifierExpression{identifier_token.content}};
}

// TODO: Implement Func Call
Expression *parse_function_call_expression(Parser *parser,
                                           lexer::Token identifier_token) {
  ExpressionList expr_list = parse_expression_list(parser);
  parser->expect(lexer::RPAREN);
  return new Expression{
      FunctionCallExpressionType,
      new FunctionCallExpression{identifier_token.content, expr_list}};
}

ExpressionList parse_expression_list(Parser *parser) {
  parser->expect(lexer::LPAREN);
  ExpressionList expr_list = ExpressionList{};
  while (parser->peek().type != lexer::RPAREN) {
    expr_list.num += 1;
    expr_list.expressions.push_back(
        parse_expression(parser, Precedence::Lowest));
    if (parser->peek().type != lexer::RPAREN) {
      parser->expect(lexer::COMMA);
    }
  }
  return expr_list;
}

// this is called with the token paramter type being STRUCT
Statement parse_struct_statement(Parser *parser) {
  parser->consume();
  lexer::Token struct_name = parser->expect(lexer::IDENTIFIER);

  StructDeclerationStatement *struct_t =
      new StructDeclerationStatement{0, struct_name.content, {}, {}};

  Statement stmt = {STRUCT_DECLERATION, struct_t};
  parser->expect(lexer::LCBRACKET);

  while (parser->peek().type != lexer::RCBRACKET) {
    Type param_type = parse_type(parser);
    lexer::Token param_name = parser->expect(lexer::IDENTIFIER);
    parser->expect(lexer::SEMICOLON);
    struct_t->types.push_back(param_type);
    struct_t->identifiers.push_back(param_name.content);
    struct_t->member_count++;
  }

  parser->expect(lexer::RCBRACKET);
  return stmt;
}
}  // namespace parser

// compares two dfifferent ast's
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
