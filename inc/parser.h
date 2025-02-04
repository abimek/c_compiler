#pragma once
#include <llvm/IR/Value.h>

#include <optional>
#include <string>
#include <vector>

#include "lexer.h"
namespace parser {

enum StatementType {
  VARIABLE_DECLERATION,
  FUNCTION_DECLERATION,
  STRUCT_DECLERATION,
  RETURN_STATEMENT,
  ASSIGNMENT_STATEMENT,
  IF_STATEMENT,
	FUNC_CALL_STATEMENT
};

struct Type {
  // Add string type thing alter
  enum Kind { INT, FLOAT, VOID, BOOL, CUSTOM };

  Kind kind;
  // Identifier is used for custom types
  std::string identifier;
};

enum ExpressionType {
  BinaryOperatorExpressionType,
  LiteralExpressionType,
  IdentifierExpressionType,
  FunctionCallExpressionType,
  PrefixExpressionType
};

enum Precedence {
  Lowest,  // Lowest Precendence, initally passed
  Term,    // Addition Subtraction
  Factor,  // Multiplication, Division,
  Compare
};

// Ran on expressions
enum InfixOperator {
  ADDITION,
  SUBTRACTION,
  DIVISION,
  MULTIPLICATION,
  GREATER_THAN,
  LESS_THAN,
  EQUAL
};

struct Statement {
  StatementType type;
  void *statement;
};

struct Block {
  std::vector<Statement> statements;
};

struct Program {
  Block block;
};

struct Prototype {
  int num;
  std::string identifier;
  Type return_type;
  std::vector<Type> types;
  std::vector<std::string> vars;
};

struct FunctionStatement {
  Prototype prototype;
  Block block;
};

struct Expression {
  ExpressionType type;
  void *expression;
};

struct ReturnStatement {
  Expression *expr;
};

struct ExpressionList {
  int num;
  std::vector<Expression *> expressions;
};

struct LiteralExpression {
  Type::Kind type;
  void *literal;
};

struct IntLiteral {
  int literal;
};

struct FloatLiteral {
  float literal;
};

struct BoolLiteral {
  int literal;
};

struct VariableAssignmentStatement {
  std::string identifier;
  Expression *expression;
};

struct IfStatement {
  Expression *cond;
  Block than_block;
  std::optional<Block> else_block;
};

struct IdentifierExpression {
  std::string identifier;
};

enum PrefixOp { PLUS, MINUS };

struct PrefixExpression {
  PrefixOp prefix;
  Expression *expression;
};

struct FunctionCallStatement{
  std::string identifier;
  ExpressionList expressions;
};

struct FunctionCallExpression {
  std::string identifier;
  ExpressionList expressions;
};

struct BinaryOperatorExpression {
  InfixOperator op;
  Expression *left;
  Expression *right;
};

struct VariableDeclerationStatement {
  Type type;
  std::string name;
  Expression *expression;
};

struct StructDeclerationStatement {
  int member_count;
  std::string identifier;
  std::vector<Type> types;
  std::vector<std::string> identifiers;
};

struct Parser {
  // must begin at 0
  int index;
  std::vector<lexer::Token> tokens;
  std::vector<Statement> program;

  lexer::Token consume();
  lexer::Token peek();
  lexer::Token peek_ahead();
  lexer::Token expect(lexer::TokenType type);
  lexer::Token read_ahead();

  bool ended();
};

Program parse(std::vector<lexer::Token> tokens);
Statement parse_variable_decleration(Parser *parser, Type type,
                                     std::string identifier);
Statement parse_statement(Parser *parser);
Statement parse_type_statement(Parser *parser);
Statement parse_struct_statement(Parser *parser);
Expression *parse_function_call_expression(Parser *parser,
                                           lexer::Token identifier);
Expression *parse_identifier_expression(Parser *parser,
                                        lexer::Token identifier_token);
Type parse_type(Parser *parser);
Statement parse_if_statement(Parser *parser);
Expression *parse_expression(Parser *parser, Precedence precedence);
Expression *parse_literal_expression(Parser *parser);
Expression *parse_prefix(Parser *parser);
Expression *parse_ident(Parser *parser);
ExpressionList parse_expression_list(Parser *parser);
Expression *parse_binary_expression(Parser *parser, Expression *left,
                                    Precedence precedence);
Block parse_block(Parser *parser);
Statement parse_function_call_statement(Parser *parser);
Statement parse_return_statement(Parser *parser);
Statement parse_assignment_statement(Parser *parser);
Statement parse_identifier_statement(Parser* parser);
Statement parse_function_decleration(Parser *parser, Type type,
                                     lexer::Token identifier);
Prototype parse_prototype(Parser *parser, Type return_type,
                          std::string identifier);
}  // namespace parser
