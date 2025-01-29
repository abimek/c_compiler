#pragma once
#include <optional>
#include <string>
#include <vector>
#include "lexer.h"
#include <llvm/IR/Value.h>
namespace parser {

enum StatementType { VARIABLE_DECLERATION, FUNCTION_DECLERATION, STRUCT_DECLERATION };

struct ASTNode {
	virtual llvm::Value* codegen() = 0;
	virtual ~ASTNode() {}
};

struct Type {
  enum Kind { INT, FLOAT, VOID, CUSTOM};

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
  Factor   // Multiplication, Division
};

// Ran on expressions
enum InfixOperator { ADDITION, SUBTRACTION, DIVISION, MULTIPLICATION };


struct Statement{
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
  std::vector<Type> types;
  std::vector<std::string> vars;
};

struct FunctionStatement {
  std::string identifier;
  Type return_type;
  Prototype prototype;
	Block block;
};

struct Expression {
  ExpressionType type;
  void *expression;
};

struct ExpressionList {
  int num;
  std::vector<Expression*> expressions;
};

struct LiteralExpression {
  Type::Kind type;
  std::string value;
};

struct IdentifierExpression {
  std::string identifier;
};

enum PrefixOp { PLUS, MINUS };

struct PrefixExpression {
  PrefixOp prefix;
  Expression *expression;
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

struct VariableDeclerationStatement: public ASTNode{
  Type type;
  std::string name;
  Expression *expression;

  VariableDeclerationStatement(Type t, std::string n, Expression* e): type(t), name(n), expression(e) {}

	virtual llvm::Value* codegen() override;
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
                                           lexer::Token identifier_token);
Expression *parse_identifier_expression(Parser *parser,
                                        lexer::Token identifier_token);
Type parse_type(Parser *parser);
Expression *parse_expression(Parser *parser, Precedence precedence);
Expression *parse_literal_expression(Parser *parser);
Expression *parse_prefix(Parser *parser);
Expression *parse_ident(Parser *parser);
ExpressionList parse_expression_list(Parser *parser);
Expression *parse_binary_expression(Parser *parser, Expression *left,
                                    Precedence precedence);
Block parse_block(Parser *parser);
Statement parse_function_decleration(Parser *parser, Type type,
                                   lexer::Token identifier);
Prototype parse_prototype(Parser *parser);
}  // namespace parser
