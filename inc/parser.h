#pragma once
#include <optional>
namespace parser_testing {
	void test_variable_decleration_no_initilization();
	void test_struct_decleration();
}

namespace parser {
	enum StatementType {
		VARIABLE_DECLERATION,
		FUNCTION,
		STRUCT_DECLERATION
	};

	// strings are currently not supported / turned off
	enum Primitive {
		INT_T,
		FLOAT_T,
		STRING_T
	};

	struct Type {
		bool is_primitive;
		std::optional<Primitive> primitive;
		std::string identifier;
	};

	enum ExpressionType {
		TwoPartOperatorExpressionType,
		IdentifierExpressionType,
		FunctionCallExpressionType,
	};

	// Ran on expressions
	enum Operator {
		ADDITION,
		SUBTRACTION,
		DIVISION,
		MULTIPLICATION
	};

	struct Statement {
		StatementType type;
		void* statement;
	};

	struct Parameters {
		int num;
		std::vector<Type> types; 
		std::vector<std::string> vars;
	};

	struct FunctionStatement {
		std::string identifier;
		Parameters parameters;
		std::vector<Statement> statements;
	};

	struct Expression {
		ExpressionType type;
		void* expression;
	};

	struct ExpressionList {
		int num;
		std::vector<void*> expressions;
	};

	struct LiteralExpression {
		Primitive type;
		std::string value;
	};

	struct IdentifierExpression {
		std::string identifier;
	};

	struct FunctionCallExpression {
		std::string identifier;
		Type return_type;
		ExpressionList expressions;
	};

	struct TwoPartOperatorExpression {
		Operator op;
		Expression* left;
		Expression* right;
	};

	struct VariableDeclerationStatement {
		Type type;
		std::string name;
		Expression* expression;
	};

	struct StructDeclerationStatement {
		int member_count; 
		std::string identifier;
		std::vector<Type> types;
		std::vector<std::string> identifiers;
	};

	struct Parser {
		//must begin at 0
		int index;
		std::vector<Token> tokens;
		std::vector<Statement> program;

		Token consume();
		Token peek();
		Token expect(TokenType type);
		Token read_ahead();
		bool ended();
	};

	std::vector<Statement> parse(std::vector<Token> tokens);
	Statement parse_statement(Parser* parser);
	Statement parse_type_statement(Parser* parser);
	Statement parse_struct_statement(Parser* parser);
	Expression* parse_function_call_expression(Parser* parser, Token identifier_token);
	Expression* parse_identifier_expression(Parser* parser, Token identifier_token);
	Type parse_type(Parser* parser);
	Expression* parse_expression(Parser* parser);
	Expression* parse_literal_expression(Parser* parser);
}
