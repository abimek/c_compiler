#pragma once
#include <optional>
namespace parser_testing {
	void test_variable_decleration_no_initilization();
	void test_struct_decleration();
	void test_prefix();
}

namespace parser {

	enum StatementType {
		VARIABLE_DECLERATION,
		FUNCTION,
		STRUCT_DECLERATION
	};

	struct Type {
		enum PrimitiveType {
			INT_T,
			FLOAT_T,
			STRING_T,
			VOID
		};

		bool is_primitive;
		std::optional<PrimitiveType> primitive;
		std::string identifier;
	};

	enum ExpressionType {
		BinaryOperatorExpressionType,
		LiteralExpressionType,
		IdentifierExpressionType,
		FunctionCallExpressionType,
		PrefixExpressionType
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
		Type::PrimitiveType type;
		std::string value;
	};

	struct IdentifierExpression {
		std::string identifier;
	};

	enum Prefix {
		PLUS_PREFIX,
		MINUS_PREFIX
	};

	struct PrefixExpression {
		Prefix prefix;
		Expression* expression;
	};

	struct FunctionCallExpression {
		std::string identifier;
		Type return_type;
		ExpressionList expressions;
	};

	struct BinaryOperatorExpression {
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
		std::vector<lexer::Token> tokens;
		std::vector<Statement> program;

		lexer::Token consume();
		lexer::Token peek();
		lexer::Token expect(lexer::TokenType type);
		lexer::Token read_ahead();

		bool ended();
	};

	Statement parse_variable_decleration(Parser* parser, Type type, std::string identifier);
	std::vector<Statement> parse(std::vector<lexer::Token> tokens);
	Statement parse_statement(Parser* parser);
	Statement parse_type_statement(Parser* parser);
	Statement parse_struct_statement(Parser* parser);
	Expression* parse_function_call_expression(Parser* parser, lexer::Token identifier_token);
	Expression* parse_identifier_expression(Parser* parser, lexer::Token identifier_token);
	Type parse_type(Parser* parser);
	Expression* parse_expression(Parser* parser);
	Expression* parse_literal_expression(Parser* parser);
	Expression* parse_prefix(Parser* parser);
	Prefix token_to_prefix(lexer::Token token);
	Expression* parse_ident(Parser* parser);
}

namespace ast_comparer {
	bool expressions_equal(parser::Expression* expr1, parser::Expression* expr2);
	bool prefix_expressions_equal(parser::PrefixExpression* expr1, parser::PrefixExpression* expr2);
	bool identifier_expressions_equal(parser::IdentifierExpression* expr1, parser::IdentifierExpression* expr2);
	bool types_equal(parser::Type type1, parser::Type type2);
}

