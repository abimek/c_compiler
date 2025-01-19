#include "lexer.h"
#include "parser.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <optional>

namespace parser {

	Token Parser::read_ahead() {
		Token token = tokens[index+1];
		index++;
		return token;
	}

	Token Parser::consume() {
		Token token = tokens[index];
		index++;
		return token;
	}

	Token Parser::peek() {
		return tokens[index];
	}

	Token Parser::expect(TokenType type) {
		Token token = consume();
		if(token.type == type){
			return token;
		}
		throw std::runtime_error("Unexpected token found, expected <" + token_type_to_string(type) + ">, found: <" + token_type_to_string(token.type) + ">");
	}

	bool Parser::ended() {
		return index >= tokens.size();
	}

	std::vector<Statement> parse(std::vector<Token> tokens){
		Parser parser = {0, tokens};
		while(!parser.ended()){
			Statement statement = parse_statement(&parser);
			parser.program.push_back(statement);
		}
		return parser.program;
	}

	Statement parse_statement(Parser* parser) {
		switch(parser->peek().type){
			case IDENTIFIER:
				throw std::runtime_error("custom types currently not supported, but should be included as a type statement");
				break;
			case INT:
			case FLOAT:
				return parse_type_statement(parser);
				break;
			case STRUCT:
				return parse_struct_statement(parser);
			default:
				throw std::runtime_error("unexpected error");
		}
	}

	// Parses statements that begin with type. Either a function, or vaiable
	// decleration
	Statement parse_type_statement(Parser* parser) {
		Type type = parse_type(parser);
		Token identifier = parser->expect(IDENTIFIER);
		TokenType token_type = parser->peek().type;
		if(token_type == EQUALS){
				parser->consume();
				Expression* expr = parse_expression(parser);
				VariableDeclerationStatement* var = new VariableDeclerationStatement{type, identifier.content, expr};
				Statement stmt = {VARIABLE_DECLERATION, var};
				return stmt;
		}
		if(token_type == LCBRACKET){
				parser->consume();
				//TODO: Parse Function
		}
		if(token_type == SEMICOLON){
				//var decleraiton with no assignment
				parser->consume();
				VariableDeclerationStatement* var = new VariableDeclerationStatement{type, identifier.content, nullptr};
				Statement stmt = {VARIABLE_DECLERATION, var};
				return stmt;
		}
		throw std::runtime_error("Unexpected type");
	}

	//TODO: implement custom types (strings/identifiers)
	Type parse_type(Parser* parser) {
		Token t_type = parser->consume();
		Type type = {true, std::nullopt, ""};
		switch(t_type.type){
			case INT:
				type.primitive = INT_T;
				break;
			case FLOAT:
				type.primitive = FLOAT_T;
				break;
			case STRING:
				type.primitive = STRING_T;
				break;
			case IDENTIFIER:
				//IMPLEMENT LATER
			default:
				throw std::runtime_error("INVALID TYPE");
		}
		return type;
	}



	//parses an expression
	//TODO: include strings
	//TODO: add method calls on structs later
	Expression* parse_expression(Parser* parser){
		Expression* expr = expr;

		Token token = parser->consume();
		switch(token.type){
			case INT_DATA:
			case FLOAT_DATA:
				expr = parse_literal_expression(parser);
				break;
			case IDENTIFIER:
				if(parser->peek().type == LPAREN){
					expr = parse_function_call_expression(parser, token);
				} else {
					expr = parse_identifier_expression(parser, token);
				}
				break;
			default:
				break;
		}

		Token next_t = parser->consume();
		return expr;
	}

	Expression* parse_identifier_expression(Parser* parser, Token identifier_token){
		return new Expression{IdentifierExpressionType, new IdentifierExpression{identifier_token.content}};
	}


	//TODO: Implement Func Call
	Expression* parse_function_call_expression(Parser* parser, Token identifier_token){
	}

	Expression* parse_literal_expression(Parser* parser){
		Token literal = parser->consume();
	}

	// this is called with the token paramter type being STRUCT
	Statement parse_struct_statement(Parser* parser) {
		parser->consume();
		Token struct_name = parser->expect(IDENTIFIER);
		StructDeclerationStatement* struct_t = new StructDeclerationStatement{0, struct_name.content, {}, {}};
		Statement stmt = {STRUCT_DECLERATION, struct_t};
		parser->expect(LCBRACKET);
		while(parser->peek().type != RCBRACKET){
			Type param_type = parse_type(parser);
			Token param_name = parser->expect(IDENTIFIER);
			parser->expect(SEMICOLON);

			struct_t->types.push_back(param_type);
			struct_t->identifiers.push_back(param_name.content);
			struct_t->member_count++;
		}
		parser->expect(RCBRACKET);
		parser->expect(SEMICOLON);
		return stmt;
	}
}

//compares two dfifferent ast's
namespace ast_comparer {

	//TODO: implement expression matching
	bool expressions_equal(parser::Expression* expr1, parser::Expression* expr2) {
		return true;
	}

	bool types_equal(parser::Type type1, parser::Type type2){
		return (type1.is_primitive == type2.is_primitive) && (type1.identifier == type2.identifier) && (type1.primitive == type2.primitive);
	}

	bool variable_declerations_equal(parser::VariableDeclerationStatement* decl1, parser::VariableDeclerationStatement* decl2){
		return (decl1->name == decl2->name) && types_equal(decl1->type, decl2->type) && expressions_equal(decl1->expression, decl2->expression);
	}

	bool type_vector_equal(std::vector<parser::Type> vec1, std::vector<parser::Type> vec2) {
		if(vec1.size() != vec2.size()){
			return false;
		}
		for(int i = 0; i < vec1.size(); i++){
			if(!types_equal(vec1[i], vec2[i])){
				return false;
			}
		}
		return true;
	}

	bool string_vector_equal(std::vector<std::string> vec1, std::vector<std::string> vec2) {
		if(vec1.size() != vec2.size()){
			return false;
		}
		for(int i = 0; i < vec1.size(); i++){
			if(vec1[i] != vec2[i]){
				return false;
			}
		}
		return true;
	}

	bool struct_declerations_equal(parser::StructDeclerationStatement* decl1, parser::StructDeclerationStatement* decl2){
		return (decl1->member_count == decl2->member_count) && (decl1->identifier == decl2->identifier) && type_vector_equal(decl1->types, decl2->types) && string_vector_equal(decl1->identifiers, decl2->identifiers);
	}

	bool statements_equal(parser::Statement stmt1, parser::Statement stmt2){
		if(stmt1.type != stmt2.type){
			return false;
		}
		bool equal = true;
		switch(stmt1.type){
			case parser::VARIABLE_DECLERATION:
				{
					parser::VariableDeclerationStatement* decl_stmt1 = (parser::VariableDeclerationStatement*)(stmt1.statement);
					parser::VariableDeclerationStatement* decl_stmt2 = (parser::VariableDeclerationStatement*)(stmt1.statement);
					equal = variable_declerations_equal(decl_stmt1, decl_stmt2);
				}
				break;
			case parser::STRUCT_DECLERATION:
				{
					parser::StructDeclerationStatement* decl_stmt1 = (parser::StructDeclerationStatement*)(stmt1.statement);
					parser::StructDeclerationStatement* decl_stmt2 = (parser::StructDeclerationStatement*)(stmt1.statement);
					equal = struct_declerations_equal(decl_stmt1, decl_stmt2);
				}
				break;
		}
		return equal;
	}

	bool programs_equal(std::vector<parser::Statement> ast1, std::vector<parser::Statement> ast2){
		for(int i = 0; i < ast1.size(); i++){
			parser::Statement stmt1 = ast1[i];
			parser::Statement stmt2 = ast2[i];
			if(!statements_equal(stmt1, stmt2)){
				return false;
			}
		}
		return true;
	}
}
//testing for the parser
namespace parser_testing {

	std::string bool_to_status(bool val){
		if(val){
			return "suceeded";
		}
		return "failed";
	}

	void test_variable_decleration_no_initilization(){
		std::string sourcecode = "int myint;";

		std::vector<parser::Statement> validation_program;
		parser::VariableDeclerationStatement* decl_stmt = new parser::VariableDeclerationStatement{
			parser::Type{true, parser::INT_T, ""},
			"myint",
			nullptr
		};
		validation_program.push_back(parser::Statement{parser::VARIABLE_DECLERATION, decl_stmt});
		bool suceeded = ast_comparer::programs_equal(validation_program, parser::parse(tokenize(sourcecode)));
		std::cout << "test test_variable_decleration_no_initilization: " << bool_to_status(suceeded) << std::endl;
	}

	void test_struct_decleration(){
		std::string sourcecode = "struct mystruct {int x; float y;};";

		std::vector<parser::Statement> validation_program;

		std::vector<parser::Type> struct_types ={parser::Type{true, parser::INT_T, ""}, parser::Type{true, parser::FLOAT_T, ""}};
		std::vector<std::string> struct_identifiers = {"x", "y"};
		parser::StructDeclerationStatement* decl_stmt = new parser::StructDeclerationStatement{
			2,
			"mystruct",
			struct_types,
			struct_identifiers
		};
		validation_program.push_back(parser::Statement{parser::STRUCT_DECLERATION, decl_stmt});
		bool suceeded = ast_comparer::programs_equal(validation_program, parser::parse(tokenize(sourcecode)));
		std::cout << "test test_struct_decleration: " << bool_to_status(suceeded) << std::endl;
	}
}

