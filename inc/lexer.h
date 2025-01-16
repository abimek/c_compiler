#pragma once
#include <string>
#include <vector>
#include <iostream>
enum TokenType {
	IDENTIFIER,

	//LANGUAGE
	IF,

	//PRIMITIVES
	INT,
	FLOAT,

	//types
	STRING,
	INT_DATA,
	FLOAT_DATA,

	// brackets/Parenthesis
	LPAREN,
	RPAREN,
	LCBRACKET,
	RCBRACKET,
	LHBRACKET,
	RHBRACKET,

	//MISC
	SEMICOLON,
	COLON,
	EQUALS,
	DOUBLEEQUALS,
	COMMENT,

	//MATH
	PLUS,
	MINUS,
	MULTIPLY,
	DIVIDE,

};
std::string token_type_to_string(TokenType t);
// token type
struct Token {
	TokenType type;
	std::string content;
};
std::ostream& operator<<(std::ostream& os, Token token);

struct Lexer{
	int last_char;
	int current_char;
	std::string sourcecode;
	std::vector<Token> tokens;

	void step();
	char read_current_char();
	char read_next_char();
};

std::vector<Token> tokenize(std::string sourcecode);
