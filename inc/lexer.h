#pragma once
#include <iostream>
#include <string>
#include <vector>

namespace lexer {
enum TokenType {
  IDENTIFIER,

  // KEYWORDS
  IF,
  ELSE,
  STRUCT,
  RETURN,
  TRUE,
  FALSE,

  // PRIMITIVES
  INT,
  FLOAT,
  VOID,
  BOOL,

  // types, actually holds the data
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

  // MISC
  SEMICOLON,
  COLON,
  EQUALS,
  DOUBLEEQUALS,
  COMMENT,
  COMMA,
  PERIOD,

  // MATH
  PLUS,
  MINUS,
  MULTIPLY,
  DIVIDE,
  GThan,
  LThan,
};

std::string token_type_to_string(TokenType t);

// token type
struct Token {
  TokenType type;
  std::string content;
};
std::ostream &operator<<(std::ostream &os, Token token);

// Main lexer construct
struct Lexer {
  int last_char;
  int current_char;
  std::string sourcecode;
  std::vector<Token> tokens;

  void step();
  char read_current_char();
  char read_next_char();
};

std::vector<Token> tokenize(std::string sourcecode);

// predefined funcs
void lex_string(Lexer *lexer);
void lex_identifier(Lexer *lexer);
void lex_number(Lexer *lexer);
void lex_equal(Lexer *lexer);
void lex_slash(Lexer *lexer);
bool is_int_or_letter(char ch);
bool is_int(char ch);
bool is_letter(char ch);
}  // namespace lexer
