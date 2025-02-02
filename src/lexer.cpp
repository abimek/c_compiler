#include "lexer.h"

#include <cwchar>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace lexer {
// Lexer defined methods
void Lexer::step() {
  last_char += 1;
  current_char += 1;
}

char Lexer::read_current_char() {
  if (current_char >= sourcecode.length()) {
    throw std::runtime_error("Unable to read next char");
  }
  return sourcecode[current_char];
}

char Lexer::read_next_char() {
  if (current_char + 1 >= sourcecode.length()) {
    throw std::runtime_error("Unable to read next char");
  }
  return sourcecode[current_char + 1];
}

//<< Overriding for printing out Token
std::ostream &operator<<(std::ostream &os, Token token) {
  // Customize how the struct is printed
  os << "Token { type: " << token_type_to_string(token.type) << ", content: \""
     << token.content << "\" }";
  return os;
}

std::string token_type_to_string(TokenType t) {
  switch (t) {
    case IDENTIFIER:
      return "IDENTIFIER";
    case IF:
      return "IF";
    case ELSE:
      return "ELSE";
    case INT:
      return "INT";
    case STRUCT:
      return "STRUCT";
    case FLOAT:
      return "FLOAT";
    case STRING:
      return "STRING";
    case INT_DATA:
      return "INT_DATA";
    case FLOAT_DATA:
      return "FLOAT_DATA";
    case LPAREN:
      return "LPAREN";
    case RPAREN:
      return "RPAREN";
    case LCBRACKET:
      return "LCBRACKET";
    case RCBRACKET:
      return "RCBRACKET";
    case LHBRACKET:
      return "LHBRACKET";
    case RHBRACKET:
      return "RHBRACKET";
    case SEMICOLON:
      return "SEMICOLON";
    case COLON:
      return "COLON";
    case EQUALS:
      return "EQUALS";
    case DOUBLEEQUALS:
      return "DOUBLEEQUALS";
    case COMMENT:
      return "COMMENT";
    case COMMA:
      return "COMMA";
    case PERIOD:
      return "PERIOD";
    case PLUS:
      return "PLUS";
    case MINUS:
      return "MINUS";
    case MULTIPLY:
      return "MULTIPLY";
    case DIVIDE:
      return "DIVIDE";
    case RETURN:
      return "RETURN";
    case VOID:
      return "VOID";
    case GThan:
      return "GTHAN";
    case LThan:
      return "LTHAN";
    case BOOL:
      return "BOOL";
    case TRUE:
      return "TRUE";
    case FALSE:
      return "FALSE";
  }
}

// CODE
std::vector<Token> tokenize(std::string sourcecode) {
  Lexer lexer;
  lexer.last_char = -1;
  lexer.current_char = 0;
  lexer.sourcecode = sourcecode;

  while (lexer.current_char < lexer.sourcecode.length()) {
    char current_char = lexer.read_current_char();
    switch (current_char) {
      case '+':
        lexer.tokens.push_back({PLUS});
        lexer.step();
        break;
      case '-':
        lexer.tokens.push_back({MINUS});
        lexer.step();
        break;
      case '*':
        lexer.tokens.push_back({MULTIPLY});
        lexer.step();
        break;
      case ',':
        lexer.tokens.push_back({COMMA});
        lexer.step();
        break;
      case '.':
        lexer.tokens.push_back({PERIOD});
        lexer.step();
        break;
      case '/':
        lex_slash(&lexer);
        break;
      case '(':
        lexer.tokens.push_back({LPAREN});
        lexer.step();
        break;
      case ')':
        lexer.tokens.push_back({RPAREN});
        lexer.step();
        break;
      case '{':
        lexer.tokens.push_back({LCBRACKET});
        lexer.step();
        break;
      case '>':
        lexer.tokens.push_back({GThan});
        lexer.step();
        break;
      case '<':
        lexer.tokens.push_back({LThan});
        lexer.step();
        break;
      case '}':
        lexer.tokens.push_back({RCBRACKET});
        lexer.step();
        break;
      case '[':
        lexer.tokens.push_back({LHBRACKET});
        lexer.step();
        break;
      case ']':
        lexer.tokens.push_back({RHBRACKET});
        lexer.step();
        break;
      case ':':
        lexer.tokens.push_back({COLON});
        lexer.step();
        break;
      case ';':
        lexer.tokens.push_back({SEMICOLON});
        lexer.step();
        break;
      case '"':
        lex_string(&lexer);
        break;
      case '=':
        lex_equal(&lexer);
        break;
      case '\n':
        lexer.step();
        break;
      case ' ':
        lexer.step();
        break;
      default:
        if (is_int(current_char)) {
          lex_number(&lexer);
        } else if (is_letter(current_char)) {
          lex_identifier(&lexer);
        } else {
          lexer.step();
        }
        break;
    };
  }

  return lexer.tokens;
}

void lex_identifier(Lexer *lexer) {
  if (!is_letter(lexer->read_current_char())) {
    throw std::runtime_error("expression: " + lexer->read_current_char());
  }

  Token identifier_token = {IDENTIFIER, ""};

  while (is_int_or_letter(lexer->read_current_char())) {
    identifier_token.content += lexer->read_current_char();
    lexer->step();
  }

  // Handle language constructs here
  if (identifier_token.content == "if") {
    identifier_token = {IF, ""};
  }
  if (identifier_token.content == "else") {
    identifier_token = {ELSE, ""};
  }
  if (identifier_token.content == "int") {
    identifier_token = {INT, ""};
  }
  if (identifier_token.content == "float") {
    identifier_token = {FLOAT, ""};
  }
  if (identifier_token.content == "struct") {
    identifier_token = {STRUCT, ""};
  }
  if (identifier_token.content == "return") {
    identifier_token = {RETURN, ""};
  }
  if (identifier_token.content == "void") {
    identifier_token = {VOID, ""};
  }
  if (identifier_token.content == "true") {
    identifier_token = {TRUE, ""};
  }
  if (identifier_token.content == "false") {
    identifier_token = {FALSE, ""};
  }
  if (identifier_token.content == "bool") {
    identifier_token = {BOOL, ""};
  }
  lexer->tokens.push_back(identifier_token);
}

void lex_number(Lexer *lexer) {
  Token number_token = {INT_DATA, ""};
  while (is_int(lexer->read_current_char()) ||
         lexer->read_current_char() == '.') {
    if (lexer->read_current_char() == '.') {
      if (number_token.type == FLOAT_DATA) {
        throw std::runtime_error("Too many decimals in float");
        return;
      }
      number_token.type = FLOAT_DATA;
    }
    number_token.content += lexer->read_current_char();
    lexer->step();
  }
  lexer->tokens.push_back(number_token);
}

bool is_int(char ch) { return (ch >= 48 && ch <= 57); }

bool is_letter(char ch) {
  return (ch >= 65 && ch <= 90) || (ch >= 97 && ch <= 122);
}

bool is_int_or_letter(char ch) { return is_int(ch) || is_letter(ch); }

void lex_slash(Lexer *lexer) {
  Token token = {DIVIDE, ""};
  lexer->step();
  if (lexer->read_current_char() == '/') {
    lexer->step();
    token.type = COMMENT;
    while (lexer->read_current_char() != '\n') {
      token.content += lexer->read_current_char();
      lexer->step();
    }
  }
  lexer->tokens.push_back(token);
}

// lex the equal sign
void lex_equal(Lexer *lexer) {
  Token token = {EQUALS, ""};
  lexer->step();
  if (lexer->read_current_char() == '=') {
    token.type = DOUBLEEQUALS;
    lexer->step();
  }
  lexer->tokens.push_back(token);
}

// read a string token
void lex_string(Lexer *lexer) {
  Token string_token = {STRING, ""};
  // step
  lexer->step();
  while (lexer->read_current_char() != '"') {
    string_token.content += lexer->read_current_char();
    lexer->step();
  }
  lexer->step();
  lexer->tokens.push_back(string_token);
}
}  // namespace lexer
