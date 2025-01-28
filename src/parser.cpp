#include "parser.h"

#include <cmath>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

#include "lexer.h"
#include "utils.h"

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
  std::vector<Statement> program_statements = parse_statements(&parser);
  parser.program.insert(parser.program.end(), program_statements.begin(),
                        program_statements.end());
  return parser.program;
}

std::vector<Statement> parse_statements(Parser *parser) {
  std::vector<Statement> statements = {};
  while (!parser->ended() && parser->peek().type != lexer::RCBRACKET) {
    Statement statement = parse_statement(parser);
    statements.push_back(statement);
  }
  return statements;
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
      return parse_function_statement(parser, type, identifier);
    default:
      throw std::runtime_error("Unexpected token");
  }
}

Statement parse_function_statement(Parser *parser, Type return_type,
                                   lexer::Token identifier) {
  Parameters params = parse_function_parameters(parser);
  parser->expect(lexer::LCBRACKET);
  std::vector<Statement> statements = parse_statements(parser);
  parser->expect(lexer::RCBRACKET);
  return parser::Statement{
      parser::StatementType::FUNCTION,
      new parser::FunctionStatement{identifier.content, return_type, params,
                                    statements}};
}

Parameters parse_function_parameters(Parser *parser) {
  parser->expect(lexer::LPAREN);
  Parameters params = Parameters{0, {}, {}};
  while (parser->peek().type != lexer::RPAREN) {
    params.num += 1;
    params.types.push_back(parse_type(parser));
    params.vars.push_back(parser->expect(lexer::IDENTIFIER).content);
    if (parser->peek().type == lexer::COMMA) {
      parser->consume();
    }
  }
  parser->expect(lexer::RPAREN);
  return params;
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
    parser->expect(lexer::SEMICOLON);
    return stmt;
  }
  if (parser->peek().type == lexer::SEMICOLON) {
    VariableDeclerationStatement *var =
        new VariableDeclerationStatement{type, identifier, nullptr};
    Statement stmt = {VARIABLE_DECLERATION, var};
    parser->expect(lexer::SEMICOLON);
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

  while (true) {
    if (!utils::is_operator_token(parser->peek().type)) {
      break;
    }
    Precedence next_prec = utils::infix_operator_to_precendence(
        utils::token_to_infix_operator(parser->peek().type));
    if (next_prec < precedence) {
      break;
    }
    // Switchs operator token operations
    switch (parser->peek().type) {
        // Anything being added here should also be added to is_operator_token
      case lexer::PLUS:
      case lexer::MINUS:
      case lexer::MULTIPLY:
      case lexer::DIVIDE:
        expr = parse_binary_expression(parser, expr, precedence);
        break;
      default:
        throw std::runtime_error("Unexpected binary operator");
    }
  }
  return expr;
}

/*
 * Parse a binary expression such as addition or multiplication, handles the
 * appropriate opperator precedence
 */
Expression *parse_binary_expression(Parser *parser, Expression *left,
                                    Precedence precedence) {
  InfixOperator op = utils::token_to_infix_operator(parser->consume().type);
  Precedence prec = utils::infix_operator_to_precendence(op);
  return new Expression{
      BinaryOperatorExpressionType,
      new BinaryOperatorExpression{op, left, parse_expression(parser, prec)}};
}

/*
 * Parse Prefix Operators
 */
Expression *parse_prefix(Parser *parser) {
  lexer::Token prefix = parser->consume();
  Expression *expr = parse_expression(parser, Precedence::Lowest);
  return new Expression{
      PrefixExpressionType,
      new PrefixExpression{utils::token_to_prefix(prefix), expr}};
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

/*
 * Parses a function call expression
 */
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
  parser->expect(lexer::SEMICOLON);
  return stmt;
}
}  // namespace parser
