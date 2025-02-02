#include "utils.h"

#include "lexer.h"
#include "parser.h"

namespace utils {

bool bool_literal_token_to_bool(lexer::TokenType token_type) {
  switch (token_type) {
    case lexer::TRUE:
      return true;
    case lexer::FALSE:
      return false;
    default:
      throw std::runtime_error("unexpected boolean type in bool token to bool");
  }
}

bool is_operator_token(lexer::TokenType token_type) {
  switch (token_type) {
    case lexer::PLUS:
    case lexer::MINUS:
    case lexer::MULTIPLY:
    case lexer::DIVIDE:
    case lexer::GThan:
    case lexer::LThan:
    case lexer::DOUBLEEQUALS:
      return true;
    default:
      return false;
  }
}
/*
 * Utility function that turns a token to an ifnix operators
 * TODO: Move to a utilities file later
 */
parser::InfixOperator token_to_infix_operator(lexer::TokenType token_t) {
  switch (token_t) {
    case lexer::PLUS:
      return parser::InfixOperator::ADDITION;
    case lexer::MINUS:
      return parser::InfixOperator::SUBTRACTION;
    case lexer::MULTIPLY:
      return parser::InfixOperator::MULTIPLICATION;
    case lexer::DIVIDE:
      return parser::InfixOperator::DIVISION;
    case lexer::GThan:
      return parser::InfixOperator::GREATER_THAN;
    case lexer::LThan:
      return parser::InfixOperator::LESS_THAN;
    case lexer::DOUBLEEQUALS:
      return parser::InfixOperator::EQUAL;
    default:
      throw std::runtime_error("Unepxect infix opeartor");
  }
}

/*
 * Turns and InfixOperator to it's Precedence
 * TODO: Move to a utitlies file later
 */
parser::Precedence infix_operator_to_precendence(parser::InfixOperator op) {
  switch (op) {
    case parser::InfixOperator::ADDITION:
    case parser::InfixOperator::SUBTRACTION:
      return parser::Precedence::Term;
    case parser::InfixOperator::MULTIPLICATION:
    case parser::InfixOperator::DIVISION:
      return parser::Precedence::Factor;
    case parser::InfixOperator::GREATER_THAN:
    case parser::InfixOperator::LESS_THAN:
    case parser::InfixOperator::EQUAL:
      return parser::Precedence::Compare;
    default:
      throw std::runtime_error(
          "Unexpected Infix Operator To Precedance Conversion");
  }
}

parser::PrefixOp token_to_prefix(lexer::Token token) {
  switch (token.type) {
    case lexer::MINUS:
      return parser::PrefixOp::MINUS;
    default:
      throw std::runtime_error("Unexpected prefix: " +
                               lexer::token_type_to_string(token.type));
  }
}

}  // namespace utils
