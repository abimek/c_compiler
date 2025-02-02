#pragma once
#include "lexer.h"
#include "parser.h"

namespace utils {
bool is_operator_token(lexer::TokenType token_type);
parser::PrefixOp token_to_prefix(lexer::Token token);
parser::Precedence infix_operator_to_precendence(parser::InfixOperator op);
parser::InfixOperator token_to_infix_operator(lexer::TokenType token_t);
bool bool_literal_token_to_bool(lexer::TokenType token_type);
}  // namespace utils
