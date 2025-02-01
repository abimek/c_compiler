#include <execution>
#include <fstream>
#include <iostream>
#include <streambuf>
#include <string>
#include <vector>

#include "lexer.h"
#include "parser.h"
#include "parser_test.h"

using namespace lexer;

void run_tests();

int main(int argc, char *argv[]) {
  std::ifstream t(argv[1]);
  std::string str((std::istreambuf_iterator<char>(t)),
                  std::istreambuf_iterator<char>());

  std::vector<Token> tokens = tokenize(str);
  for (Token t : tokens) {
    std::cout << t << std::endl;
  }
  run_tests();
  return 0;
}

void run_tests() {
  std::cout << std::endl << "Running Tests: " << std::endl << std::endl;
  parser_testing::test_should_fail();
  parser_testing::test_prefix();
  parser_testing::test_struct_decleration();
  parser_testing::test_variable_decleration_no_initilization();
  parser_testing::test_literal();
  parser_testing::test_add();
  parser_testing::test_operator_precedence();
  parser_testing::test_function_call();
  parser_testing::test_function_statement();
}
