#include <fstream>
#include <streambuf>
#include "lexer.h"
#include "parser.h"
#include <vector>
#include <iostream>
#include <string>
void run_tests();

int main(int argc, char *argv[]){
	std::ifstream t(argv[1]);
	std::string str((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

	std::vector<Token> tokens = tokenize(str);
	for(Token t: tokens){
		std::cout << t << std::endl;
	}

	run_tests();
	return 0;
}

void run_tests() {
	std::cout << std::endl << "Running Tests: " << std::endl << std::endl;
	parser_testing::test_variable_decleration_no_initilization();
	parser_testing::test_struct_decleration();
}
