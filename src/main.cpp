#include <fstream>
#include <streambuf>
#include "lexer.h"
#include <vector>
#include <iostream>
#include <string>

int main(int argc, char *argv[]){
	std::ifstream t(argv[1]);
	std::string str((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

	std::vector<Token> tokens = tokenize(str);
	for(Token t: tokens){
		std::cout << t << std::endl;
	}
	return 0;
}


