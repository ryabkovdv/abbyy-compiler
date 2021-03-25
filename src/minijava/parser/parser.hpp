#ifndef MINIJAVA_PARSER_PARSER_HPP
#define MINIJAVA_PARSER_PARSER_HPP

#include <minijava/ast/ast.hpp>

namespace minijava {

int parse(FILE* file, AstTree* tree);
int print_tokens(FILE* file);

} // namespace minijava

#endif
