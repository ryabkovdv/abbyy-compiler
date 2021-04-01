#ifndef MINIJAVA_PARSER_PARSER_HPP
#define MINIJAVA_PARSER_PARSER_HPP

#include <minijava/ast/ast.hpp>

namespace minijava {

std::error_code parse(FILE* file, AstTree* tree);
std::error_code print_tokens(FILE* file);

} // namespace minijava

#endif
