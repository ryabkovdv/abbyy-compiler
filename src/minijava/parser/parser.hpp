#ifndef MINIJAVA_PARSER_PARSER_HPP
#define MINIJAVA_PARSER_PARSER_HPP

#include <minijava/ast/ast.hpp>

#include <minijava/diagnostic.hpp>

namespace minijava {

std::error_code
parse(std::string_view content, DiagnosticEngine& diag, AstTree* tree);

std::error_code
print_tokens(std::string_view content, DiagnosticEngine& diag);

} // namespace minijava

#endif
