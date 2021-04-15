#ifndef MINIJAVA_PARSER_PARSER_HPP
#define MINIJAVA_PARSER_PARSER_HPP

#include <minijava/ast/ast.hpp>

#include <minijava/diagnostic.hpp>

namespace minijava {

std::error_code
parse(std::string_view content, DiagnosticEngine& diag, AstTree* tree);

} // namespace minijava

#endif
