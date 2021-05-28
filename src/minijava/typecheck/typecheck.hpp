#ifndef MINIJAVA_TYPECHECK_TYPECHECK_HPP
#define MINIJAVA_TYPECHECK_TYPECHECK_HPP

#include <minijava/ast/ast.hpp>
#include <minijava/ir/ir.hpp>

#include <minijava/diagnostic.hpp>

namespace minijava {

void typecheck(AstTree& tree, DiagnosticEngine& diag, IRBuilder* builder);

} // namespace minijava

#endif
