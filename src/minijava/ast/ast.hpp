#ifndef MINIJAVA_AST_AST_HPP
#define MINIJAVA_AST_AST_HPP

#include <ast_decl.hpp>

namespace minijava {
namespace detail {

inline constexpr char BoolSymbolString[] = "boolean";
inline constexpr char IntSymbolString[] = "int";
inline constexpr char IntArraySymbolString[] = "int[]";

} // namespace detail

inline constexpr auto BoolSymbol = Symbol(detail::BoolSymbolString);
inline constexpr auto IntSymbol = Symbol(detail::IntSymbolString);
inline constexpr auto IntArraySymbol = Symbol(detail::IntArraySymbolString);

} // namespace minijava

#endif
