#ifndef MINIJAVA_PARSER_YYEXTRA_HPP
#define MINIJAVA_PARSER_YYEXTRA_HPP

#include <minijava/utility.hpp>

#include <unordered_set>

namespace minijava::detail {

struct SymtabHasher {
    size_t operator()(Symbol symbol) const noexcept
    {
        return std::hash<std::string_view>{}(symbol.to_view());
    }
};

struct SymtabEqual {
    bool operator()(Symbol lhs, Symbol rhs) const noexcept
    {
        return lhs.to_view() == rhs.to_view();
    }
};

struct yy_extra_type {
    std::unordered_set<Symbol, SymtabHasher, SymtabEqual> symtab;
    BumpAllocator* pool;
};

} // namespace minijava::detail

#endif
