#ifndef MINIJAVA_AST_DECL_HPP
#define MINIJAVA_AST_DECL_HPP

#include <minijava/utility.hpp>
#include <minijava/diagnostic.hpp>

namespace minijava {

enum struct MethodAccess {
    Public,
    Private,
};

$declarations
$definitions

struct AstTree {
    Span<ClassDecl const* const> classes;
    const Stmt* main;

    BumpAllocator pool;
};

template <typename V, typename RetT = void, typename TreeRetT = void>
class AstVisitor {
public:
    template <typename R>
    constexpr auto visit(const R& range)
        -> std::void_t<decltype(std::begin(range)), decltype(std::end(range))>
    {
        for (const auto& node : range)
            (void)visit(node);
    }

    constexpr TreeRetT visit(const AstTree* tree)
    {
        return visit(*tree);
    }

    constexpr TreeRetT visit(const AstTree& tree)
    {
        return static_cast<V*>(this)->visitTree(tree);
    }

    constexpr void visitTree(const AstTree& tree)
    {}

$visitors
};

} // namespace minijava

#endif
