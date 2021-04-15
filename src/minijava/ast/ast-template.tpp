#ifndef MINIJAVA_AST_DECL_HPP
#define MINIJAVA_AST_DECL_HPP

#include <minijava/utility.hpp>

namespace minijava {

$declarations
$definitions

struct MethodDecl : ClassRecord {
    enum Access : int {
        Public,
        Private,
    };

    Access access;
    Symbol name;
    Symbol ret_type;
    Span<Parameter> params;
    Span<Stmt const* const> body;

    explicit constexpr MethodDecl(Access access, Symbol name, Symbol ret_type,
                                  Span<Parameter> params,
                                  Span<Stmt const* const> body)
        : ClassRecord{ClassRecord::MethodDecl}, access(access), name(name),
          ret_type(ret_type), params(params), body(body)
    {}
};

struct BoolLiteral : Expr {
    static const BoolLiteral True;
    static const BoolLiteral False;

    bool value;

    explicit constexpr BoolLiteral(bool value)
        : Expr{Stmt::BoolLiteral}, value(value)
    {}
};

inline const BoolLiteral BoolLiteral::True{true};
inline const BoolLiteral BoolLiteral::False{false};

struct NullExpr : Expr {
    static const NullExpr Instance;

    constexpr NullExpr() : Expr{Stmt::NullExpr}
    {}
};

inline const NullExpr NullExpr::Instance{};

struct ThisExpr : Expr {
    static const ThisExpr Instance;

    constexpr ThisExpr() : Expr{Stmt::ThisExpr}
    {}
};

inline const ThisExpr ThisExpr::Instance{};

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
