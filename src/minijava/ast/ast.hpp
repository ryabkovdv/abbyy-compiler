#ifndef MINIJAVA_AST_AST_HPP
#define MINIJAVA_AST_AST_HPP

#include <ast_decl.hpp>

namespace minijava {

struct MethodDecl : ClassRecord {
    enum Access : int {
        Public,
        Private,
    };

    Access access;
    std::string_view name;
    std::string_view type;
    Span<Parameter> parameters;
    Span<const Stmt*> body;

    explicit constexpr MethodDecl(Access access, std::string_view name,
                                  std::string_view type,
                                  Span<Parameter> parameters,
                                  Span<const Stmt*> body)
        : ClassRecord{ClassRecord::MethodDecl}, access(access), name(name),
          type(type), parameters(parameters), body(body)
    {}

    static constexpr bool classof(const ClassRecord& record)
    {
        return record.kind == ClassRecord::MethodDecl;
    }
};

struct ThisExpr : Expr {
    static const ThisExpr Instance;

    constexpr ThisExpr() : Expr{Stmt::ThisExpr}
    {}

    static constexpr bool classof(const Stmt& record)
    {
        return record.kind == Stmt::ThisExpr;
    }
};

inline const ThisExpr ThisExpr::Instance{};

struct BoolLiteral : Expr {
    static const BoolLiteral True;
    static const BoolLiteral False;

    bool value;

    explicit constexpr BoolLiteral(bool value)
        : Expr{Stmt::BoolLiteral}, value(value)
    {}

    static constexpr bool classof(const Stmt& record)
    {
        return record.kind == Stmt::BoolLiteral;
    }
};

inline const BoolLiteral BoolLiteral::True{true};
inline const BoolLiteral BoolLiteral::False{false};

struct AstTree {
    Span<const ClassDecl*> classes;
    const Stmt* main;

    BumpAllocator pool;
};

template <typename V, typename RetT = void, typename TreeRetT = void>
struct AstVisitor : detail::AstVisitorImpl<V, RetT> {
    using detail::AstVisitorImpl<V, RetT>::visit;

    constexpr TreeRetT visit(const AstTree* tree)
    {
        return visit(*tree);
    }

    constexpr TreeRetT visit(const AstTree& tree)
    {
        return static_cast<V*>(this)->visitTree(tree);
    }

    constexpr void visitTree(const AstTree& tree)
    {
        static_cast<V*>(this)->visitMain(*tree.main);
        visit(tree.classes);
    }

    constexpr void visitMain(const Stmt& main)
    {
        (void)visit(main);
    }
};

} // namespace minijava

#endif
