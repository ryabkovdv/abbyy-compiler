#ifndef MINIJAVA_AST_AST_HPP
#define MINIJAVA_AST_AST_HPP

#include <ast_decl.hpp>

namespace minijava {

struct Parameter {
    std::string_view name;
    std::string_view type;

    Parameter(std::string_view name, std::string_view type)
        : name(name), type(type)
    {}
};

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

    explicit MethodDecl(Access access, std::string_view name,
                        std::string_view type, Span<Parameter> parameters,
                        Span<const Stmt*> body)
        : ClassRecord{ClassRecord::MethodDecl}, access(access), name(name),
          type(type), parameters(parameters), body(body)
    {}

    static bool classof(const ClassRecord* record)
    {
        return record->kind == ClassRecord::MethodDecl;
    }
};

struct IntExpr : Expr {
    std::string_view value;

    explicit IntExpr(std::string_view value) : Expr{Stmt::IntExpr}, value(value)
    {}
};

struct BoolExpr : Expr {
    bool value;

    static const BoolExpr TrueValue;
    static const BoolExpr FalseValue;
};

struct ThisExpr : Expr {
    static const ThisExpr Value;
};

inline const ThisExpr ThisExpr::Value = ThisExpr{Stmt::ThisExpr};
inline const BoolExpr BoolExpr::TrueValue = BoolExpr{Stmt::BoolExpr, true};
inline const BoolExpr BoolExpr::FalseValue = BoolExpr{Stmt::BoolExpr, false};

struct AstTree {
    Span<const ClassDecl*> classes;
    Span<const Stmt*> main;

    BumpAllocator pool;
    bool valid = true;
};

} // namespace minijava

#endif
