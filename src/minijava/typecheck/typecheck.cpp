#include <minijava/typecheck/typecheck.hpp>

#include <unordered_map>

using namespace minijava;

static bool is_lvalue(const Expr* expr) noexcept
{
    if (isa<IdentExpr>(expr))
        return true;
    if (auto* subscript = dyn_cast<SubscriptExpr>(expr))
        return isa<IdentExpr>(subscript->array);
    return false;
}

static bool is_same(const Type* x, const Type* y) noexcept
{
    if (x == y)
        return true;
    if (x == nullptr || y == nullptr)
        return true;
    return false;
}

static bool is_convertible(const Type* from, const Type* to) noexcept
{
    if (from == to)
        return true;

    if (from == nullptr || to == nullptr)
        return true;

    if (auto* cl_to = dyn_cast<ClassType>(to)) {
        auto* cl_from = dyn_cast<ClassType>(from);
        while (cl_from != nullptr) {
            if (cl_from == cl_to)
                return true;
            cl_from = cl_from->base;
        }
        return from->kind == Type::Null;
    }
    return false;
}

namespace {

class TypeChecker : public AstVisitor<TypeChecker, const Type*> {
public:
    void visitTree(const AstTree& tree)
    {
        // create class types
        // verify class records
        // verify statements
    }

    const Type* visitClassDecl(const ClassDecl& node)
    {
        m_class_type = cast<ClassType>(lookup_type(node.name));
        assert(m_class_type != nullptr);

        push_scope();
        m_scope_end[-1] = m_class_type->m_vars;

        for (auto* record : node.records) {
            if (auto* method = dyn_cast<MethodDecl>(record))
                visitMethodDecl(*method);
        }

        pop_scope();
        return nullptr;
    }

    const Type* visitMethodDecl(const MethodDecl& node)
    {
        m_ret_type = lookup_type(node.ret_type);

        push_scope();
        visit(node.params);
        visit(node.body);
        pop_scope();

        return nullptr;
    }

    const Type* visitParameter(const Parameter& node)
    {
        auto* type = lookup_type(node.type);
        insert_variable(node.name, type);
        return nullptr;
    }

    const Type* visitVarDecl(const VarDecl& node)
    {
        auto* type = lookup_type(node.type);
        insert_variable(node.name, type);
        return nullptr;
    }

    const Type* visitCompoundStmt(const CompoundStmt& node)
    {
        push_scope();
        visit(node.body);
        pop_scope();
        return nullptr;
    }

    const Type* visitIfStmt(const IfStmt& node)
    {
        auto* cond_type = visit(node.cond);
        verify_same(cond_type, &BoolType);

        push_scope();
        visit(node.then_branch);
        pop_scope();

        push_scope();
        visit(node.else_branch);
        pop_scope();

        return nullptr;
    }

    const Type* visitWhileStmt(const WhileStmt& node)
    {
        auto* cond_type = visit(node.cond);
        verify_same(cond_type, &BoolType);

        push_scope();
        visit(node.body);
        pop_scope();

        return nullptr;
    }

    const Type* visitReturnStmt(const ReturnStmt& node)
    {
        auto* expr_type = visit(node.expr);
        verify_convertible(expr_type, m_ret_type);
        return nullptr;
    }

    const Type* visitPrintStmt(const PrintStmt& node)
    {
        visit(node.args);
        return nullptr;
    }

    const Type* visitAssignStmt(const AssignStmt& node)
    {
        auto* lhs_type = visit(node.lhs);
        auto* rhs_type = visit(node.rhs);

        if (!is_lvalue(node.lhs))
            m_diag->error("left hand side is not an lvalue");
        else
            verify_convertible(rhs_type, lhs_type);
        return nullptr;
    }

    const Type* visitBinaryExpr(const BinaryExpr& node)
    {
        auto* lhs_type = visit(node.lhs);
        auto* rhs_type = visit(node.rhs);

        if (isa<EqExpr>(node) || isa<NeExpr>(node)) {
            if (!is_convertible(lhs_type, rhs_type) &&
                !is_convertible(rhs_type, lhs_type)) {
                m_diag->error("cannot compare objects of types `{}' and `{}'",
                              *lhs_type, *rhs_type);
            }
            return &BoolType;
        }

        const Type* target_type;
        const Type* ret_type;

        switch (node.kind) {
        case BinaryExpr::MulExpr:
        case BinaryExpr::DivExpr:
        case BinaryExpr::RemExpr:
        case BinaryExpr::AddExpr:
        case BinaryExpr::SubExpr:
            target_type = &IntType;
            ret_type = &IntType;
            break;
        case BinaryExpr::LtExpr:
        case BinaryExpr::LeExpr:
        case BinaryExpr::GtExpr:
        case BinaryExpr::GeExpr:
            target_type = &IntType;
            ret_type = &BoolType;
            break;
        case BinaryExpr::AndExpr:
        case BinaryExpr::OrExpr:
            target_type = &BoolType;
            ret_type = &BoolType;
            break;
        default:
            unreachable("invalid binary operation");
        }

        if (is_same(lhs_type, target_type) && is_same(rhs_type, target_type))
            return ret_type;

        if (lhs_type == rhs_type) {
            m_diag->error(
                "invalid binary operator for type `{}': expected `{}'",
                *lhs_type, *target_type);
        } else {
            m_diag->error(
                "invalid binary operator for types `{}' and `{}': expected `{}'",
                *lhs_type, *rhs_type, *target_type);
        }
        return nullptr;
    }

    const Type* visitUnaryExpr(const UnaryExpr& node)
    {
        const Type* target_type;
        switch (node.kind) {
        case UnaryExpr::NotExpr:
            target_type = &BoolType;
            break;
        case UnaryExpr::NegExpr:
            target_type = &IntType;
            break;
        default:
            unreachable("invalid unary operation");
        }

        auto* expr_type = visit(node.expr);
        if (is_same(expr_type, target_type))
            return target_type;

        m_diag->error("invalid unary operator for type `{}': expected `{}'",
                      *expr_type, *target_type);
        return nullptr;
    }

    const Type* visitSubscriptExpr(const SubscriptExpr& node)
    {
        auto* array_type = visit(node.array);
        auto* index_type = visit(node.index);
        verify_same(array_type, &IntArrayType);
        verify_same(index_type, &IntType);
        return &IntType;
    }

    const Type* visitCallExpr(const CallExpr& node)
    {
        auto* object_type = visit(node.object);
        if (object_type == nullptr)
            return nullptr;

        auto* signature = lookup_method(object_type, node.name);
        if (signature == nullptr)
            return nullptr;

        auto args = node.args;
        auto params = signature->params;
        if (args.size() != params.size()) {
            m_diag->error("argument count mismatch: expected {}, got {}",
                          params.size(), args.size());
        } else {
            for (size_t i = 0; i < args.size(); ++i) {
                auto* param_type = params[i];
                auto* arg_type = visit(args[i]);
                verify_convertible(arg_type, param_type);
            }
        }
        return signature->ret_type;
    }

    const Type* visitLengthExpr(const LengthExpr& node)
    {
        auto* expr_type = visit(node.expr);
        verify_same(expr_type, &IntArrayType);
        return &IntType;
    }

    const Type* visitNewIntArrayExpr(const NewIntArrayExpr& node)
    {
        auto* count_type = visit(node.count);
        verify_same(count_type, &IntType);
        return &IntArrayType;
    }

    const Type* visitNewObjectExpr(const NewObjectExpr& node)
    {
        return lookup_type(node.name);
    }

    const Type* visitIdentExpr(const IdentExpr& node)
    {
        return lookup_variable(node.name);
    }

    const Type* visitIntLiteral(const IntLiteral&)
    {
        return &IntType;
    }

    const Type* visitBoolLiteral(const BoolLiteral&)
    {
        return &BoolType;
    }

    const Type* visitNullExpr(const NullExpr&)
    {
        return &NullType;
    }

    const Type* visitThisExpr(const ThisExpr&)
    {
        if (m_class_type == nullptr)
            m_diag->error("cannot use `this' inside of static method");
        return m_class_type;
    }

private:
    const Type* m_ret_type = nullptr;
    const ClassType* m_class_type = nullptr;
    DiagnosticEngine* m_diag;

    std::unordered_map<Symbol, const Type*> m_types;

    std::vector<std::unordered_map<Symbol, const Type*>> m_scopes;
    decltype(m_scopes)::iterator m_scope_end;

    const MethodSig* lookup_method(const Type*, Symbol);

    void push_scope()
    {
        if (m_scope_end == m_scopes.end()) {
            m_scopes.emplace_back();
            m_scope_end = m_scopes.end();
        } else {
            ++m_scope_end;
        }
    }

    void pop_scope()
    {
        --m_scope_end;
        m_scope_end->clear();
    }

    void insert_variable(Symbol symbol, const Type* type)
    {
        auto scope = m_scope_end - 1;
        auto [it, ok] = scope->emplace(symbol, type);
        if (!ok)
            m_diag->error("redeclaration of variable `{}'", symbol);
    }

    const Type* lookup_variable(Symbol symbol)
    {
        auto outer_scope = m_scopes.begin();
        auto scope_it = m_scope_end;
        while (true) {
            --m_scope_end;
            auto it = scope_it->find(symbol);
            if (it != scope_it->end())
                return it->second;

            if (scope_it == outer_scope) {
                m_diag->error("variable `{}' not found in this scope", symbol);
                return nullptr;
            }
        }
    }

    void insert_type(Symbol symbol, const Type* type)
    {
        auto [it, ok] = m_types.emplace(symbol, type);
        if (!ok)
            m_diag->error("redeclaration of type `{}'", symbol);
    }

    const Type* lookup_type(Symbol symbol)
    {
        auto it = m_types.find(symbol);
        if (it != m_types.end())
            return it->second;

        m_diag->error("type `{}' not found", symbol);
        return nullptr;
    }

    void verify_same(const Type* x, const Type* y)
    {
        if (!is_same(x, y))
            m_diag->error("mismatched types: expected `{}', got `{}'", *y, *x);
    }

    void verify_convertible(const Type* from, const Type* to)
    {
        if (!is_convertible(from, to))
            m_diag->error("cannot convert type `{}' to `{}'", *from, *to);
    }
};

} // namespace
