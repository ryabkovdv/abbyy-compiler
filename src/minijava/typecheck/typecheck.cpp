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

namespace {

struct Signature {
    const Type* ret_type;
    Span<const Type* const> parameters;
};

class TypeChecker : public AstVisitor<TypeChecker, const Type*> {
public:
    void visitTree(const AstTree& tree)
    {
        // create class types
        // verify class records
        // verify statements
    }

    const Type* visitVarDecl(const VarDecl& node)
    {
        auto* type = lookup_type(node.type);
        return nullptr;
    }

    const Type* visitCompoundStmt(const CompoundStmt& node)
    {
        visit(node.body);
        return nullptr;
    }

    const Type* visitIfStmt(const IfStmt& node)
    {
        auto* cond_type = visit(node.condition);
        verify_same(cond_type, &BoolType);

        visit(node.if_branch);
        visit(node.else_branch);
        return nullptr;
    }

    const Type* visitWhileStmt(const WhileStmt& node)
    {
        auto* cond_type = visit(node.condition);
        verify_same(cond_type, &BoolType);

        visit(node.body);
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
        visit(node.arguments);
        return nullptr;
    }

    const Type* visitAssignStmt(const AssignStmt& node)
    {
        auto* lhs_type = visit(node.target);
        auto* rhs_type = visit(node.value);

        if (is_lvalue(node.target)) {
            verify_convertible(rhs_type, lhs_type);
        } else {
            // emit error
        }
        return nullptr;
    }

    const Type* visitBinaryExpr(const BinaryExpr& node)
    {
        auto* lhs_type = visit(node.lhs);
        auto* rhs_type = visit(node.rhs);

        const Type* target_type = [&] {
            switch (node.kind) {
            case BinaryExpr::AndExpr:
            case BinaryExpr::OrExpr:
                return &BoolType;
            default:
                return &IntType;
            }
        }();

        verify_same(lhs_type, target_type);
        verify_same(rhs_type, target_type);
        return target_type;
    }

    const Type* visitNotExpr(const NotExpr& node)
    {
        auto* expr_type = visit(node.expr);
        verify_same(expr_type, &BoolType);
        return &BoolType;
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
        if (object_type == nullptr) {
        }

        auto* signature = lookup_method(object_type, node.name);
        if (signature == nullptr) {
        }

        auto args = node.arguments;
        auto params = signature->parameters;
        if (args.size() != params.size()) {
        }

        for (size_t i = 0; i < args.size(); ++i) {
            auto* param_type = params[i];
            auto* arg_type = visit(args[i]);
            verify_convertible(arg_type, param_type);
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

    const Type* visitThisExpr(const ThisExpr&)
    {
        if (m_class == nullptr) {
            // not in class scope (in main)
        }
        return m_class;
    }

    const Type* visitIntLiteral(const IntLiteral&)
    {
        return &IntType;
    }

    const Type* visitBoolLiteral(const BoolLiteral&)
    {
        return &BoolType;
    }

private:
    const Type* m_ret_type = &VoidType;
    const ClassType* m_class = nullptr;
    DiagnosticEngine* m_diag;

    std::unordered_map<std::string_view, 

    const Type* lookup_type(std::string_view);
    const Type* lookup_variable(std::string_view);
    const Signature* lookup_method(const Type*, std::string_view);

    static void verify_same(const Type* x, const Type* y) noexcept
    {
        if (x == y)
            return;

        if (x == nullptr || y == nullptr)
            return;

        // not equal
    }

    static void verify_convertible(const Type* from, const Type* to) noexcept
    {
        if (from == to)
            return;

        if (from == nullptr || to == nullptr)
            return;

        if (auto* cl_to = dyn_cast<ClassType>(to)) {
            auto* cl_from = dyn_cast<ClassType>(from);
            while (cl_from != nullptr) {
                if (cl_from == cl_to)
                    return;
                cl_from = cl_from->base;
            }
        }

        // not convertible
    }
};

} // namespace
