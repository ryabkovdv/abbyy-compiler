#include <minijava/ast/graphviz.hpp>

#include <fmt/format.h>

using namespace minijava;

namespace {

struct AstPrinter : AstVisitor<AstPrinter, int> {
private:
    int m_id = 0;

    template <typename R>
    void print_list(const R& range, int root_id, std::string_view port)
    {
        bool first = true;
        for (const auto& decl : range) {
            int id = visit(decl);
            if (std::exchange(first, false))
                fmt::print(R"({}:"{}"->{};)", root_id, port, id);
            else
                fmt::print(R"({}->{}[style=dashed,color=dimgray];)", root_id,
                           id);
            root_id = id;
        }
    }

    static void link(int from, int to, std::string_view port)
    {
        fmt::print(R"({}:"{}"->{};)", from, port, to);
    }

public:
    void visitTree(const AstTree& tree)
    {
        fmt::print(R"(digraph G {{node [shape=record];)");

        int root_id = m_id++;
        fmt::print(
            R"({}[label="{{Root|{{<main> main| <classes> classes}}}}"];)",
            root_id);

        int main_id = visit(tree.main);
        link(root_id, main_id, "main");
        print_list(tree.classes, root_id, "classes");

        fmt::print("}}\n");
    }

    int visitClassDecl(const ClassDecl& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{ClassDecl|name: {})", id, node.name);
        if (!node.base.empty())
            fmt::print(R"(|base: {})", node.base);
        fmt::print(R"(|<records> records}}"];)");

        print_list(node.records, id, "records");

        return id;
    }

    int visitClassVarDecl(const ClassVarDecl& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{ClassVarDecl|name: {}|type: {}}}"];)", id,
                   node.name, node.type);
        return id;
    }

    int visitMethodDecl(const MethodDecl& node)
    {
        int id = m_id++;
        std::string_view access =
            (node.access == MethodDecl::Public) ? "public" : "private";
        fmt::print(R"({}[label="{{MethodDecl|name: {}|type: {}|access: {}|)"
                   R"({{<params> params|<body> body}}}}"];)",
                   id, node.name, node.type, access);

        print_list(node.parameters, id, "params");
        print_list(node.body, id, "body");

        return id;
    }

    int visitParameter(const Parameter& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{name: {}|type: {}}}"];)", id, node.name,
                   node.type);
        return id;
    }

    int visitVarDecl(const VarDecl& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{VarDecl|name: {}|type: {}}}"];)", id,
                   node.name, node.type);
        return id;
    }

    int visitCompoundStmt(const CompoundStmt& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="<body> CompoundStmt"];)", id);

        print_list(node.body, id, "body");

        return id;
    }

    int visitIfStmt(const IfStmt& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{IfStmt|)"
                   R"({{<cond> cond|<then> then|<else> else}}}}"];)",
                   id);

        int cond_id = visit(node.condition);
        int then_id = visit(node.if_branch);
        int else_id = visit(node.else_branch);
        link(id, cond_id, "cond");
        link(id, then_id, "then");
        link(id, else_id, "else");

        return id;
    }

    int visitWhileStmt(const WhileStmt& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{WhileStmt|{{<cond> cond|<body> body}}}}"];)",
                   id);

        int cond_id = visit(node.condition);
        int body_id = visit(node.body);
        link(id, cond_id, "cond");
        link(id, body_id, "body");

        return id;
    }

    int visitReturnStmt(const ReturnStmt& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{ReturnStmt|<expr> expr}}"];)", id);

        int expr_id = visit(node.expr);
        link(id, expr_id, "expr");

        return id;
    }

    int visitPrintStmt(const PrintStmt& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{PrintStmt|<args> args}}"];)", id);
        print_list(node.arguments, id, "args");
        return id;
    }

    int visitAssignStmt(const AssignStmt& node)
    {
        int id = m_id++;
        fmt::print(
            R"({}[label="{{AssignStmt|{{<target> target|<value> value}}}}"];)",
            id);

        int target_id = visit(node.target);
        int value_id = visit(node.value);
        link(id, target_id, "target");
        link(id, value_id, "value");

        return id;
    }

    int visitBinaryExpr(const BinaryExpr& node)
    {
        std::string_view name;
        switch (node.kind) {
        case Expr::MulExpr: name = "MulExpr"; break;
        case Expr::DivExpr: name = "DivExpr"; break;
        case Expr::RemExpr: name = "RemExpr"; break;
        case Expr::AddExpr: name = "AddExpr"; break;
        case Expr::SubExpr: name = "SubExpr"; break;
        case Expr::EqExpr:  name = "EqExpr";  break;
        case Expr::NeExpr:  name = "NeExpr";  break;
        case Expr::LtExpr:  name = "LtExpr";  break;
        case Expr::LeExpr:  name = "LeExpr";  break;
        case Expr::GtExpr:  name = "GtExpr";  break;
        case Expr::GeExpr:  name = "GeExpr";  break;
        case Expr::AndExpr: name = "AndExpr"; break;
        case Expr::OrExpr:  name = "OrExpr";  break;
        default: unreachable("invalid binary operation");
        }

        int id = m_id++;
        fmt::print(R"({}[label="{{{}|{{<lhs> lhs|<rhs> rhs}}}}"];)", id, name);

        int lhs_id = visit(node.lhs);
        int rhs_id = visit(node.rhs);
        link(id, lhs_id, "lhs");
        link(id, rhs_id, "rhs");

        return id;
    }

    int visitNotExpr(const NotExpr& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{NotExpr|<expr> expr}}"];)", id);

        int expr_id = visit(node.expr);
        link(id, expr_id, "expr");

        return id;
    }

    int visitSubscriptExpr(const SubscriptExpr& node)
    {
        int id = m_id++;
        fmt::print(
            R"({}[label="{{SubscriptExpr|{{<array> array|<index> index}}}}"];)",
            id);

        int array_id = visit(node.array);
        int index_id = visit(node.index);
        link(id, array_id, "array");
        link(id, index_id, "index");

        return id;
    }

    int visitCallExpr(const CallExpr& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{CallExpr|name: {}|)"
                   R"({{<object> object|<args> args}}}}"];)",
                   id, node.name);

        int object_id = visit(node.object);
        link(id, object_id, "object");
        print_list(node.arguments, id, "args");

        return id;
    }

    int visitLengthExpr(const LengthExpr& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{LengthExpr|<expr> expr}}"];)", id);

        int expr_id = visit(node.expr);
        link(id, expr_id, "expr");

        return id;
    }

    int visitNewIntArrayExpr(const NewIntArrayExpr& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{NewIntArrayExpr|<count> count}}"];)", id);

        int count_id = visit(node.count);
        link(id, count_id, "count");

        return id;
    }

    int visitNewObjectExpr(const NewObjectExpr& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{NewObjectExpr|name: {}}}"];)", id, node.name);
        return id;
    }

    int visitIdentExpr(const IdentExpr& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{IdentExpr|name: {}}}"];)", id, node.name);
        return id;
    }

    int visitIntLiteral(const IntLiteral& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{IntLiteral|value: {}}}"];)", id, node.value);
        return id;
    }

    int visitThisExpr(const ThisExpr&)
    {
        int id = m_id++;
        fmt::print(R"({}[label="ThisExpr"];)", id);
        return id;
    }

    int visitBoolLiteral(const BoolLiteral& node)
    {
        int id = m_id++;
        fmt::print(R"({}[label="{{BoolLiteral|value: {}}}"];)", id, node.value);
        return id;
    }
};

} // namespace

void minijava::print_ast(const AstTree& tree)
{
    AstPrinter{}.visit(tree);
}
