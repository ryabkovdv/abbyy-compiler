#include <minijava/ir/graphviz.hpp>

#include <fmt/format.h>

using namespace minijava;

template <>
struct fmt::formatter<IRType> : fmt::formatter<std::string_view> {
    template <typename FormatContext>
    auto format(IRType type, FormatContext& ctx)
    {
        std::string_view name;
        switch (type) {
        case IRType::None: name = "None"; break;
        case IRType::i1:   name = "i1";   break;
        case IRType::i8:   name = "i8";   break;
        case IRType::i16:  name = "i16";  break;
        case IRType::i32:  name = "i32";  break;
        case IRType::i64:  name = "i64";  break;
        }
        return fmt::formatter<std::string_view>::format(name, ctx);
    }
};

template <>
struct fmt::formatter<IRICmp::Op> : fmt::formatter<std::string_view> {
    template <typename FormatContext>
    auto format(IRICmp::Op op, FormatContext& ctx)
    {
        std::string_view name;
        switch (op) {
        case IRICmp::Op::Eq:  name = "Eq";  break;
        case IRICmp::Op::Ne:  name = "Ne";  break;
        case IRICmp::Op::Ugt: name = "Ugt"; break;
        case IRICmp::Op::Uge: name = "Uge"; break;
        case IRICmp::Op::Ult: name = "Ult"; break;
        case IRICmp::Op::Ule: name = "Ule"; break;
        case IRICmp::Op::Sgt: name = "Sgt"; break;
        case IRICmp::Op::Sge: name = "Sge"; break;
        case IRICmp::Op::Slt: name = "Slt"; break;
        case IRICmp::Op::Sle: name = "Sle"; break;
        }
        return fmt::formatter<std::string_view>::format(name, ctx);
    }
};

namespace {

class IRPrinter {
public:
    void visit(const IRTree& tree)
    {
        print(R"(digraph G {{node [shape=record];)");

        for (auto* func : tree.funcs)
            visit(func);

        print("}}\n");
    }

    void visit(IRFunction* n)
    {
        print(R"("{}"[label="{{Function| <entry> entry}}"];)", (void*)n);
        link(n, n->blocks[0], "entry");
        for (auto* block : n->blocks)
            visit(block);
    }

    void* visit(IRBlock* block) 
    {
        print(R"("{}"[label="{{Block| <entry> entry}}"];)", (void*)block);
        print_list(block->instructions, block, "entry");
        return block;
    }

    void* visit(IRValue* node)
    {
        switch (node->kind) {
        case IRValueKind::Const: {
            auto* n = static_cast<IRConst*>(node);
            print(R"("{}"[label="{{Const|type: {}|value: {}}}"];)", (void*)n,
                  n->type, n->value);
            return node;
        }
        case IRValueKind::Extern: {
            auto* n = static_cast<IRExtern*>(node);
            print(R"("{}"[label="{{Extern|type: {}|name: {}}}"];)", (void*)n,
                  n->type, n->name);
            return node;
        }
        case IRValueKind::Param: {
            auto* n = static_cast<IRParam*>(node);
            print(R"("{}"[label="{{Const|type: {}|id: {}}}"];)", (void*)n,
                  n->type, n->id);
            return node;
        }
        case IRValueKind::Alloca: {
            auto* n = static_cast<IRAlloca*>(node);
            print(R"("{}"[label="{{Alloca|type: {}|storage_type: {}}}"];)",
                  (void*)n, n->type, n->storage_type);
            return node;
        }
        default:
            return node;
        }
    }

    void* visit(IRInst* node)
    {
        switch (node->kind) {
        case IRValueKind::Load: {
            auto* n = static_cast<IRLoad*>(node);
            print(R"("{}"[label="{{Load|{{type: {}| <ptr> ptr}}}}"];)", (void*)n,
                  n->type);
            link(node, visit(n->ptr), "ptr");
            return node;
        }
        case IRValueKind::Store: {
            auto* n = static_cast<IRStore*>(node);
            print(
                R"("{}"[label="{{Store|{{stype: {}| <ptr> ptr| <value> value}}}}"];)",
                (void*)n, n->stype);
            link(node, visit(n->ptr), "ptr");
            link(node, visit(n->value), "value");
            return node;
        }
        case IRValueKind::Call: {
            auto* n = static_cast<IRCall*>(node);
            print(
                R"("{}"[label="{{Call|{{type: {}| <callee> callee| <args> args}}}}"];)",
                (void*)n, n->type);
            link(node, visit(n->callee), "callee");
            print_list(n->args, node, "args");
            return node;
        }
        case IRValueKind::Ret: {
            auto* n = static_cast<IRRet*>(node);
            print(R"("{}"[label="{{Ret|{{retty: {}| <value> value}}}}"];)",
                  (void*)n, n->retty);
            link(node, visit(n->value), "value");
            return node;
        }
        case IRValueKind::Br: {
            auto* n = static_cast<IRBr*>(node);
            print(R"("{}"[label="{{Br|{{<dest> dest}}}}"];)", (void*)n);
            link(node, n->dest, "dest");
            return node;
        }
        case IRValueKind::CondBr: {
            auto* n = static_cast<IRCondBr*>(node);
            print(
                R"("{}"[label="{{CondBr|{{cond_type: {}| <cond> cond| <true> true| <false> false}}}}"];)",
                (void*)n, n->cond_type);
            link(node, visit(n->cond), "cond");
            link(node, n->true_block, "true");
            link(node, n->false_block, "false");
            return node;
        }
        case IRValueKind::Zext: {
            auto* n = static_cast<IRZext*>(node);
            print(
                R"("{}"[label="{{Zext|{{to: {}| from: {}| <value> value}}}}"];)",
                (void*)n, n->type, n->from);
            link(node, visit(n->value), "value");
            return node;
        }
        case IRValueKind::Sext: {
            auto* n = static_cast<IRSext*>(node);
            print(
                R"("{}"[label="{{Sext|{{to: {}| from: {}| <value> value}}}}"];)",
                (void*)n, n->type, n->from);
            link(node, visit(n->value), "value");
            return node;
        }
        case IRValueKind::Trunc: {
            auto* n = static_cast<IRTrunc*>(node);
            print(
                R"("{}"[label="{{Trunc|{{to: {}| from: {}| <value> value}}}}"];)",
                (void*)n, n->type, n->from);
            link(node, visit(n->value), "value");
            return node;
        }
        case IRValueKind::Add: {
            auto* n = static_cast<IRAdd*>(node);
            print(R"("{}"[label="{{Add|{{type: {}| <lhs> lhs| <rhs> rhs}}}}"];)",
                  (void*)n, n->type);
            link(node, visit(n->lhs), "lhs");
            link(node, visit(n->rhs), "rhs");
            return node;
        }
        case IRValueKind::Sub: {
            auto* n = static_cast<IRSub*>(node);
            print(R"("{}"[label="{{Sub|{{type: {}| <lhs> lhs| <rhs> rhs}}}}"];)",
                  (void*)n, n->type);
            link(node, visit(n->lhs), "lhs");
            link(node, visit(n->rhs), "rhs");
            return node;
        }
        case IRValueKind::Mul: {
            auto* n = static_cast<IRMul*>(node);
            print(R"("{}"[label="{{Mul|{{type: {}| <lhs> lhs| <rhs> rhs}}}}"];)",
                  (void*)n, n->type);
            link(node, visit(n->lhs), "lhs");
            link(node, visit(n->rhs), "rhs");
            return node;
        }
        case IRValueKind::Div: {
            auto* n = static_cast<IRDiv*>(node);
            print(R"("{}"[label="{{Div|{{type: {}| <lhs> lhs| <rhs> rhs}}}}"];)",
                  (void*)n, n->type);
            link(node, visit(n->lhs), "lhs");
            link(node, visit(n->rhs), "rhs");
            return node;
        }
        case IRValueKind::Rem: {
            auto* n = static_cast<IRDiv*>(node);
            print(R"("{}"[label="{{Rem|{{type: {}| <lhs> lhs| <rhs> rhs}}}}"];)",
                  (void*)n, n->type);
            link(node, visit(n->lhs), "lhs");
            link(node, visit(n->rhs), "rhs");
            return node;
        }
        case IRValueKind::ICmp: {
            auto* n = static_cast<IRICmp*>(node);
            print(
                R"("{}"[label="{{ICmp{}|{{type: {}| <lhs> lhs| <rhs> rhs}}}}"];)",
                (void*)n, n->op, n->type);
            link(node, visit(n->lhs), "lhs");
            link(node, visit(n->rhs), "rhs");
            return node;
        }
        default:
            return node;
        }
    }

private:
    template <typename... Args>
    static void print(std::string_view format, const Args&... args)
    {
        fmt::print(format, args...);
    }

    static void link(void* from, void* to, std::string_view port)
    {
        print(R"("{}":"{}"->"{}";)", from, port, to);
    }

    template <typename R>
    void print_list(const R& range, void* root_id, std::string_view port)
    {
        bool first = true;
        for (const auto& decl : range) {
            void* id = visit(decl);
            if (std::exchange(first, false))
                print(R"("{}":"{}"->"{}";)", root_id, port, id);
            else
                print(R"("{}"->"{}"[style=dashed,color=dimgray];)", root_id, id);
            root_id = id;
        }
    }
};

} // namespace

void minijava::print_ir(const IRTree& tree)
{
    IRPrinter{}.visit(tree);
}
