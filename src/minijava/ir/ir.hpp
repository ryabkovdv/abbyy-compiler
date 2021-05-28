#ifndef MINIJAVA_IR_IR_HPP
#define MINIJAVA_IR_IR_HPP

#include <minijava/utility.hpp>

namespace minijava {

struct IRBlock;
struct IRInst;

enum struct IRType {
    None,

    i1,
    i8,
    i16,
    i32,
    i64,
};

enum struct IRValueKind {
    Const,
    Extern,
    Param,

    Alloca,
    Load,
    Store,

    Call,
    Ret,
    Br,
    CondBr,

    Zext,
    Sext,
    Trunc,

    Add,
    Sub,
    Mul,
    Div,
    Rem,
    ICmp,

    Select,
};

struct IRValue {
    IRValueKind kind;
    IRType type = IRType::None;
};

struct IRConst : IRValue {
    uint64_t value;

    explicit IRConst(IRType type, uint64_t value)
        : IRValue{IRValueKind::Const, type}, value(value)
    {}
};

struct IRExtern : IRValue {
    std::string_view name;

    explicit IRExtern(IRType ptr_type, std::string_view name)
        : IRValue{IRValueKind::Extern, ptr_type}, name(name)
    {}
};

struct IRParam : IRValue {
    unsigned id;

    explicit IRParam(IRType type, unsigned id)
        : IRValue{IRValueKind::Param, type}, id(id)
    {}
};

struct IRInst : IRValue {
};

struct IRAlloca : IRInst {
    IRType storage_type;

    explicit IRAlloca(IRType ptr_type, IRType storage_type)
        : IRInst{IRValueKind::Alloca, ptr_type}, storage_type(storage_type)
    {}
};

struct IRLoad : IRInst {
    IRValue* ptr;

    explicit IRLoad(IRType type, IRValue* ptr)
        : IRInst{IRValueKind::Load, type}, ptr(ptr)
    {}
};

struct IRStore : IRInst {
    IRType stype;
    IRValue* ptr;
    IRValue* value;

    explicit IRStore(IRType stype, IRValue* ptr, IRValue* value)
        : IRInst{IRValueKind::Store}, stype(stype), ptr(ptr), value(value)
    {}
};

struct IRCall : IRInst {
    IRValue* callee;
    Span<IRValue*> args;

    explicit IRCall(IRType type, IRValue* callee, Span<IRValue*> args)
        : IRInst{IRValueKind::Call, type}, callee(callee), args(args)
    {}
};

struct IRRet : IRInst {
    IRType retty;
    IRValue* value;

    explicit IRRet(IRType retty, IRValue* value)
        : IRInst{IRValueKind::Ret}, retty(retty), value(value)
    {}
};

struct IRBr : IRInst {
    IRBlock* dest;

    explicit IRBr(IRBlock* dest) : IRInst{IRValueKind::Br}, dest(dest)
    {}
};

struct IRCondBr : IRInst {
    IRType cond_type;
    IRValue* cond;
    IRBlock* true_block;
    IRBlock* false_block;

    explicit IRCondBr(IRType cond_type, IRValue* cond, IRBlock* true_block,
                      IRBlock* false_block)
        : IRInst{IRValueKind::CondBr}, cond_type(cond_type), cond(cond),
          true_block(true_block), false_block(false_block)
    {}
};

struct IRCast : IRInst {
    IRType from;
    IRValue* value;
};

struct IRZext : IRCast {
    explicit IRZext(IRType to, IRType from, IRValue* value)
        : IRCast{IRValueKind::Zext, to, from, value}
    {}
};

struct IRSext : IRCast {
    explicit IRSext(IRType to, IRType from, IRValue* value)
        : IRCast{IRValueKind::Sext, to, from, value}
    {}
};

struct IRTrunc : IRCast {
    explicit IRTrunc(IRType to, IRType from, IRValue* value)
        : IRCast{IRValueKind::Trunc, to, from, value}
    {}
};

struct IRBinOp : IRInst {
    IRValue* lhs;
    IRValue* rhs;
};

struct IRAdd : IRBinOp {
    explicit IRAdd(IRType type, IRValue* lhs, IRValue* rhs)
        : IRBinOp{IRValueKind::Add, type, lhs, rhs}
    {}
};

struct IRSub : IRBinOp {
    explicit IRSub(IRType type, IRValue* lhs, IRValue* rhs)
        : IRBinOp{IRValueKind::Sub, type, lhs, rhs}
    {}
};

struct IRMul : IRBinOp {
    explicit IRMul(IRType type, IRValue* lhs, IRValue* rhs)
        : IRBinOp{IRValueKind::Mul, type, lhs, rhs}
    {}
};

struct IRDiv : IRBinOp {
    explicit IRDiv(IRType type, IRValue* lhs, IRValue* rhs)
        : IRBinOp{IRValueKind::Div, type, lhs, rhs}
    {}
};

struct IRRem : IRBinOp {
    explicit IRRem(IRType type, IRValue* lhs, IRValue* rhs)
        : IRBinOp{IRValueKind::Rem, type, lhs, rhs}
    {}
};

struct IRICmp : IRBinOp {
    enum Op {
        Eq,
        Ne,
        Ugt,
        Uge,
        Ult,
        Ule,
        Sgt,
        Sge,
        Slt,
        Sle,
    } op;

    explicit IRICmp(IRType type, Op op, IRValue* lhs, IRValue* rhs)
        : IRBinOp{IRValueKind::ICmp, type, lhs, rhs}, op(op)
    {}
};

struct IRSelect : IRInst {
    IRType cond_type;
    IRValue* cond;
    IRValue* true_val;
    IRValue* false_val;

    explicit IRSelect(IRType type, IRType cond_type, IRValue* cond,
                      IRValue* true_val, IRValue* false_val)
        : IRInst{IRValueKind::Select, type}, cond_type(cond_type), cond(cond),
          true_val(true_val), false_val(false_val)
    {}
};

struct IRBlock {
    GCArray<IRInst*> instructions;

    void insert(IRInst* inst)
    {
        instructions.push_back(inst);
    }
};

struct IRFunction {
    IRType retty;
    Span<IRType> params;
    GCArray<IRBlock*> blocks;
};

struct IRTree {
    BumpAllocator pool;

    GCArray<IRFunction*> funcs{pool};
};

class IRBuilder {
public:
    explicit IRBuilder(IRTree& tree) : m_pool(tree.pool), m_tree(tree)
    {}

    auto* insertFunction()
    {
        auto* func = new (m_pool) IRFunction{};
        func->blocks = GCArray<IRBlock*>(m_pool);
        m_tree.funcs.push_back(func);
        return func;
    }

    auto* getFunction()
    {
        return m_func;
    }

    void setFunction(IRFunction* func)
    {
        m_func = func;
    }

    auto* insertBlock()
    {
        auto* block = new (m_pool) IRBlock{};
        block->instructions = GCArray<IRInst*>(m_pool);
        m_func->blocks.push_back(block);
        return block;
    }

    auto* getBlock()
    {
        return m_block;
    }

    void setBlock(IRBlock* block)
    {
        m_block = block;
    }

    template <typename... Args>
    auto* newConst(Args&&... args)
    {
        return new (m_pool) IRConst(std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto* newExtern(Args&&... args)
    {
        return new (m_pool) IRExtern(std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto* newParam(Args&&... args)
    {
        return new (m_pool) IRParam(std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto* insertAlloca(Args&&... args)
    {
        auto* inst = new (m_pool) IRAlloca(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertLoad(Args&&... args)
    {
        auto* inst = new (m_pool) IRLoad(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertStore(Args&&... args)
    {
        auto* inst = new (m_pool) IRStore(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertCall(Args&&... args)
    {
        auto* inst = new (m_pool) IRCall(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    Span<IRValue*> make_args(Args... args)
    {
        GCArray<IRValue*> a(sizeof...(args), m_pool);
        (a.push_back(args), ...);
        return a;
    }

    template <typename... Args>
    auto* insertRet(Args&&... args)
    {
        auto* inst = new (m_pool) IRRet(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertBr(Args&&... args)
    {
        auto* inst = new (m_pool) IRBr(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertCondBr(Args&&... args)
    {
        auto* inst = new (m_pool) IRCondBr(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertZext(Args&&... args)
    {
        auto* inst = new (m_pool) IRZext(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertSext(Args&&... args)
    {
        auto* inst = new (m_pool) IRSext(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertTrunc(Args&&... args)
    {
        auto* inst = new (m_pool) IRTrunc(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    IRValue* insertZextOrTrunc(IRType to, IRType from, IRValue* value)
    {
        if (to == from)
            return value;
        if (to < from)
            return insertTrunc(to, from, value);
        return insertZext(to, from, value);
    }

    IRValue* insertSextOrTrunc(IRType to, IRType from, IRValue* value)
    {
        if (to == from)
            return value;
        if (to < from)
            return insertTrunc(to, from, value);
        return insertSext(to, from, value);
    }

    template <typename... Args>
    auto* insertAdd(Args&&... args)
    {
        auto* inst = new (m_pool) IRAdd(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertSub(Args&&... args)
    {
        auto* inst = new (m_pool) IRSub(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertMul(Args&&... args)
    {
        auto* inst = new (m_pool) IRMul(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertDiv(Args&&... args)
    {
        auto* inst = new (m_pool) IRDiv(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertRem(Args&&... args)
    {
        auto* inst = new (m_pool) IRRem(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertICmp(Args&&... args)
    {
        auto* inst = new (m_pool) IRICmp(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    template <typename... Args>
    auto* insertSelect(Args&&... args)
    {
        auto* inst = new (m_pool) IRSelect(std::forward<Args>(args)...);
        m_block->instructions.push_back(inst);
        return inst;
    }

    BumpAllocator& get_pool() const
    {
        return m_pool;
    }

private:
    BumpAllocator& m_pool;
    IRTree& m_tree;

    IRFunction* m_func = nullptr;
    IRBlock* m_block = nullptr;
};

} // namespace minijava

#endif
