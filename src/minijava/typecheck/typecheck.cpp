#include <minijava/typecheck/typecheck.hpp>

#include <minijava/typecheck/type.hpp>

using namespace minijava;

static bool is_same(const Type* x, const Type* y) noexcept
{
    if (x == y)
        return true;
    if (x == nullptr || y == nullptr)
        return true;
    return false;
}

static bool is_convertible(const Type* to, const Type* from) noexcept
{
    if (from == to)
        return true;

    if (from == nullptr || to == nullptr)
        return true;

    if (from->kind == Type::NullID)
        return true;

    if (auto* cl_to = dyn_cast<ClassType>(to)) {
        auto* cl_from = dyn_cast<ClassType>(from);
        while (cl_from != nullptr) {
            if (cl_from == cl_to)
                return true;
            cl_from = cl_from->base;
        }
    }
    return false;
}

namespace {

struct Arch {
    IRType bool_type;
    IRType int_type;
    IRType ptr_type;

    uint64_t bool_size;
    uint64_t int_size;
    uint64_t ptr_size;
};

struct TypeValuePair {
    const Type* type;
    IRValue* value;
};

struct SymbolTypePair {
    Symbol name;
    const Type* type;
};

struct SymbolTypeValue {
    Symbol name;
    const Type* type;
    IRValue* mem;
};

struct SignatureIndexPair {
    const Signature* sig;
    unsigned index;
};

class TypeGenerator
    : public AstVisitor<TypeGenerator, void, std::vector<SymbolTypePair>> {
public:
    TypeGenerator(BumpAllocator& pool, DiagnosticEngine& diag)
        : m_pool(pool), m_diag(diag)
    {
        m_arch = {IRType::i8, IRType::i32, IRType::i64, 1, 4, 8};
    }

    std::vector<SymbolTypePair> visitTree(const AstTree& tree)
    {
        insert_type(BoolSymbol, &BoolType, {});
        insert_type(IntSymbol, &IntType, {});
        insert_type(IntArraySymbol, &IntArrayType, {});

        std::vector<ClassType*> classes;
        for (auto* cl : tree.classes) {
            auto* cl_type = new (m_pool) ClassType{};
            cl_type->name = dup_string(m_pool, cl->name.to_view());
            if (!cl->base.empty()) {
                cl_type->base =
                    cast<ClassType>(lookup_type(cl->base, cl->base_loc));
            }
            classes.push_back(cl_type);
            insert_type(cl->name, cl_type, cl->name_loc);
        }

        for (size_t i = 0; i < classes.size(); ++i) {
            m_class = classes[i];
            visit(tree.classes[i]);
        }

        return std::move(m_types);
    }

    void visitClassDecl(const ClassDecl& node)
    {
        m_next_offset = m_arch.ptr_size;
        m_vars = GCArray<ClassType::VarEntry>(m_pool);
        m_methods = GCArray<ClassType::MethodEntry>(m_pool);
        m_local_vars.clear();
        m_local_methods.clear();

        if (m_class->base != nullptr) {
            for (auto& x : m_class->base->vars)
                m_vars.push_back(x);
            for (auto& x : m_class->base->methods)
                m_methods.push_back(x);
            m_next_offset = m_class->base->size;
        }

        visit(node.records);

        m_class->size = m_next_offset;
        m_class->vars = m_vars;
        m_class->methods = m_methods;
    }

    void visitClassVarDecl(const ClassVarDecl& node)
    {
        auto* type = lookup_type(node.type, node.type_loc);

        for (auto name : m_local_vars) {
            if (node.name == name) {
                m_diag.error(node.name_loc,
                             "redeclaration of class variable '{}'", name);
                return;
            }
        }

        m_local_vars.push_back(node.name);
        m_vars.push_back({node.name, type, get_offset(type)});
    }

    void visitMethodDecl(const MethodDecl& node)
    {
        auto* retty = lookup_type(node.retty, node.retty_loc);
        GCArray<const Type*> params(node.params.size(), m_pool);
        for (auto& param : node.params) {
            auto* type = lookup_type(param.type, param.type_loc);
            params.push_back(type);
        }

        auto sig = Signature{retty, params};
        for (auto name : m_local_methods) {
            if (node.name == name) {
                m_diag.error(node.name_loc, "redeclaration of method '{}'",
                             name);
                return;
            }
        }
        m_local_methods.push_back(node.name);

        bool overriden = false;
        for (auto& [name, base_sig] : m_methods) {
            if (node.name != name)
                continue;

            overriden = true;
            if (sig != base_sig) {
                m_diag.error(node.name_loc,
                             "mismatched signature of overriden method '{}'",
                             name);
                return;
            }
            break;
        }

        if (!overriden)
            m_methods.push_back({node.name, sig});
    }

private:
    Arch m_arch;

    BumpAllocator& m_pool;
    DiagnosticEngine& m_diag;

    ClassType* m_class = nullptr;

    uint64_t m_next_offset = 0;

    GCArray<ClassType::VarEntry> m_vars;
    GCArray<ClassType::MethodEntry> m_methods;

    std::vector<Symbol> m_local_vars;
    std::vector<Symbol> m_local_methods;

    std::vector<SymbolTypePair> m_types;

    void insert_type(Symbol name, const Type* type, Location loc)
    {
        for (auto& [type_name, _] : m_types) {
            if (name == type_name) {
                m_diag.error(loc, "redeclaration of type '{}'", name);
                return;
            }
        }
        m_types.push_back({name, type});
    }

    const Type* lookup_type(Symbol name, Location loc)
    {
        for (auto& [type_name, type] : m_types) {
            if (name == type_name)
                return type;
        }

        m_diag.error(loc, "type '{}' not found", name);
        return nullptr;
    }

    uint64_t get_offset(const Type* type)
    {
        if (type == nullptr)
            return -1;

        uint64_t size;
        switch (type->kind) {
        case Type::BoolID:
            size = m_arch.bool_size;
            break;
        case Type::IntID:
            size = m_arch.int_size;
            break;
        case Type::NullID:
        case Type::IntArrayID:
        case Type::ClassID:
            size = m_arch.ptr_size;
        }

        uint64_t align = size;
        uint64_t offset = (m_next_offset + align - 1) & -align;
        m_next_offset = offset + size;
        return offset;
    }
};

class TypeChecker : public AstVisitor<TypeChecker, TypeValuePair> {
public:
    TypeChecker(Span<const SymbolTypePair> types, DiagnosticEngine& diag,
                IRBuilder& builder)
        : m_ir(builder), m_pool(builder.get_pool()), m_diag(diag),
          m_types(types)
    {
        m_arch = {IRType::i8, IRType::i32, IRType::i64, 1, 4, 8};
        m_this_ptr = m_ir.newParam(m_arch.ptr_type, 0);
    }

    void visitTree(const AstTree& tree)
    {
        push_scope();
        IRFunction* func = m_ir.insertFunction();
        m_ir.setFunction(func);
        m_ir.setBlock(m_ir.insertBlock());
        visit(tree.main);
        pop_scope();

        visit(tree.classes);
    }

    TypeValuePair visitClassDecl(const ClassDecl& node)
    {
        m_class = cast<ClassType>(lookup_type(node.name, node.name_loc));
        visit(node.records);
        return {};
    }

    TypeValuePair visitClassVarDecl(const ClassVarDecl& node)
    {
        (void)node;
        return {};
    }

    TypeValuePair visitMethodDecl(const MethodDecl& node)
    {
        push_scope();

        IRFunction* func = m_ir.insertFunction();
        m_ir.setFunction(func);
        m_ir.setBlock(m_ir.insertBlock());

        m_retty = lookup_type(node.retty, node.retty_loc);
        func->retty = get_ir_type(m_retty);

        // 0'th parameter is 'this' pointer.
        GCArray<IRType> param_types(1 + node.params.size(), m_pool);
        param_types.push_back(m_arch.ptr_type);
        unsigned i = 1;
        for (auto& param : node.params) {
            auto* type = lookup_type(param.type, param.type_loc);
            IRType ir_type = get_ir_type(type);
            IRValue* ptr = insert_variable(param.name, type, param.name_loc);
            m_ir.insertStore(ir_type, ptr, m_ir.newParam(ir_type, i));
            param_types.push_back(ir_type);
            ++i;
        }
        func->params = param_types;

        visit(node.body);

        pop_scope();
        return {};
    }

    TypeValuePair visitLocalVarDecl(const LocalVarDecl& node)
    {
        auto* type = lookup_type(node.type, node.type_loc);
        insert_variable(node.name, type, node.name_loc);
        return {};
    }

    TypeValuePair visitCompoundStmt(const CompoundStmt& node)
    {
        push_scope();
        visit(node.body);
        pop_scope();
        return {};
    }

    TypeValuePair visitIfStmt(const IfStmt& node)
    {
        IRBlock* then_block = m_ir.insertBlock();
        IRBlock* else_block = m_ir.insertBlock();
        IRBlock* end_block = m_ir.insertBlock();

        IRValue* cond_val = as_bool(node.cond);
        m_ir.insertCondBr(m_arch.bool_type, cond_val, then_block, else_block);

        push_scope();
        m_ir.setBlock(then_block);
        visit(node.then_branch);
        m_ir.insertBr(end_block);
        pop_scope();

        push_scope();
        m_ir.setBlock(else_block);
        visit(node.else_branch);
        m_ir.insertBr(end_block);
        pop_scope();

        m_ir.setBlock(end_block);
        return {};
    }

    TypeValuePair visitWhileStmt(const WhileStmt& node)
    {
        IRBlock* header_block = m_ir.insertBlock();
        IRBlock* body_block = m_ir.insertBlock();
        IRBlock* end_block = m_ir.insertBlock();

        m_ir.insertBr(header_block);
        m_ir.setBlock(header_block);

        IRValue* cond_val = as_bool(node.cond);
        m_ir.insertCondBr(m_arch.bool_type, cond_val, body_block, end_block);

        push_scope();
        m_ir.setBlock(body_block);
        visit(node.body);
        m_ir.insertBr(header_block);
        pop_scope();

        m_ir.setBlock(end_block);
        return {};
    }

    TypeValuePair visitReturnStmt(const ReturnStmt& node)
    {
        IRValue* value = convert_to(m_retty, node.expr);
        m_ir.insertRet(get_ir_type(m_retty), value);
        return {};
    }

    TypeValuePair visitPrintStmt(const PrintStmt& node)
    {
        GCArray<IRValue*> call_args(node.args.size(), m_pool);
        for (auto* arg : node.args)
            call_args.push_back(visit(arg).value);

        m_ir.insertCall(
            IRType::None,
            m_ir.newExtern(m_arch.ptr_type, "__minijava_print"),
            call_args
        );
        return {};
    }

    TypeValuePair visitAssignStmt(const AssignStmt& node)
    {
        if (auto* ident = dyn_cast<IdentExpr>(node.lhs)) {
            auto [lhs_type, lhs_mem] =
                get_variable_memory(ident->name, ident->loc);
            m_ir.insertStore(
                get_ir_type(lhs_type),
                lhs_mem,
                convert_to(lhs_type, node.rhs)
            );
            return {};
        }

        if (auto* subscript = dyn_cast<SubscriptExpr>(node.lhs)) {
            if (isa<IdentExpr>(subscript->array)) {
                IRValue* mem = m_ir.insertAdd(
                    m_arch.ptr_type,
                    as_int_array(subscript->array),
                    m_ir.insertMul(
                        m_arch.ptr_type,
                        m_ir.newConst(m_arch.ptr_type, m_arch.int_size),
                        m_ir.insertZextOrTrunc(
                            m_arch.ptr_type,
                            m_arch.int_type,
                            as_int(subscript->index)
                        )
                    )
                );
                m_ir.insertStore(
                    m_arch.int_type,
                    mem,
                    as_int(node.rhs)
                );
                return {};
            }
        }

        m_diag.error(node.loc, "left hand side is not an lvalue");
        return {};
    }

    TypeValuePair visitEqExpr(const EqExpr& node)
    {
        return visitEqNeExpr(node);
    }

    TypeValuePair visitNeExpr(const NeExpr& node)
    {
        return visitEqNeExpr(node);
    }

    TypeValuePair visitEqNeExpr(const BinaryExpr& node)
    {
        auto op = (node.kind == StmtKind::EqExpr) ? IRICmp::Eq : IRICmp::Ne;
        auto [lhs_type, lhs_val] = visit(node.lhs);
        auto [rhs_type, rhs_val] = visit(node.rhs);

        if (!is_convertible(lhs_type, rhs_type) &&
            !is_convertible(rhs_type, lhs_type)) {
            m_diag.error(node.op_loc,
                         "cannot compare objects "
                         "of types '{}' and '{}'",
                         *lhs_type, *rhs_type);
            return {};
        }

        if (lhs_type == nullptr || rhs_type == nullptr) {
            return {};
        }

        if (lhs_type == &NullType && rhs_type == &NullType) {
            IRValue* value = m_ir.newConst(
                m_arch.bool_type,
                node.kind == StmtKind::EqExpr
            );
            return {&BoolType, value};
        }

        if (lhs_type == &NullType) {
            std::swap(lhs_type, rhs_type);
            std::swap(lhs_val, rhs_val);
        }

        if (rhs_type == &NullType)
            rhs_val = m_ir.newConst(get_ir_type(lhs_type), 0);

        IRValue* value = m_ir.insertICmp(
            get_ir_type(lhs_type),
            op,
            lhs_val,
            rhs_val
        );
        value = m_ir.insertZextOrTrunc(m_arch.bool_type, IRType::i1, value);
        return {&BoolType, value};
    }

    TypeValuePair visitAndExpr(const AndExpr& node)
    {
        IRValue* value_ptr = m_ir.insertAlloca(m_arch.ptr_type, m_arch.bool_type);
        IRBlock* zero_block = m_ir.insertBlock();
        IRBlock* nonzero_block = m_ir.insertBlock();
        IRBlock* end_block = m_ir.insertBlock();

        m_ir.insertCondBr(
            m_arch.bool_type,
            as_bool(node.lhs),
            nonzero_block, zero_block
        );

        m_ir.setBlock(zero_block);
        m_ir.insertStore(
            m_arch.bool_type,
            value_ptr,
            m_ir.newConst(m_arch.bool_type, 0)
        );
        m_ir.insertBr(end_block);

        m_ir.setBlock(nonzero_block);
        m_ir.insertStore(
            m_arch.bool_type,
            value_ptr,
            as_bool(node.rhs)
        );
        m_ir.insertBr(end_block);

        m_ir.setBlock(end_block);
        IRValue* value = m_ir.insertLoad(m_arch.bool_type, value_ptr);
        return {&BoolType, value};
    }

    TypeValuePair visitOrExpr(const OrExpr& node)
    {
        IRValue* value_ptr = m_ir.insertAlloca(m_arch.ptr_type, m_arch.bool_type);
        IRBlock* zero_block = m_ir.insertBlock();
        IRBlock* nonzero_block = m_ir.insertBlock();
        IRBlock* end_block = m_ir.insertBlock();

        m_ir.insertCondBr(
            m_arch.bool_type,
            as_bool(node.lhs),
            nonzero_block, zero_block
        );

        m_ir.setBlock(nonzero_block);
        m_ir.insertStore(
            m_arch.bool_type,
            value_ptr,
            m_ir.newConst(m_arch.bool_type, 1)
        );
        m_ir.insertBr(end_block);

        m_ir.setBlock(zero_block);
        m_ir.insertStore(
            m_arch.bool_type,
            value_ptr,
            as_bool(node.rhs)
        );
        m_ir.insertBr(end_block);

        m_ir.setBlock(end_block);
        IRValue* value = m_ir.insertLoad(m_arch.bool_type, value_ptr);
        return {&BoolType, value};
    }

    TypeValuePair visitBinaryExpr(const BinaryExpr& node)
    {
        auto [lhs_type, lhs_val] = visit(node.lhs);
        auto [rhs_type, rhs_val] = visit(node.rhs);

        IRValue* binop;
        const Type* op_type;
        const Type* retty;
        switch (node.kind) {
        case StmtKind::MulExpr:
        case StmtKind::DivExpr:
        case StmtKind::RemExpr:
        case StmtKind::AddExpr:
        case StmtKind::SubExpr:
            op_type = &IntType;
            retty = &IntType;
            switch (node.kind) {
            case StmtKind::MulExpr:
                binop = m_ir.insertMul(m_arch.int_type, lhs_val, rhs_val);
                break;
            case StmtKind::DivExpr:
                binop = m_ir.insertDiv(m_arch.int_type, lhs_val, rhs_val);
                break;
            case StmtKind::RemExpr:
                binop = m_ir.insertRem(m_arch.int_type, lhs_val, rhs_val);
                break;
            case StmtKind::AddExpr:
                binop = m_ir.insertAdd(m_arch.int_type, lhs_val, rhs_val);
                break;
            case StmtKind::SubExpr:
                binop = m_ir.insertSub(m_arch.int_type, lhs_val, rhs_val);
                break;
            default:
                unreachable("invalid binary operation");
            }
            break;
        case StmtKind::LtExpr:
        case StmtKind::LeExpr:
        case StmtKind::GtExpr:
        case StmtKind::GeExpr: {
            op_type = &IntType;
            retty = &BoolType;
            IRICmp::Op op;
            switch (node.kind) {
            case StmtKind::LtExpr: op = IRICmp::Slt; break;
            case StmtKind::LeExpr: op = IRICmp::Sle; break;
            case StmtKind::GtExpr: op = IRICmp::Sgt; break;
            case StmtKind::GeExpr: op = IRICmp::Sge; break;
            default: unreachable("invalid binary operation");
            }
            binop = m_ir.insertICmp(m_arch.int_type, op, lhs_val, rhs_val);
            binop = m_ir.insertZextOrTrunc(m_arch.bool_type, IRType::i1, binop);
            break;
        }
        default:
            unreachable("invalid binary operation");
        }

        if (is_same(lhs_type, op_type) && is_same(rhs_type, op_type))
            return {retty, binop};

        if (lhs_type == rhs_type) {
            m_diag.error(node.op_loc,
                "invalid binary operator for type '{}': expected '{}'",
                *lhs_type, *op_type);
        } else {
            m_diag.error(node.op_loc,
                "invalid binary operator for types '{}' and '{}': expected '{}'",
                *lhs_type, *rhs_type, *op_type);
        }
        return {};
    }

    TypeValuePair visitNotExpr(const NotExpr& node)
    {
        IRValue* value = m_ir.insertSelect(
            m_arch.bool_type,
            m_arch.bool_type,
            as_bool(node.expr),
            m_ir.newConst(m_arch.bool_type, 0),
            m_ir.newConst(m_arch.bool_type, 1)
        );
        return {&BoolType, value};
    }

    TypeValuePair visitNegExpr(const NegExpr& node)
    {
        IRValue* value = m_ir.insertSub(
            m_arch.int_type,
            m_ir.newConst(m_arch.int_type, 0),
            as_int(node.expr)
        );
        return {&IntType, value};
    }

    TypeValuePair visitSubscriptExpr(const SubscriptExpr& node)
    {
        IRValue* value = m_ir.insertLoad(
            m_arch.int_type,
            m_ir.insertAdd(
                m_arch.ptr_type,
                as_int_array(node.array),
                m_ir.insertMul(
                    m_arch.ptr_type,
                    m_ir.newConst(m_arch.ptr_type, m_arch.int_size),
                    m_ir.insertZextOrTrunc(
                        m_arch.ptr_type,
                        m_arch.int_type,
                        as_int(node.index)
                    )
                )
            )
        );
        return {&IntType, value};
    }

    TypeValuePair visitCallExpr(const CallExpr& node)
    {
        auto [object_type, object_ptr] = visit(node.object);
        if (object_type == nullptr)
            return {};

        auto [signature, index] =
            lookup_method(object_type, node.name, node.name_loc);
        if (signature == nullptr)
            return {};

        auto args = node.args;
        auto params = signature->params;
        if (args.size() != params.size()) {
            m_diag.error(node.args_loc,
                         "argument count mismatch: "
                         "expected {}, got {}",
                         params.size(), args.size());
            return {};
        }

        GCArray<IRValue*> call_args(1 + args.size(), m_pool);
        call_args.push_back(object_ptr);
        for (size_t i = 0; i < args.size(); ++i)
            call_args.push_back(convert_to(params[i], args[i]));

        IRValue* vtable = m_ir.insertLoad(m_arch.ptr_type, object_ptr);
        IRValue* method = m_ir.insertLoad(
            m_arch.ptr_type,
            m_ir.insertAdd(
                m_arch.ptr_type,
                vtable,
                m_ir.newConst(m_arch.ptr_type, m_arch.ptr_size * index)
            )
        );

        IRValue* result = m_ir.insertCall(
            get_ir_type(signature->retty),
            method,
            call_args
        );
        return {signature->retty, result};
    }

    TypeValuePair visitLengthExpr(const LengthExpr& node)
    {
        IRValue* value = m_ir.insertLoad(
            m_arch.int_type,
            m_ir.insertAdd(
                m_arch.ptr_type,
                m_ir.newConst(m_arch.ptr_type, -m_arch.int_size),
                as_int_array(node.expr)
            )
        );
        return {&IntType, value};
    }

    TypeValuePair visitNewIntArrayExpr(const NewIntArrayExpr& node)
    {
        IRValue* ptr = m_ir.insertCall(
            m_arch.ptr_type,
            m_ir.newExtern(m_arch.ptr_type, "__minijava_newarray"),
            m_ir.make_args(
                as_int(node.count)
            )
        );
        return {&IntArrayType, ptr};
    }

    TypeValuePair visitNewObjectExpr(const NewObjectExpr& node)
    {
        auto* type = cast<ClassType>(lookup_type(node.name, node.name_loc));
        IRValue* ptr = m_ir.insertCall(
            m_arch.ptr_type,
            m_ir.newExtern(m_arch.ptr_type, "__minijava_new"),
            m_ir.make_args(
                m_ir.newConst(m_arch.ptr_type, type->size)
            )
        );
        m_ir.insertStore(
            m_arch.ptr_type,
            ptr,
            m_ir.newExtern(m_arch.ptr_type, make_vtable_name(node.name))
        );
        return {type, ptr};
    }

    TypeValuePair visitIdentExpr(const IdentExpr& node)
    {
        auto [type, ptr] = get_variable_memory(node.name, node.loc);
        IRValue* value = m_ir.insertLoad(get_ir_type(type), ptr);
        return {type, value};
    }

    TypeValuePair visitIntLiteral(const IntLiteral& node)
    {
        uint64_t value = 1234;
        return {&IntType, m_ir.newConst(m_arch.int_type, value)};
    }

    TypeValuePair visitBoolLiteral(const BoolLiteral& node)
    {
        return {&BoolType, m_ir.newConst(m_arch.bool_type, node.value)};
    }

    TypeValuePair visitNullExpr(const NullExpr&)
    {
        return {&NullType, nullptr};
    }

    TypeValuePair visitThisExpr(const ThisExpr& node)
    {
        if (m_class == nullptr)
            m_diag.error(node.loc, "cannot use 'this' inside of static method");
        return {m_class, m_this_ptr};
    }

private:
    Arch m_arch;

    IRBuilder& m_ir;
    BumpAllocator& m_pool;
    DiagnosticEngine& m_diag;

    const Type* m_retty = nullptr;
    const ClassType* m_class = nullptr;
    IRValue* m_this_ptr = nullptr;

    IRType get_ir_type(const Type* type) const
    {
        if (type == nullptr)
            return IRType::None;

        switch (type->kind) {
        case Type::BoolID:
            return m_arch.bool_type;
        case Type::IntID:
            return m_arch.int_type;
        case Type::NullID:
        case Type::IntArrayID:
        case Type::ClassID:
            return m_arch.ptr_type;
        }
        unreachable("invalid type kind");
    }

    std::string_view make_vtable_name(Symbol name)
    {
        static constexpr char Prefix[] = "__vtable_";
        auto cl_name = name.to_view();

        auto* data = static_cast<char*>(
            m_pool.allocate(sizeof(Prefix) + cl_name.size(), 1));
        std::memcpy(data, Prefix, sizeof(Prefix));
        std::memcpy(data + sizeof(Prefix), cl_name.data(), cl_name.size());
        return {data, sizeof(Prefix) + cl_name.size()};
    }

    Span<const SymbolTypePair> m_types;

    const Type* lookup_type(Symbol name, Location loc)
    {
        for (auto& [type_name, type] : m_types) {
            if (name == type_name)
                return type;
        }

        m_diag.error(loc, "type '{}' not found", name);
        return nullptr;
    }

    std::vector<SymbolTypeValue> m_vars;
    std::vector<size_t> m_scopes;

    void push_scope()
    {
        m_scopes.push_back(m_vars.size());
    }

    void pop_scope()
    {
        while (m_vars.size() != m_scopes.back())
            m_vars.pop_back();
        m_scopes.pop_back();
    }

    IRValue* insert_variable(Symbol name, const Type* type, Location loc)
    {
        for (size_t i = m_scopes.back(); i < m_vars.size(); ++i) {
            if (name == m_vars[i].name) {
                m_diag.error(loc, "redeclaration of variable '{}'", name);
                return nullptr;
            }
        }

        IRValue* value = m_ir.insertAlloca(m_arch.ptr_type, get_ir_type(type));
        m_vars.push_back({name, type, value});
        return value;
    }

    TypeValuePair get_variable_memory(Symbol name, Location loc)
    {
        for (ptrdiff_t i = m_vars.size() - 1; i >= 0; --i) {
            if (m_vars[i].name == name)
                return {m_vars[i].type, m_vars[i].mem};
        }

        if (m_class != nullptr) {
            auto& vars = m_class->vars;
            for (ptrdiff_t i = vars.size() - 1; i >= 0; --i) {
                if (vars[i].name == name) {
                    IRValue* mem = m_ir.insertAdd(
                        m_arch.ptr_type,
                        m_this_ptr,
                        m_ir.newConst(m_arch.ptr_type, vars[i].offset)
                    );
                    return {vars[i].type, mem};
                }
            }
        }

        m_diag.error(loc, "variable '{}' used before it is declared", name);
        return {};
    }

    SignatureIndexPair lookup_method(const Type* type, Symbol name, Location loc)
    {
        auto* cl_type = dyn_cast<ClassType>(type);
        if (cl_type == nullptr) {
            m_diag.error(loc, "type '{}' is not an object type", *type);
            return {};
        }

        unsigned i = 0;
        for (auto& [method_name, sig] : cl_type->methods) {
            if (name == method_name)
                return {&sig, i};
            ++i;
        }

        m_diag.error(loc, "class '{}' has no method named '{}'", *type, name);
        return {};
    }

    IRValue* as_bool(const Expr* expr)
    {
        auto [type, value] = visit(expr);
        verify_same(type, &BoolType, expr->loc);
        return value;
    }

    IRValue* as_int(const Expr* expr)
    {
        auto [type, value] = visit(expr);
        verify_same(type, &IntType, expr->loc);
        return value;
    }

    IRValue* as_int_array(const Expr* expr)
    {
        auto [type, value] = visit(expr);
        verify_same(type, &IntArrayType, expr->loc);
        return value;
    }

    IRValue* convert_to(const Type* type, const Expr* expr)
    {
        auto [expr_type, value] = visit(expr);
        if (expr_type == &NullType)
            return m_ir.newConst(get_ir_type(type), 0);

        verify_convertible(type, expr_type, expr->loc);
        return value;
    }

    void verify_same(const Type* x, const Type* y, Location loc)
    {
        if (!is_same(x, y)) {
            m_diag.error(loc,
                "mismatched types: expected '{}', got '{}'", *y, *x);
        }
    }

    void verify_convertible(const Type* to, const Type* from, Location loc)
    {
        if (!is_convertible(to, from))
            m_diag.error(loc, "cannot convert type '{}' to '{}'", *from, *to);
    }
};

} // namespace

void minijava::typecheck(AstTree& tree, DiagnosticEngine& diag,
                         IRBuilder* builder)
{
    auto types = TypeGenerator(tree.pool, diag).visit(tree);
    TypeChecker(types, diag, *builder).visit(tree);
}
