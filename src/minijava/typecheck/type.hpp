#ifndef MINIJAVA_TYPECHECK_TYPE_HPP
#define MINIJAVA_TYPECHECK_TYPE_HPP

#include <minijava/utility.hpp>

#include <unordered_map>

namespace minijava {

struct Type {
    enum Kind : int {
        Null,
        Bool,
        Int,
        IntArray,
        Class,
    } kind;
};

struct MethodSig {
    const Type* ret_type;
    Span<Type const* const> params;
};

struct ClassType : Type {
    const ClassType* base;

    std::unordered_map<Symbol, const Type*> m_vars;
    std::unordered_map<Symbol, MethodSig> m_methods;

    static constexpr bool classof(const Type& type) noexcept
    {
        return type.kind == Type::Class;
    }
};

inline constexpr Type NullType = Type{Type::Null};
inline constexpr Type BoolType = Type{Type::Bool};
inline constexpr Type IntType = Type{Type::Int};
inline constexpr Type IntArrayType = Type{Type::IntArray};

} // namespace minijava

#endif
