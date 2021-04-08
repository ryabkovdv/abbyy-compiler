#ifndef MINIJAVA_TYPECHECK_TYPE_HPP
#define MINIJAVA_TYPECHECK_TYPE_HPP

namespace minijava {

struct Type {
    enum Kind : int {
        Void,
        Bool,
        Int,
        IntArray,
        Class,
    } kind;
};

struct ClassType : Type {
    const ClassType* base;

    static constexpr bool classof(const Type& type) noexcept
    {
        return type.kind == Type::Class;
    }
};

inline constexpr Type VoidType = Type{Type::Void};
inline constexpr Type BoolType = Type{Type::Bool};
inline constexpr Type IntType = Type{Type::Int};
inline constexpr Type IntArrayType = Type{Type::IntArray};

} // namespace minijava

#endif
