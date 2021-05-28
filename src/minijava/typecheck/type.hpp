#ifndef MINIJAVA_TYPECHECK_TYPE_HPP
#define MINIJAVA_TYPECHECK_TYPE_HPP

#include <minijava/utility.hpp>

#include <fmt/format.h>

namespace minijava {

struct Type {
    enum Kind {
        NullID,
        BoolID,
        IntID,
        IntArrayID,
        ClassID,
    } kind;
};

inline constexpr auto NullType = Type{Type::NullID};
inline constexpr auto BoolType = Type{Type::BoolID};
inline constexpr auto IntType = Type{Type::IntID};
inline constexpr auto IntArrayType = Type{Type::IntArrayID};

struct Signature {
    const Type* retty;
    Span<Type const* const> params;

    friend bool operator==(const Signature& lhs, const Signature& rhs)
    {
        if (lhs.retty != rhs.retty)
            return false;

        if (lhs.params.size() != rhs.params.size())
            return false;

        for (size_t i = 0; i < lhs.params.size(); ++i) {
            if (lhs.params[i] != rhs.params[i])
                return false;
        }

        return true;
    }

    friend bool operator!=(const Signature& lhs, const Signature& rhs)
    {
        return !(lhs == rhs);
    }
};

struct ClassType : Type {
    struct VarEntry {
        Symbol name;
        const Type* type;
        uint64_t offset;
    };

    struct MethodEntry {
        Symbol name;
        Signature sig;
    };

    uint64_t size = 0;
    std::string_view name;
    const ClassType* base = nullptr;
    Span<const VarEntry> vars;
    Span<const MethodEntry> methods;

    ClassType() : Type{Type::ClassID}
    {}

    static constexpr bool classof(const Type& type) noexcept
    {
        return type.kind == Type::ClassID;
    }
};

} // namespace minijava

template <>
struct fmt::formatter<minijava::Symbol> : fmt::formatter<std::string_view> {
    template <typename FormatContext>
    auto format(minijava::Symbol symbol, FormatContext& ctx)
    {
        return fmt::formatter<std::string_view>::format(symbol.to_view(), ctx);
    }
};

template <>
struct fmt::formatter<minijava::Type> : fmt::formatter<std::string_view> {
    template <typename FormatContext>
    auto format(const minijava::Type& type, FormatContext& ctx)
    {
        std::string_view name;
        switch (type.kind) {
        case minijava::Type::NullID:
            name = "null";
            break;
        case minijava::Type::BoolID:
            name = "boolean";
            break;
        case minijava::Type::IntID:
            name = "int";
            break;
        case minijava::Type::IntArrayID:
            name = "int[]";
            break;
        case minijava::Type::ClassID:
            auto& cl_type = static_cast<const minijava::ClassType&>(type);
            name = cl_type.name;
            break;
        }
        return fmt::formatter<std::string_view>::format(name, ctx);
    }
};

#endif
