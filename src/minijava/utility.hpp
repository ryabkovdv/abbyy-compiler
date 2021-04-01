#ifndef MINIJAVA_UTILITY_HPP
#define MINIJAVA_UTILITY_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <new>
#include <numeric>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>

namespace minijava {

[[noreturn]]
inline void unreachable(const char* msg)
{
    (void)msg;
#ifdef _MSC_VER
    __assume(0);
#elif defined(__GNUC__)
    __builtin_unreachable();
#endif
}

template <typename F>
class [[nodiscard]] ScopeExit {
public:
    ScopeExit(const ScopeExit&) = delete;
    ScopeExit& operator=(const ScopeExit&) = delete;

    explicit constexpr ScopeExit(F callback) noexcept
        : m_callback(std::move(callback))
    {}

    ~ScopeExit() noexcept
    {
        if (m_active)
            m_callback();
    }

    constexpr void cancel() noexcept
    {
        m_active = false;
    }

private:
    F m_callback;
    bool m_active = true;
};

class BumpAllocator {
public:
    static constexpr size_t MaxAlign = 64;

    BumpAllocator() = default;
    BumpAllocator(const BumpAllocator&) = delete;
    BumpAllocator& operator=(const BumpAllocator&) = delete;

    ~BumpAllocator()
    {
        reset();
    }

    [[nodiscard, gnu::returns_nonnull]]
    [[gnu::malloc, gnu::alloc_size(2), gnu::alloc_align(3)]]
    void* allocate(size_t size, size_t alignment)
    {
        assert(alignment <= MaxAlign);
        assert(ptrdiff_t(size) >= 0 && "allocation size overflow");

        size_t aligned_space = m_space & -alignment;
        if (aligned_space < size) [[unlikely]]
            return allocate_block(size, alignment);
        m_space = aligned_space - size;
        return m_base - aligned_space;
    }

    bool try_resize(void* p, size_t old_size, size_t new_size) noexcept
    {
        assert(ptrdiff_t(old_size) >= 0 && "allocation size overflow");
        assert(ptrdiff_t(new_size) >= 0 && "allocation size overflow");

        auto* alloc_end = static_cast<std::byte*>(p) + old_size;
        if (alloc_end != m_base - m_space)
            return false;
        if (m_space < ptrdiff_t(new_size) - ptrdiff_t(old_size))
            return false;
        m_space -= new_size - old_size;
        return true;
    }

    void reset() noexcept
    {
        m_base = nullptr;
        m_space = 0;

        Block* current = std::exchange(m_blocks, nullptr);
        while (current != nullptr) {
            Block* next = current->next;
            std::free(current);
            current = next;
        }
    }

private:
    struct Block { Block* next; };

    std::byte* m_base = nullptr;
    ptrdiff_t m_space = 0;
    Block* m_blocks = nullptr;

    [[nodiscard, gnu::returns_nonnull]]
    [[gnu::noinline, gnu::malloc, gnu::alloc_size(2), gnu::alloc_align(3)]]
    void* allocate_block(size_t size, size_t alignment);
};

template <typename T>
class Span {
public:
    Span() = default;
    Span(const Span&) = default;
    Span& operator=(const Span&) = default;

    constexpr Span(T* first, size_t count) noexcept
        : m_begin(first), m_end(first + count)
    {}

    constexpr Span(T* first, T* last) noexcept : m_begin(first), m_end(last)
    {}

    template <typename R, typename = std::void_t<
        std::enable_if_t<!std::is_same_v<R, Span>>,
        decltype(std::data(std::declval<R>())),
        decltype(std::size(std::declval<R>())),
        std::enable_if_t<std::is_convertible_v<
            std::remove_pointer_t<decltype(std::data(std::declval<R&>()))> (*)[],
            T (*)[]
        >>>>
    constexpr Span(R&& range) noexcept
        : m_begin(std::data(range)), m_end(m_begin + std::size(range))
    {}

    constexpr T& operator[](size_t i) const noexcept
    {
        assert(i < size());
        return m_begin[i];
    }

    constexpr T& back(size_t i = 1) const noexcept
    {
        assert(1 <= i && i <= size());
        return m_end[-i];
    }

    constexpr Span first(size_t count) const noexcept
    {
        assert(count <= size());
        return Span(m_begin, m_begin + count);
    }

    constexpr Span last(size_t count) const noexcept
    {
        assert(count <= size());
        return Span(m_end - count, m_end);
    }

    constexpr Span subspan(size_t offset) const noexcept
    {
        assert(offset <= size());
        return Span(m_begin + offset, m_end);
    }

    constexpr Span subspan(size_t offset, size_t count) const noexcept
    {
        assert(offset <= size());
        assert(count <= size() - offset);
        return Span(m_begin + offset, count);
    }

    constexpr size_t size()   const noexcept { return m_end - m_begin; }
    constexpr bool   empty()  const noexcept { return m_begin == m_end; }
    constexpr T*     data()   const noexcept { return m_begin; }
    constexpr T*     begin()  const noexcept { return m_begin; }
    constexpr T*     end()    const noexcept { return m_end; }
    constexpr auto   rbegin() const noexcept { return std::reverse_iterator(m_end); }
    constexpr auto   rend()   const noexcept { return std::reverse_iterator(m_begin); }

private:
    T* m_begin = nullptr;
    T* m_end = nullptr;
};

template <typename R>
Span(R& range) -> Span<std::remove_pointer_t<decltype(std::data(range))>>;

template <typename R>
Span(const R& range) -> Span<std::remove_pointer_t<decltype(std::data(range))>>;

namespace detail {

[[noreturn, gnu::noinline]]
void array_overflow();

[[gnu::noinline]]
void array_grow(void* opaque, size_t elem_info);

} // namespace detail

template <typename T>
class GCArray {
public:
    static_assert(std::is_trivially_copyable_v<T>);
    static_assert(alignof(T) <= BumpAllocator::MaxAlign);

    GCArray() = default;

    GCArray(GCArray&&) noexcept = default;
    GCArray& operator=(GCArray&&) noexcept = default;

    GCArray(const GCArray&) = delete;
    GCArray& operator=(const GCArray&) = delete;

    explicit constexpr GCArray(BumpAllocator& pool) noexcept : m_pool(&pool)
    {}

    explicit constexpr GCArray(size_t init_cap, BumpAllocator& pool)
        : m_pool(&pool)
    {
        reserve(init_cap);
    }

    void reset() noexcept
    {
        m_begin = m_end = m_cap = nullptr;
    }

    void reserve(size_t min_cap)
    {
        if (capacity() >= min_cap)
            return;

        constexpr auto MaxCap =
            std::numeric_limits<ptrdiff_t>::max() / SizeInfo;
        if (min_cap > MaxCap) [[unlikely]]
            detail::array_overflow();
        detail::array_grow(this, Align | (min_cap * SizeInfo));
    }

    template <typename... Args>
    T& emplace_back(Args&&... args)
    {
        if (m_end == m_cap) [[unlikely]]
            detail::array_grow(this, Align | (MinCap * SizeInfo));
        ::new (m_end) T(std::forward<Args>(args)...);
        return *m_end++;
    }

    T& push_back(const T& x)
    {
        return emplace_back(x);
    }

    T& push_back(T&& x)
    {
        return emplace_back(std::move(x));
    }

    void pop_back() noexcept
    {
        assert(!empty());
        --m_end;
    }

    T pop_back_value() noexcept
    {
        pop_back();
        return std::move(*m_end);
    }

    T& operator[](size_t i) noexcept
    {
        assert(i < size());
        return m_begin[i];
    }

    const T& operator[](size_t i) const noexcept
    {
        assert(i < size());
        return m_begin[i];
    }

    T& back(size_t i = 1) noexcept
    {
        assert(1 <= i && i <= size());
        return m_end[-i];
    }

    const T& back(size_t i = 1) const noexcept
    {
        assert(1 <= i && i <= size());
        return m_end[-i];
    }

    size_t   capacity() const noexcept { return m_cap - m_begin; }
    size_t   size()     const noexcept { return m_end - m_begin; }
    bool     empty()    const noexcept { return m_begin == m_end; }
          T* data()           noexcept { return m_begin; }
    const T* data()     const noexcept { return m_begin; }
          T* begin()          noexcept { return m_begin; }
    const T* begin()    const noexcept { return m_begin; }
    const T* cbegin()   const noexcept { return m_begin; }
          T* end()            noexcept { return m_end; }
    const T* end()      const noexcept { return m_end; }
    const T* cend()     const noexcept { return m_end; }
    auto     rbegin()         noexcept { return std::reverse_iterator(end()); }
    auto     rbegin()   const noexcept { return std::reverse_iterator(end()); }
    auto     crbegin()  const noexcept { return std::reverse_iterator(end()); }
    auto     rend()           noexcept { return std::reverse_iterator(begin()); }
    auto     rend()     const noexcept { return std::reverse_iterator(begin()); }
    auto     crend()    const noexcept { return std::reverse_iterator(begin()); }

private:
    T* m_begin = nullptr;
    T* m_end = nullptr;
    T* m_cap = nullptr;
    BumpAllocator* m_pool;

    static constexpr auto SizeInfo = sizeof(T) << 8;
    static constexpr auto Align = std::max<size_t>(alignof(T), 8);
    static constexpr auto MinCap = []() -> size_t {
        if (sizeof(T) == 1)
            return 16;
        if (sizeof(T) <= 8)
            return 8;
        return std::max<size_t>(64 / sizeof(T), 2);
    }();
};

template <typename... Args>
auto dup_string(BumpAllocator& pool, Args&&... args)
    -> decltype(std::string_view(std::forward<Args>(args)...))
{
    auto str = std::string_view(std::forward<Args>(args)...);
    auto* data = static_cast<char*>(pool.allocate(str.size(), 1));
    std::memcpy(data, str.data(), str.size());
    return std::string_view(data, str.size());
}

template <typename To, typename From>
struct copy_cv {
    using type = To;
};

template <typename To, typename From>
struct copy_cv<To, const From> {
    using type = const To;
};

template <typename To, typename From>
struct copy_cv<To, volatile From> {
    using type = volatile To;
};

template <typename To, typename From>
struct copy_cv<To, const volatile From> {
    using type = const volatile To;
};

template <typename To, typename From>
using copy_cv_t = typename copy_cv<To, From>::type;

template <typename To, typename From>
constexpr bool isa(From& from) noexcept
{
    if constexpr (std::is_base_of_v<To, From>)
        return true;
    else
        return To::classof(from);
}

template <typename To, typename From>
constexpr bool isa(From* from) noexcept
{
    assert(from != nullptr);
    return isa<To>(*from);
}

template <typename To, typename From>
constexpr auto cast(From& from) -> copy_cv_t<To, From>&
{
    assert(isa<To>(from));
    return static_cast<copy_cv_t<To, From>&>(from);
}

template <typename To, typename From>
constexpr auto cast(From* from) -> copy_cv_t<To, From>*
{
    assert(isa<To>(from));
    return static_cast<copy_cv_t<To, From>*>(from);
}

template <typename To, typename From>
constexpr auto dyn_cast(From* from) -> copy_cv_t<To, From>*
{
    if (!isa<To>(from))
        return nullptr;
    return static_cast<copy_cv_t<To, From>*>(from);
}

} // namespace minijava

[[nodiscard]]
inline void* operator new(size_t size, std::align_val_t alignment,
                          minijava::BumpAllocator& pool)
{
    return pool.allocate(size, size_t(alignment));
}

[[nodiscard]]
inline void* operator new(size_t size, minijava::BumpAllocator& pool)
{
    return pool.allocate(size, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}

#endif
