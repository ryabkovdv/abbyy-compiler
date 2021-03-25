#include <minijava/utility.hpp>

#include <stdexcept>

using namespace minijava;

void* BumpAllocator::allocate_block(size_t size, size_t alignment)
{
    size_t header = std::max(alignment, sizeof(Block));
    size_t block_size = (size < 2048) ? 32768 : size + header;

    auto* ptr = static_cast<std::byte*>(std::malloc(block_size));
    if (ptr == nullptr) [[unlikely]]
        throw std::bad_alloc();

    auto align_down = [](std::byte* ptr, size_t alignment) {
        auto uptr = reinterpret_cast<uintptr_t>(ptr);
        auto aligned = uptr & -alignment;
        return ptr + (aligned - uptr);
    };

    auto* aligned = align_down(ptr + header, alignment);
    if (size < 2048) {
        m_base = align_down(ptr + block_size, MaxAlign);
        m_space = m_base - (aligned + size);
    }
    m_blocks = ::new (ptr) Block{m_blocks};
    return aligned;
}

void detail::array_overflow()
{
    throw std::length_error("array capacity overflow");
}

void detail::array_grow(void* opaque, size_t elem_info)
{
    struct OpaqueArray {
        std::byte* m_begin;
        std::byte* m_end;
        std::byte* m_cap;
        BumpAllocator* m_pool;
    };

    auto* array = static_cast<OpaqueArray*>(opaque);
    auto* pool = array->m_pool;
    auto* old_data = array->m_begin;
    size_t old_size = array->m_end - old_data;
    size_t old_cap = array->m_cap - old_data;

    size_t align = elem_info & 0xFF;
    size_t min_cap = elem_info >> 8;

    size_t new_cap = std::max(2 * old_cap, min_cap);
    if (ptrdiff_t(new_cap) < 0) [[unlikely]]
        detail::array_overflow();

    if (pool->try_resize(old_data, old_cap, new_cap)) {
        array->m_cap = old_data + new_cap;
        return;
    }

    array->m_begin = static_cast<std::byte*>(pool->allocate(new_cap, align));
    array->m_end = array->m_begin + old_size;
    array->m_cap = array->m_begin + new_cap;
    std::memcpy(array->m_begin, old_data, old_size);
}
