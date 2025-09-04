#pragma once

#include <cstddef>
#define ASAN_POISON_MEMORY_REGION(addr, size) ((void)(addr), (void)(size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) ((void)(addr), (void)(size))

// todo: GCC asan
#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#undef ASAN_POISON_MEMORY_REGION
#undef ASAN_UNPOISON_MEMORY_REGION
extern "C" {
void __asan_poison_memory_region(void const volatile *addr, size_t size);
void __asan_unpoison_memory_region(void const volatile *addr, size_t size);
};
#define ASAN_POISON_MEMORY_REGION(addr, size)                                  \
  __asan_poison_memory_region(addr, size)
#define ASAN_UNPOISON_MEMORY_REGION(addr, size)                                \
  __asan_unpoison_memory_region(addr, size)
#endif
#endif
