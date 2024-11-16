#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>

template <typename T> class InlineStorageRef;

template <size_t Size, size_t Align = std::min(alignof(std::max_align_t), Size)>
class InlineStorage {
private:
  template <typename T> friend class InlineStorageRef;

  alignas(Align) std::byte buf[Size];

public:
  template <typename T> InlineStorageRef<T> as() {
    return InlineStorageRef<T>{*this};
  }
};

template <typename T> class InlineStorageRef {
  std::byte *buf;

public:
  InlineStorageRef(InlineStorage<sizeof(T), alignof(T)> &storage)
      : buf(storage.buf) {}
  template <typename... Args> T &emplace(Args &&...args) {
    return *std::construct_at<T>(reinterpret_cast<T *>(buf),
                                 std::forward<Args>(args)...);
  }

  void destroy() { (*this)->~T(); }

  T &operator*() { return *std::launder(reinterpret_cast<T *>(buf)); }
  T *operator->() { return std::launder(reinterpret_cast<T *>(buf)); }
};

template <typename T, size_t N> class InlineStorageArr {
  // static_assert((T *)0 + 1 == (decltype(storage) *)0 + 1);
public:
  InlineStorage<sizeof(T), alignof(T)> storage[N];

  InlineStorageRef<T> operator[](size_t pos) {
    assert(pos < N);
    return InlineStorageRef{storage[pos]};
  }

  T *operator*() { return std::launder(reinterpret_cast<T *>(storage)); }
};
