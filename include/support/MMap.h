#pragma once
#include "support/ArrayRef.h"
#include <cstddef>
#include <fcntl.h>
#include <string>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

class MMap {
  MutArrayRef<char> arr = MutArrayRef<char>::emptyRef();

public:
  MMap(const char *path) {
    int fd = open(path, O_RDONLY);
    if (fd < 0)
      return;
    struct stat st;
    if (fstat(fd, &st) < 0) {
      close(fd);
      return;
    }
    auto len = st.st_size + 1;
    auto *ptr = mmap(nullptr, len, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
    close(fd);
    if (ptr == MAP_FAILED)
      return;

    // null-terminate for string applications
    reinterpret_cast<char *>(ptr)[st.st_size] = 0;
    arr = MutArrayRef<char>{static_cast<char *>(ptr), size_t(len)};
  }
  ~MMap() {
    if (arr.size() != 0)
      munmap(static_cast<void *>(arr.data()), arr.size());
  }
  MMap(const std::string &path) : MMap(path.c_str()) {}

  size_t size() const { return arr.size(); }
  const char *data() const { return arr.data(); }
  operator ArrayRef<char>() const { return arr; }
  explicit operator bool() const { return size() != 0; }
};
