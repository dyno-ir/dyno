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
  MMap(const std::string &path) {
    int fd = open(path.c_str(), O_RDONLY);
    if (fd < 0)
      return;
    struct stat st;
    if (fstat(fd, &st) < 0) {
      close(fd);
      return;
    }
    auto len = st.st_size;
    auto *ptr = mmap(nullptr, len, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (ptr == MAP_FAILED)
      return;

    arr = MutArrayRef<char>{static_cast<char *>(ptr), size_t(len)};
  }
  ~MMap() {
    if (arr.size() != 0)
      munmap(static_cast<void *>(arr.data()), arr.size());
  }

  size_t size() const { return arr.size(); }
  const char *data() const { return arr.data(); }
  operator ArrayRef<char>() const { return arr; }
};
