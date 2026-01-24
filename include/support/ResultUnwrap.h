#pragma once

#define FWD_ERR(x)                                                             \
  do {                                                                         \
    if (auto res = x; !res)                                                    \
      return std::unexpected{res.error()};                                     \
  } while (false)

#define UNWRAP_INNER(out, in, tmp)                                             \
  auto tmp = (in);                                                             \
  if (!tmp)                                                                    \
    return std::unexpected{tmp.error()};                                       \
  auto &out = *tmp;

#define CONCAT_INNER(a, b) a##b
#define CONCAT(a, b) CONCAT_INNER(a, b)

#define UNWRAP(out, in) UNWRAP_INNER(out, in, CONCAT(_tmp_val, __LINE__))
