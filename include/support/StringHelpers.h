#pragma once
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/SIMD.h"
#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>
#include <ostream>
#include <string>
#include <tuple>

#define FOR_ESCAPE_CHARS(x)                                                    \
  x("\n", "\\n") x("\r", "\\r") x("\t", "\\t") x("\b", "\\b") x("\f", "\\f")   \
      x("\"", "\\\"") x("\\", "\\\\")

class StringHelpers {
  using mask_t = vec_mask_unsigned_t<vec_u8_t>;

  static std::tuple<mask_t, mask_t, vec_u8_t>
  findAndClassifyEscapes(vec_u8_t data, mask_t valid) {
    mask_t common = 0;
    vec_u8_t escChar = vec_u8_t(0);
    auto add = [&](uint8_t c, uint8_t e) {
      mask_t mm = (data == vec_u8_t(c)).mask() & valid;
      common |= mm;
      escChar = vec_u8_t::select(mm, vec_u8_t(e), escChar);
    };
#define ADD_ESCAPE(unesc, esc)                                                 \
  add(static_cast<uint8_t>(unesc[0]), static_cast<uint8_t>(esc[1]));
    FOR_ESCAPE_CHARS(ADD_ESCAPE)
#undef ADD_ESCAPE
    mask_t control = (data < vec_u8_t(0x20)).mask() & valid & ~common;
    return {common, control, escChar};
  }

  template <size_t Stride>
  static size_t escapeChunk(std::string &out, size_t outSz, vec_u8_t data,
                            size_t chunkSz) {
    mask_t base = repeatBits(mask_t(1), static_cast<unsigned>(Stride)) &
                  bit_mask_ones<mask_t>(
                      static_cast<unsigned>(Stride * chunkSz));

    vec_u8_t expanded = vec_expand(data.raw(), base);
    auto [common, control, escChar] = findAndClassifyEscapes(expanded, base);

    mask_t escape = common | control;
    vec_u8_t outv = vec_u8_t::select(escape, vec_u8_t('\\'), expanded);
    vec_u8_t second = vec_shift_right<1>(escChar.raw());
    outv = vec_u8_t::select(common << 1, second, outv);

    if constexpr (Stride == 6) {
      auto hex = [](vec_u8_t byte) {
        vec_u8_t nib = byte & vec_u8_t(0xF);
        auto ge10 = nib >= vec_u8_t(10);
        return nib + vec_u8_t('0') +
               vec_u8_t::select(ge10.mask(), vec_u8_t('a' - '0' - 10),
                                vec_u8_t(0));
      };
      outv = vec_u8_t::select(control << 1, vec_u8_t('u'), outv);
      outv = vec_u8_t::select(mask_t((base << 2) | (base << 3)),
                              vec_u8_t('0'), outv);
      vec_u8_t shifted4 = vec_shift_right<4>(expanded.raw());
      vec_u8_t shifted5 = vec_shift_right<5>(expanded.raw());
      outv = vec_u8_t::select(base << 4, hex(shifted4 >> 4), outv);
      outv = vec_u8_t::select(base << 5, hex(shifted5), outv);
    }

    mask_t valid = base | (common << 1);
    if constexpr (Stride == 6)
      valid |= control * bit_mask_ones<mask_t>(Stride);

    size_t n = std::popcount(valid);
    if (outSz + n > out.size()) [[unlikely]]
      out.resize(outSz + n);
    vec_compress_store(reinterpret_cast<uint8_t *>(out.data() + outSz), valid,
                       outv.raw());
    return n;
  }

  static vec_u8_t hexVal(vec_u8_t v) {
    vec_u8_t lower = v | vec_u8_t('a' - 'A');
    vec_u8_t digit = v - vec_u8_t('0');
    vec_u8_t diff = lower - vec_u8_t('a');
    vec_u8_t alpha = diff + vec_u8_t(10) + (diff >> 7) * vec_u8_t(16);
    return vec_u8_t::select((digit <= vec_u8_t(9)).mask(), digit, alpha);
  }

  static mask_t isHex(vec_u8_t v) {
    return (hexVal(v) <= vec_u8_t(15)).mask();
  }

  static std::tuple<mask_t, vec_u8_t> commonEscape(vec_u8_t b1) {
    mask_t mask = 0;
    vec_u8_t chr = vec_u8_t('\\');
    auto add = [&](uint8_t c, uint8_t e) {
      mask_t m = (b1 == vec_u8_t(e)).mask();
      mask |= m;
      chr = vec_u8_t::select(m, vec_u8_t(c), chr);
    };
#define ADD_ESCAPE(unesc, esc)                                                 \
  add(static_cast<uint8_t>(unesc[0]), static_cast<uint8_t>(esc[1]));
    FOR_ESCAPE_CHARS(ADD_ESCAPE)
#undef ADD_ESCAPE
    return {mask, chr};
  }

public:
  template <typename MaskFn>
  static const char *find(const char *begin, const char *end, MaskFn maskFn) {
    for (const char *p = begin; p < end; p += SIMD_WIDTH) {
      auto rem = end - p;
      auto validMask =
          bit_mask_ones<mask_t>(std::min<size_t>(rem, bit_mask_sz<mask_t>));
      auto data =
          vec_u8_t::load(reinterpret_cast<const uint8_t *>(p), validMask, 0);
      mask_t invalid = ~maskFn(data) & validMask;
      size_t endIdx = std::countr_zero(invalid);
      if (endIdx < bit_mask_sz<mask_t>)
        return p + endIdx;
    }
    return end;
  }

  static size_t unescape(MutArrayRef<uint8_t> out, ArrayRef<char> data) {
    size_t outSz = 0;
    size_t pos = 0;
    size_t size = data.size();

    while (pos < size) {
      size_t chunkSz = std::min<size_t>(size - pos, SIMD_WIDTH);
      mask_t low = bit_mask_ones<mask_t>(chunkSz);
      vec_u8_t chunk = vec_u8_t::load(
          reinterpret_cast<const uint8_t *>(data.data() + pos), low, 0);

      mask_t starts = bit_run_parity<mask_t>(
          (chunk == vec_u8_t('\\')).mask() & low);
      if (starts == 0) {
        vec_masked_store(out.data() + outSz, low, chunk.raw());
        outSz += chunkSz;
        pos += chunkSz;
        continue;
      }
      size_t lastStart = std::bit_width(starts) - 1;
      if (lastStart > 0 && lastStart + 6 > chunkSz) {
        chunkSz = lastStart;
        low = bit_mask_ones<mask_t>(chunkSz);
        starts &= low;
      }

      std::array<vec_u8_t, 5> b;
      b[0] = vec_shift_left<1>(chunk.raw());
      for (size_t i = 1; i < b.size(); i++)
        b[i] = vec_shift_left<1>(b[i - 1].raw());

      auto [commonMask, commonDecoded] = commonEscape(b[0]);
      mask_t commonStart = commonMask & starts;
      mask_t commonCons = mask_t(commonStart << 1);

      vec_u8_t h4 = hexVal(b[3]);
      vec_u8_t h5 = hexVal(b[4]);
      mask_t uValid = starts & (b[0] == vec_u8_t('u')).mask() & isHex(b[1]) &
                     isHex(b[2]) & (h4 <= vec_u8_t(15)).mask() &
                     (h5 <= vec_u8_t(15)).mask();
      mask_t uCons = 0;
      for (unsigned k = 1; k <= 5; k++)
        uCons |= mask_t(uValid << k);

      vec_u8_t decoded = vec_u8_t('\\');
      decoded = vec_u8_t::select(commonStart, commonDecoded, decoded);
      decoded = vec_u8_t::select(uValid, (h4 << 4) | h5, decoded);
      vec_u8_t outv = vec_u8_t::select(starts, decoded, chunk);

      mask_t valid = low & ~(commonCons | uCons);
      size_t n = std::popcount(valid);
      vec_compress_store(out.data() + outSz, valid, outv.raw());
      outSz += n;
      pos += chunkSz;
    }

    return outSz;
  }

  static std::string escape(const char *data, size_t size) {
    std::string out;
    out.resize(size + size / 4);
    size_t outSz = 0;
    constexpr size_t FastPathChunkSz = SIMD_WIDTH / 2;
    constexpr size_t SlowPathChunkSz = SIMD_WIDTH / 6;

    for (size_t pos = 0; pos < size;) {
      size_t loaded = std::min<size_t>(size - pos, FastPathChunkSz);
      mask_t low = bit_mask_ones<mask_t>(loaded);
      auto chunk = vec_u8_t::load(
          reinterpret_cast<const uint8_t *>(data + pos), low, 0);

      auto [common, control, escChar] = findAndClassifyEscapes(chunk, low);

      if (common == 0 && control == 0) {
        if (outSz + loaded > out.size()) [[unlikely]]
          out.resize(outSz + loaded);
        vec_masked_store(reinterpret_cast<uint8_t *>(out.data() + outSz), low,
                         chunk.raw());
        outSz += loaded;
        pos += loaded;
        continue;
      }

      size_t firstControl = static_cast<size_t>(std::countr_zero(control));
      size_t chunkSz;
      if (firstControl < SlowPathChunkSz) {
        chunkSz = std::min<size_t>(loaded, SlowPathChunkSz);
        outSz += escapeChunk<6>(out, outSz, chunk, chunkSz);
      } else {
        chunkSz = std::min<size_t>(loaded, firstControl);
        outSz += escapeChunk<2>(out, outSz, chunk, chunkSz);
      }
      pos += chunkSz;
    }
    out.resize(outSz);
    return out;
  }

  static void escape(std::ostream &os, const char *data, size_t size) {
    std::string out = escape(data, size);
    os.write(out.data(), out.size());
  }

  static std::string unescape(ArrayRef<char> data) {
    std::string out(data.size(), '\0');
    size_t n = unescape(
        MutArrayRef<uint8_t>(reinterpret_cast<uint8_t *>(out.data()),
                             out.size()),
        data);
    out.resize(n);
    return out;
  }

  static void unescape(std::string &s) {
    size_t n = unescape(
        MutArrayRef<uint8_t>(reinterpret_cast<uint8_t *>(s.data()), s.size()),
        ArrayRef<char>(s.data(), s.size()));
    s.resize(n);
  }

  static void unescape(std::ostream &os, ArrayRef<char> data) {
    std::string out = unescape(data);
    os.write(out.data(), out.size());
  }
};

#undef FOR_ESCAPE_CHARS
