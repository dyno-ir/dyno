#include "dyno/Constant.h"
#include "hw/HWConstant.h"
using namespace dyno;

void constants() {
  BigInt b(0x80000000, 130);
  b.setExtend(-1);

  std::cout << b << "\n";
  BigInt::shlOp(b, b, 2);
  std::cout << b << "\n";
  BigInt::lshrOp(b, b, 2);
  std::cout << b << "\n";
}

void constants2() {
  BigInt a(-1, 260);
  a.setExtend(-1);
  std::cout << a << "\n";
  BigInt b(-1, 32);
  b.setExtend(-1);
  std::cout << b << "\n";
  BigInt c = BigInt::mulOp<2>(a, b);
  std::cout << c << "\n";
}

void constants3() {
  BigInt a(-1, 64);
  a.setExtend(-1);
  std::cout << a << "\n";
  auto b = 10_b;

  std::cout << b << "\n";
  auto [q, r] = BigInt::udivmodOp(a, b);
  std::cout << q << "\n";
  std::cout << r << "\n";
}

void constants4() {
  BigInt a(4, 64);
  std::cout << a << "\n";
  BigInt b(2, 64);

  std::cout << b << "\n";
  auto [q, r] = BigInt::udivmodOp(a, b);
  std::cout << q << "\n";
  std::cout << r << "\n";
}

void constants5() {
  BigInt a(0xdeadbeefdeadbeef, 65);
  a.setExtend(-1);
  std::cout << std::dec << a << "\n";
}

void constants6() {
  HWBigInt a(0b01010101'11010101, 16);
  a.setCustom(true);

  HWBigInt b(0b01010101'00000000, 16);
  b.setCustom(true);

  HWBigInt::andOp(a, a, b);

  HWBigInt::stream_hex(std::cout, a);
  std::cout << "\n";
}

int main() {
  constants();
  constants2();
  constants3();
  constants4();
  constants5();
  constants6();
}
