#include "dyno/Constant.h"
using namespace dyno;

void constants() {
  BigInt b = BigInt::fromU64(0x80000000, 130);
  b.setExtend(-1);

  std::cout << b << "\n";
  BigInt::shlOp(b, b, 2);
  std::cout << b << "\n";
  BigInt::lshrOp(b, b, 2);
  std::cout << b << "\n";
}

void constants2() {
  BigInt a = BigInt::fromU64(-1, 260);
  a.setExtend(-1);
  std::cout << a << "\n";
  BigInt b = BigInt::fromU64(-1, 32);
  b.setExtend(-1);
  std::cout << b << "\n";
  BigInt c = BigInt::mulOp<2>(a, b);
  std::cout << c << "\n";
}

void constants3() {
  BigInt a = BigInt::fromU64(-1, 64);
  a.setExtend(-1);
  std::cout << a << "\n";
  auto b = 10_b;

  std::cout << b << "\n";
  auto [q, r] = BigInt::udivmodOp(a, b);
  std::cout << q << "\n";
  std::cout << r << "\n";
}

void constants4() {
  BigInt a = BigInt::fromU64(4, 64);
  std::cout << a << "\n";
  BigInt b = BigInt::fromU64(2, 64);

  std::cout << b << "\n";
  auto [q, r] = BigInt::udivmodOp(a, b);
  std::cout << q << "\n";
  std::cout << r << "\n";
}

void constants5() {
  BigInt a = BigInt::fromU64(0xdeadbeefdeadbeef, 65);
  a.setExtend(-1);
  std::cout << std::dec << a << "\n";
}

void constants6() {
  BigInt a = BigInt::fromU64(0b01010101'11010101, 16);
  a.setCustom(true);

  BigInt b = BigInt::fromU64(0b01010101'00000000, 16);
  b.setCustom(true);
}

int main() {
  constants();
  constants2();
  constants3();
  constants4();
  constants5();
  constants6();
}
