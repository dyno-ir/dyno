#include <iostream>
#include <support/SmallVec.h>

struct ProbeObj {
  unsigned i;

  ProbeObj(unsigned i) : i(i) { std::cout << "Created " << i << "\n"; }

  ~ProbeObj() { std::cout << "Destroyed " << i << "\n"; }

  ProbeObj(const ProbeObj &o) {
    i = o.i;
    std::cout << "Copy Constr " << i << "\n";
  }
  ProbeObj(ProbeObj &&o) {
    i = o.i;
    std::cout << "Move Constr " << i << "\n";
  }
  ProbeObj &operator=(const ProbeObj &o) {
    std::cout << "Copy Assign " << i << "\n";
    i = o.i;
    return *this;
  }
  ProbeObj &operator=(ProbeObj &&o) {
    i = o.i;
    std::cout << "Move Assign " << i << "\n";
    return *this;
  }
};

int main() {
  SmallVec<ProbeObj, 4> vec;

  for (unsigned i = 0; i < 200; ++i) {
    vec.emplace_back(i);
  }
}
