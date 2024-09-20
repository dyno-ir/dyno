#pragma once

#include "ir/IRPass.h"
#include "ir/IRPrinter.h"
#include "ir/IRBuilder.h"
#include "support/DynBitSet.h"
#include "support/PackedSet.h"
#include <cassert>
#include <compare>
#include <iostream>
#include <ranges>
#include <unordered_set>

class BlockLiveFlow {
public:
  std::unordered_set<Reg> liveIn;
  std::unordered_set<Reg> liveOut;
  std::vector<Reg> liveOutNew;
  std::unordered_set<Reg> defs;
};

class LiveFlow {
public:
  static const IRInfoID ID;
  std::unordered_map<CFGBlock *, BlockLiveFlow> blockLive;
};

class LiveFlowPass : public IRPass<Function> {
public:
  const char *name() override { return "LiveFlowPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    data.blockLive.clear();
    arch = &info.getArch();
    regInfo = &func.getRegInfo();

    std::vector<CFGBlock *> workList;
    for (auto &block : func) {
      precomputeBlock(block);
      workList.push_back(&block);
    }
    while (!workList.empty()) {
      auto &block = *workList.back();
      workList.pop_back();
      dataflowStep(block, workList);
    }
    info.publish(data);
    info.preserveAll();
  }

  void precomputeBlock(CFGBlock &block) {
    std::unordered_set<Reg> regExposed;
    auto &blockLive = data.blockLive[&block];
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi()) {
        auto phi = PhiInstrPtr(&instr);
        for (int i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &predBlockLive = data.blockLive[&phi.getPredecessorBlock(i)];
          predBlockLive.liveOutNew.push_back(phi.getPredecessorDef(i).reg());
        }
        Reg reg = instr.getDef().reg();
        blockLive.liveIn.insert(reg);
        regExposed.erase(reg);
        continue;
      }

      for (auto &op : instr) {
        if (!op.isReg())
          continue;
        Reg reg = op.reg();
        if (op.isRegDef()) {
          if (ignoreReg(*arch, reg))
            continue;
          blockLive.defs.insert(reg);
          regExposed.erase(reg);
        } else {
          if (ignoreReg(*arch, reg))
            continue;
          regExposed.insert(reg);
        }
      }
    }

    blockLive.liveIn.insert(regExposed.begin(), regExposed.end());
    for (auto &blockUse : block.operand().ssaDef()) {
      CFGBlock &pred = blockUse.getParentBlock();
      auto &predBlockLive = data.blockLive[&pred];
      predBlockLive.liveOutNew.insert(predBlockLive.liveOutNew.end(),
                                      regExposed.begin(), regExposed.end());
    }
  }

  static bool ignoreReg(Arch &arch, Reg reg) {
    if (reg.isVReg())
      return false;
    const ArchReg *archReg = arch.getArchReg(reg);
    return archReg ? archReg->noLiveness : false;
  }

  void dataflowStep(CFGBlock &block, std::vector<CFGBlock *> &workList) {
    auto &blockLive = data.blockLive[&block];
    for (auto op : blockLive.liveOutNew) {
      if (!blockLive.liveOut.insert(op).second) {
        continue;
      }
      if (blockLive.defs.find(op) != blockLive.defs.end()) {
        continue;
      }
      if (!blockLive.liveIn.insert(op).second) {
        continue;
      }
      for (auto &blockUse : block.operand().ssaDef()) {
        CFGBlock &pred = blockUse.getParentBlock();
        auto &predBlockLive = data.blockLive[&pred];
        predBlockLive.liveOutNew.push_back(op);
        workList.push_back(&pred);
      }
    }
    blockLive.liveOutNew.clear();
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveFlow>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveFlow>();
    data.blockLive.clear();
  }

private:
  LiveFlow data;
  Arch *arch;
  RegInfo *regInfo;
};

class PrintLiveFlowPass : public IRPass<Function> {
  const char *name() { return "PrintLiveFlowPass"; }

  void run(Function &func, IRInfo<Function> &info) {
    auto regLive = info.query<LiveFlow>();
    auto printer = info.query<PrintIRVisitor>();
    std::cout << "-- LivenessFlow --\n";
    for (auto &block : func) {
      auto blockLive = regLive.blockLive[&block];
      printer.printNumberedDef(block.operand());
      std::cout << ":\n";

      std::cout << "LiveIn:";
      print(blockLive.liveIn, printer);
      std::cout << "LiveOut:";
      print(blockLive.liveOut, printer);
    }
    std::cout << "----\n";
    info.preserveAll();
  }

  void print(std::unordered_set<Reg> regs, PrintIRVisitor &printer) {
    for (auto x : regs) {
      std::cout << " ";
      printer.printReg(x);
    }
    std::cout << "\n";
  }
};

class LivePos {
public:
  enum Slot {
    EARLY,
    BASE,
    LATE,
  };
  static constexpr const char *slotName(Slot s) {
    switch (s) {
    case EARLY:
      return "e";
    case BASE:
      return "b";
    case LATE:
      return "l";
    }
    return nullptr;
  }

  constexpr LivePos(size_t num, Slot slot = BASE) : num(num), slot(slot) {}
  size_t num;
  Slot slot;

  friend std::strong_ordering operator<=>(LivePos a, LivePos b) {
    auto numCmp = a.num <=> b.num;
    if (numCmp == numCmp.equal) {
      return a.slot <=> b.slot;
    }
    return a.num <=> b.num;
  }
  friend bool operator==(LivePos a, LivePos b) { return a.num == b.num; }
};

class LiveInterval;
class LiveRange;

template<>
class IntrusiveListTraits<LiveInterval> {
public:
  using DerivedINode = LiveRange;
  static constexpr bool OwnsNodes = true;
};

class LiveRange : public IntrusiveListNode<LiveInterval> {
public:
  LiveRange(LivePos from, LivePos to) : from(from), to(to) {}
  LivePos from;
  LivePos to;
};

class LiveInterval : public IntrusiveList<LiveInterval> {
public:
  void addRangeCoalesceBegin(LivePos from, LivePos to) {
    assert(from < to);
    if (!empty()) {
      if (to == getFirst().from) {
        getFirst().from = from;
        return;
      }
      assert(to < getFirst().from);
    }
    insertBegin(*new LiveRange(from, to));
  }

  void updateFromBegin(LivePos from) {
    assert(!empty());
    assert(from > getFirst().from);
    getFirst().from = from;
  }
};

class LiveIntervals {
public:
  static const IRInfoID ID;
  std::unordered_map<void *, size_t> numbering;
  size_t currNum = 0;
  std::unordered_map<Reg, LiveInterval> intervals;

  void number(void *ptr) {
    currNum += 10;
    numbering[ptr] = currNum;
  }

  size_t getNum(void *ptr) {
    auto it = numbering.find(ptr);
    assert(it != numbering.end());
    return it->second;
  }

  size_t getNumInstr(Instr &instr) { return getNum(&instr); }
  size_t getNumLiveIn(CFGBlock &block) { return getNum(&block.getSentryBegin()); }
  size_t getNumLiveOut(CFGBlock &block) {
    return getNum(block.hasNext() ? &block.getNext().getSentryBegin()
                                  : &block.getSentryEnd());
  }
};

class LiveIntervalsPass : public IRPass<Function> {
  const char *name() override { return "LiveIntervalsPass"; }
  void run(Function &func, IRInfo<Function> &info) override {
    arch = &info.getArch();
    intervals = LiveIntervals();
    flow = &info.query<LiveFlow>();
    numberInstructions(func);
    buildIntervals(func);
    info.publish(intervals);
    info.preserveAll();
  }

  void numberInstructions(Function &func) {
    for (auto &block : func) {
      intervals.number(&block.getSentryBegin());
      for (auto &instr : block) {
        intervals.number(&instr);
      }
      intervals.number(&block.getSentryEnd());
    }
  }

  void buildIntervals(Function &func) {
    for (auto &block : func | std::views::reverse) {
      auto &blockLive = flow->blockLive[&block];
      auto live = blockLive.liveOut;
      auto blockIn = intervals.getNumLiveIn(block);
      auto blockOut = intervals.getNumLiveOut(block);
      for (auto reg : live) {
        intervals.intervals[reg].addRangeCoalesceBegin(blockIn, blockOut);
      }
      for (auto &instr : block | std::views::reverse) {
        if (instr.isPhi())
          continue;
        for (auto &op : instr) {
          if (!op.isReg())
            continue;
          Reg reg = op.reg();
          if (op.isRegDef()) {
            if (LiveFlowPass::ignoreReg(*arch, reg))
              continue;
            defReg(reg, instr, live);
          } else {
            if (LiveFlowPass::ignoreReg(*arch, reg))
              continue;
            useReg(reg, instr, live);
          }
        }
      }
    }
  }

  void useReg(Reg reg, Instr &instr, std::unordered_set<Reg> &live) {
    auto blockIn = intervals.getNumLiveIn(instr.getParent());
    auto instrNum = intervals.getNumInstr(instr);
    if (live.insert(reg).second) {
      intervals.intervals[reg].addRangeCoalesceBegin(blockIn, instrNum);
    }
  }

  void defReg(Reg reg, Instr &instr, std::unordered_set<Reg> &live) {
    auto instrNum = intervals.getNumInstr(instr);
    if (live.erase(reg)) {
      intervals.intervals[reg].updateFromBegin(instrNum);
    } else {
      intervals.intervals[reg].addRangeCoalesceBegin(instrNum,
                                                     {instrNum, LivePos::LATE});
    }
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveIntervals>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveIntervals>();
  }

  LiveFlow *flow;
  Arch *arch;
  LiveIntervals intervals;
};

class PrintLiveIntervalsPass : public IRPass<Function> {
  const char *name() override { return "PrintLiveIntervalsPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    LiveIntervals &live = info.query<LiveIntervals>();
    PrintIRVisitor printer(info.getArchUnchecked());
    printer.setPreInstrCallback(
        [&](Instr &i) { std::cout << live.getNumInstr(i) << ": "; });
    printer.dispatch(func);
    std::cout << "-- LiveIntervals --\n";
    for (auto &[reg, interval] : live.intervals) {
      printer.printReg(reg);
      std::cout << ":";
      printInterval(interval);
      std::cout << "\n";
    }
    std::cout << "----\n";
    info.preserveAll();
  }

  void printInterval(LiveInterval &interval) {
    for (auto &r : interval) {
      std::cout << " ";
      printRange(r);
    }
  }

  void printRange(LiveRange &r) {
    std::cout << "[";
    printPos(r.from);
    std::cout << ";";
    printPos(r.to);
    std::cout << ")";
  }

  void printPos(LivePos pos) {
    std::cout << pos.num << LivePos::slotName(pos.slot);
  }
};

class PhysLiveSet {
public:
  PhysLiveSet() {}
  PhysLiveSet(size_t sz) : bits(sz + 1) {}

  void insert(Reg reg) {
    assert(reg.isPhysReg());
    bits.set(reg.getNum());
  }

  void insert(const PhysLiveSet &o) { bits |= o.bits; }

  void erase(Reg reg) {
    assert(reg.isPhysReg());
    bits.clr(reg.getNum());
  }

  bool contains(Reg reg) const {
    assert(reg.isPhysReg());
    return bits.tst(reg.getNum());
  }

  size_t size() { return bits.count(); }

  void clear() { bits.clrAll(); }

  DynBitSet bits;
};

class VLiveSet {
  using IdxSet = PackedSet<Reg::num_t>;

public:
  VLiveSet() {}
  VLiveSet(size_t numVRegs) : idxSet(numVRegs) {}

  bool insert(Reg reg) {
    assert(reg.isVReg());
    return idxSet.insert(reg.getIdx());
  }

  bool erase(Reg reg) {
    assert(reg.isVReg());
    return idxSet.erase(reg.getIdx());
  }

  bool contains(Reg reg) const {
    assert(reg.isVReg());
    return idxSet.contains(reg.getIdx());
  }

  size_t size() const { return idxSet.size(); }

  bool empty() const { return idxSet.empty(); }

  std::vector<Reg> toVector() {
    std::vector<Reg> res;
    for (auto reg : *this) {
      res.push_back(reg);
    }
    return res;
  }

  template <std::forward_iterator It> void insert(It begin, It end) {
    for (auto it = begin; it != end; ++it) {
      insert(*it);
    }
  }

  void clear() { idxSet.clear(); }

  class iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = Reg;
    using difference_type = std::ptrdiff_t;

    iterator() {}
    iterator(IdxSet::iterator it) : it(it) {}

    Reg operator*() const { return Reg::vReg(*it); }

    iterator &operator++() {
      ++it;
      return *this;
    }

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.it == b.it;
    }

  private:
    IdxSet::iterator it;
  };
  static_assert(std::forward_iterator<iterator>);

  iterator begin() const { return idxSet.begin(); }
  iterator end() const { return idxSet.end(); }

private:
  IdxSet idxSet;
};

class LiveSet {
public:
  LiveSet(size_t numVRegs, size_t numPhysRegs)
      : virt(numVRegs), phys(numPhysRegs) {}

  void insert(Reg reg) {
    if (reg.isPhysReg()) [[unlikely]] {
      phys.insert(reg);
      return;
    }
    virt.insert(reg);
  }

  void erase(Reg reg) {
    if (reg.isPhysReg()) [[unlikely]] {
      phys.erase(reg);
      return;
    }
    virt.erase(reg);
  }

  bool contains(Reg reg) {
    if (reg.isPhysReg()) [[unlikely]] {
      return phys.contains(reg);
    }
    return virt.contains(reg);
  }

  void clear() {
    virt.clear();
    phys.clear();
  }

  template <std::forward_iterator It> void insert(It begin, It end) {
    for (auto it = begin; it != end; ++it) {
      insert(*it);
    }
  }

  VLiveSet virt;
  PhysLiveSet phys;
};

class LiveGraph {
  friend class LiveGraphPass;
  friend class PrintLiveGraphPass;

public:
  static const IRInfoID ID;

  struct VLive {
    friend class LiveGraph;

    VLive(size_t numPhysRegs) : phys(numPhysRegs) {}
    PhysLiveSet phys;
    std::vector<Reg> virt;
    std::vector<Reg> coloringHints;
    bool ignore = true;
    size_t degree;
  };

  LiveGraph(size_t numVRegs, size_t numPhysRegs)
      : numVRegs(numVRegs), numPhysRegs(numPhysRegs), adjVLive(numVRegs) {
    vRegs.data.reserve(numVRegs);
    for (size_t i = 0; i < numVRegs; ++i) {
      vRegs.data.emplace_back(numPhysRegs);
    }
  }

  void addEdge(Reg r1, Reg r2) {
    if (r1.isVReg() && r2.isVReg()) {
      addVEdge(r1, r2);
    } else if (r1.isVReg() && r2.isPhysReg()) {
      addPhysEdge(r1, r2);
    } else if (r2.isVReg() && r1.isPhysReg()) {
      addPhysEdge(r2, r1);
    }
  }

  void addEdges(Reg reg, LiveSet &live) {
    if (reg.isPhysReg()) {
      for (auto oReg : live.virt) {
        addPhysEdge(oReg, reg);
      }
    } else {
      vRegs[reg].phys.bits |= live.phys.bits;
      for (auto oReg : live.virt) {
        addVEdge(reg, oReg);
      }
    }
  }

  void addVEdge(Reg v1, Reg v2) {
    assert(v1.isVReg() && v2.isVReg());
    if (adjVLive.tst(v1.getIdx(), v2.getIdx())) {
      return;
    }
    adjVLive.set(v1.getIdx(), v2.getIdx());
    auto &v1Live = vRegs[v1];
    auto &v2Live = vRegs[v2];
    v1Live.virt.push_back(v2);
    v2Live.virt.push_back(v1);
  }

  void addPhysEdge(Reg v, Reg phys) {
    assert(v.isVReg() && phys.isPhysReg());
    vRegs[v].phys.insert(phys);
  }

  VRegMap<VLive> vRegs;

  size_t getNumVRegs() { return numVRegs; }
  size_t getNumPhysRegs() { return numPhysRegs; }

  class neighbor_iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = std::pair<Reg, VLive &>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;
    using It = std::vector<Reg>::iterator;

    neighbor_iterator() {}
    neighbor_iterator(LiveGraph &graph, It it) : graph(&graph), it(it) {}

    value_type operator*() const { return {*it, graph->vRegs[*it]}; }

    neighbor_iterator &operator++() {
      ++it;
      return *this;
    }

    neighbor_iterator operator++(int) {
      neighbor_iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    friend bool operator==(const neighbor_iterator &a,
                           const neighbor_iterator &b) {
      return a.it == b.it;
    }

  private:
    LiveGraph *graph;
    It it;
  };
  static_assert(std::forward_iterator<neighbor_iterator>);

  auto neighbors(Reg reg) {
    auto &v = vRegs[reg];
    return Range{neighbor_iterator(*this, v.virt.begin()),
                 neighbor_iterator(*this, v.virt.end())};
  }

  void addColoringHint(Reg r1, Reg r2) {
    if (r1.isVReg()) {
      vRegs[r1].coloringHints.push_back(r2);
    }
    if (r2.isVReg()) {
      vRegs[r2].coloringHints.push_back(r1);
    }
  }

  void ignore(Reg reg) {
    auto &v = vRegs[reg];
    assert(!v.ignore);
    v.ignore = true;
    for (auto [oReg, oLive] : neighbors(reg)) {
      assert(oLive.degree > 0);
      --oLive.degree;
    }
  }

  void resetIgnore() {
    for (auto [reg, v] : vRegs) {
      v.ignore = false;
      v.degree = v.phys.size() + v.virt.size();
    }
  }

private:
  size_t numVRegs;
  size_t numPhysRegs;

  TriangularBitSet adjVLive;
};

class LiveGraphPass : public IRPass<Function> {
  const char *name() override { return "LiveGraphPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    arch = &info.getArch();
    flow = &info.query<LiveFlow>();
    size_t numVRegs = func.getRegInfo().getNumVRegs();
    size_t numPhysRegs = arch->getArchRegs().size();
    graph = std::make_unique<LiveGraph>(numVRegs, numPhysRegs);
    LiveSet live(numVRegs, numPhysRegs);
    for (auto &block : func) {
      buildGraphForBlock(block, live);
    }
    info.publish(*graph);
    info.preserveAll();
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveGraph>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveGraph>();
    graph = nullptr;
  }

  void buildGraphForBlock(CFGBlock &block, LiveSet &live) {
    auto &blockLive = flow->blockLive[&block];
    live.clear();
    live.insert(blockLive.liveOut.begin(), blockLive.liveOut.end());
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi())
        continue;
      for (auto &op : instr) {
        if (!op.isReg())
          continue;
        Reg reg = op.reg();
        if (op.isRegDef()) {
          if (LiveFlowPass::ignoreReg(*arch, reg))
            continue;
          // FIXME: should multiple defs always interfere with each other?
          live.erase(reg);
          graph->addEdges(reg, live);
        } else {
          if (LiveFlowPass::ignoreReg(*arch, reg))
            continue;
          live.insert(reg);
        }
      }
      if (instr.isCopy()) {
        graph->addColoringHint(instr.getOperand(0).reg(),
                               instr.getOperand(1).reg());
      }
    }
  }

private:
  std::unique_ptr<LiveGraph> graph;
  LiveFlow *flow;
  Arch *arch;
};

class PrintLiveGraphPass : public IRPass<Function> {
  const char *name() override { return "PrintLiveGraphPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    LiveGraph &graph = info.query<LiveGraph>();
    printer = &info.query<PrintIRVisitor>();

    for (size_t i = 0; i < graph.numVRegs; ++i) {
      std::cout << i << ":\n";
      printVLive(graph.vRegs[i]);
    }

    info.preserveAll();
  }

  void printVLive(LiveGraph::VLive v) {
    std::cout << "  Phys:";
    v.phys.bits.for_each([this](auto i) {
      std::cout << " ";
      printer->printReg(i);
    });
    std::cout << "\n";
    std::cout << "  Virt:";
    for (auto i : v.virt) {
      std::cout << " ";
      printer->printReg(Reg::vReg(i));
    }
    std::cout << "\n";
  }

  PrintIRVisitor *printer;
};
