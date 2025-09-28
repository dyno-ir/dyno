# Dyno-IR: Designing high-performance compiler IRs using dynamic objects

Goal: high-performance, multi-purpose compiler infrastructure

Many different IRs needed:

- Traditional SW compilers: AST, Middle-end IR (pure SSA), Back-end IR
  (register-based SSA, assembly)
- New-age SW compilers: high-level execution graphs, tensors
- Traditional HW synthesis tools: AST, Netlist with buses, Gate-level netlist,
  AIGs, BDDs
- New-age HW compilers: HLS, Chisel/FIRRTL, high-level state machine and memory
  optimizations
- Formal solvers: SMT constraints (smt-lib), SAT clauses

Every IR needs:

- Memory management (arena, pool, ...)
- Printing and Parsing (textual, bitstream)
- Rewriting system
  - Structural hashing
  - Canonicalizations
  - Peephole optimizations
  - Mutation tracking, Continuous Analysis maintenance
  - Lowering/lifting to/from other IRs
- Pass management
  - Pipelined execution
  - Analysis caching
- Source location tracking
- DSL: instruction definitions, rewrite rules, ...

MLIR solution: Represent everything using a single IR data-structure
(Operation). Extensibility through run-time dynamic dispatch. If
high-performance data-structures are need, can't use existing infrastructure,
would need to translate MLIR to internal solver/optimization representation.
Suitable for high-level transformation, but struggles with high-volume low-level
rewriting tasks: e.g. constant prop, instruction selection. Unsuitable for
implementing NP-complete solvers: e.g. AIGs, BDDs, SAT.

Dyno-IR: Kitchen-sink compiler framework, but don't pay for what you don't use.

- Create highly tuned custom data-structures, but make sure they interact well
  with the rest of the framework
- Ensure seamless conversion between different IR containers by building
  everything on the same underlying concepts
- Aggressively split data-structures and user-visible APIs
- Data driven design:
  - minimize node sizes, maximize cache locality
  - minimize dynamic dispatch, encourage batching
  - store side-information without hash-tables

## Dyno Objects

IR design has 2 main forces:

- static data-structures (where we know the IR types at compile-time): not
  extensible, custom APIs, fast
- dynamic data-structures (where we need run-time information about the types):
  easy to abstract, enables run-time extensibility, slow

We often need both representations:

- a "dynamic" version with good generic infrastructure (prototyping, don't care
  about performance like printing/parsing, high-level constructs)
- a "static" version for high-performance (AIG, SAT clauses, high-volume Instrs
  like BinOps). However, both representations share the need for referencing
  other IR objects, which may or may not be dynamic.

### Object References

Fundamental reference types:

- ID (dense/sparse) (type decides how this is interpreted)
- Pointer
- custom bits, inline storage

Dyno provides these fundamental reference types in a type-safe static version
and a dynamic version:

- `ObjRef<T>`, `FatObjRef<T>`
- `DynObjRef`, `FatDynObjRef`

The user decides how to reference their objects per type.

No information must be lost when converting between different reference
representations. Any lost information must be restorable through context lookup
(e.g. ID -> Ptr). FatDynObjRef must be able to represent everything. This
ensures seamless conversion between representations.

Different ways to construct and reference dyno Objects:

```cpp
// Object doesn't know its own ID
class SomeObj {
    // ... data
};

// Object knows its own ID
class SomeObj {
    ObjRef<SomeObj>;
};

// PtrRef
class SomeObjRef {
SomeObj* ptr;
};
// ThinRef
class SomeObjRef : public ObjRef<SomeObj> {

};

```

#### Dialects and Types

8-bit dialect id. 8-bit type id. In the future these may be combined into a
single id space by DSL.

#### Canonical Numberings

By definition: A canonical number is a unique identifier of an IR construct in a
certain scope that also maintains a domain specific invariant (e.g.
instruction-order/dominance inside a block)

We usually want different canonical numberings during different parts of the
pipeline. Often we also need multiple at the same time, and we don't want to
make one of them a second class citizen in the infra (extensibility, future
proofing for when we decide that our current canonical numbering doesn't cut it)

Needs to be explicitly maintained on all transformations to ensure that the
invariant holds -> Usually requires full renumbering after certain
transformations (or lazy updates after a certain number of smaller
transformations to guarantee amortized performance)

Correct number maintenance is vital for correctness of transformations. We want
to be able to detect instructions that haven't been renumbered (either for lazy
renumbering or verifying if we failed to assign a number to a new instruction to
detect compiler renumbering bugs)

Renumbering is not always reasonably possible in generic IR infrastructure,
because many different parts of generic and user infrastructure need to maintain
additional state. We can't make every pass aware of all this state that needs to
be updated. Band-aid solution for this is hooks, but they don’t always cut it in
every situation (often there is more context needed to update the numbering
efficiently) even though making mutation illegal (except replaceAllUses) makes
this a lot more feasible, and lazy BlockUpdate/InstrUpdate-style API also makes
this a lot nicer (all this infra is still needed for maintaining canonical
numberings in a side-table, just still not general enough imo). Updating the
numbering can also be very expensive, so we need to be able to do it lazily,
which doesnt really work if the canonical number is our only unique identifier.

Thus: We will always need a context-level unique identifier that doesnt change
throughout the lifetime of an object.

Pointer is ill-suited as the canonical identifier, because it's too large for
compressed IR (like AIG), we want fast map lookup, and we want objects that dont
even have per-object storage (struct of arrays object, or inline-only object).

ObjID as unique identifier, ObjMap to provide derived (canonical) numberings in
certain parts of the pipeline and with a smaller scope (e.g. per-block) (or
custom storage fields in various places once determined that one canonical
numbering is so important in a specific part of the pipeline that we gain
performance (unlikely imo))

#### Structural Hashing

The ObjRefs referencing other objects in a complex object must/should be
identify the object uniquely. Structural hash of object is thus hash over all
ObjRefs in the object.

#### Case study: Constants (custom inline storage vs. large out-of-inline)

TODO

### Object Stores

ObjStore is a concept that provides the backing storage for Dyno objects. It
handles ID and memory allocation. Usually keeps id static over the lifetime of
an object (see discussion on canonical numberings), but not necessary.

- NewDelete, Pool, ...
- Trailing Objects
- Ability to switch out allocators:
  - Debug: NewDelete ASAN
  - Release: optimized arena/pool

Generally to dereference an ID, a pointer to the object store is required. We
want to avoid tying ID namespaces to the global program state, but in some cases
it might make sense to have a thread_local context. We can then have a different
version of e.g. InstrRef

### Derived References

Goals:

- Decouple underlying datastructure from access API.
- Enable fundamental changes of underlying datastructures without changing the
  API.
- Extensible API (e.g. most code should not be using `operand(i)` directly,
  instead define wrappers on derived ref e.g. `getOutput()`)

E.g. during first prototype, the obj ref may be a pointer, and the id is stored
in the object. Later on we might want to use the ID as the reference. Derived
references document in code which invariants hold for an instruction. All
user-facing operations are defined on the DerivedRef. Dereferencing the Ref to
the underlying object allows you to access the internal unstable API.
DerivedRefs offer good IDE auto-complete.

See e.g. InstrRef. We might want to make InstRefs ID-only. Possible impl:

```cpp
template<typename Derived>
class InstrRefBase {
    // Main InstrRef API relying on Derived::operator*() and Derived::getObjID();
};

using InstrResolver = ObjMap<Instr,Instr*>;
thread_local InstrResolver instrResolver;

class InstrTRef : public ObjRef<Instr>, InstrRefBase<InstrTRef> {
    InstrResolver* resolver;

    // someRef.as<IsntrTRef>(resolver);
    InstrTRef(InstrResolver& resolver, ...) {

    }

    Instr& operator *() {
        return *resolver[obj];
    }
};

class InstrGTRef : public ObjRef<Instr>, InstRefBase<InstrGRef> {
    Instr* operator *() {
        // use thread_local context
    }
};
```

The pass manger should take care of installing/uninstalling the resolver for the
GRef.

These derived references should only be viewed as access objects, they should
not be stored into any datastructures. Use one of ObjRef<T>/T*/... for storage.
We can have a helper function that converts ObjRef<T>.as<>() and as<>(T*)
default to the reference types specified in ObjTraits.

All Instr-like reference types (Instr, compressed Instr, AIGNode) should provide
a minimal uniform interface of:

```cpp
class SomeInstrLikeRef {
    // Even if opcode is alwys the same, return a static value.
    Opcode getOpcode();

    SomeInstrLikeRef operandDef(unsigned n);
};
```

This interface should be used e.g. by Rewriter.

```cpp
template<IsInstrLikeRef RefT>
class Rewriter {

    bool apply() {}

}
```

### Interfaces

Generic passes require ways to deal with custom objects that it doesn't know
about, so unfortunately we need some form of dynamic dispatch.

Primary form of dynamic dispatch:

- Dynamic dispatch on dialect id
- Usually refer precomputed tables (e.g. opcode -> side effects).
- as a last resort dispatch to dialect version of function (e.g. print,
  parse,...)

`StaticInferfaces`, etc. provides support for installing LUT pointers, function
ptrs, etc. for dynamic dispatch. `InterfaceTraits<Obj>` is used to provide the
underlying dynamic dispatch based on the object. This is usually based on the
obj id, but e.g. can also dispatch on Instr opcode.

### Side Information

Hash-tables are slow. Dense numbering allows LUT-based maps/sets.

For dense mappings prefer `ObjMapVec<ObjT, Val>`.

## DSL and textual IR interop (TODO)

### Object definitions

```
let hw.wire = type {
    let params = list { %sz:cpp.unsigned }
}
let op.ADD = core.instr {
    let variadic = true;
    // ...
}
```

```
dialect op {
using core;
let ADD = Instr { }
}
```

The DSL should be implemented using dyno and is just a shortform for the
equivalent Dyno dialect.

```
dsl.LET %a:dsl.def("op", "ADD") %b:core.block {
    //...
}
```

### Rewrite rules

Full RegExp Support for Opcode and Operand matching. Base matcher expressions:

- '.': any operand
- 'wire(32)': any Obj constructor

let X, Y be any matcher expressions:

- 'X\*': 0 or more matches
- 'X+': 1 or more matches
- '(X)': (nameable) group
- 'X | Y': match X or Y
- 'X, Y': first match X and then Y
- 'X / Y': disable operand matcher ordering for expressions X and Y, but matches
  in X have precedence over Y
- 'def(X)': all operands in X must be defs
- 'use(X)': all operands in X must be uses

Any part of the expression can be named using '%name:' (including the
instruction itself)

```
using op;
using rewrite;

let pat = InstrPat {
    match { some IR matchers }
    if { some cpp code }
    if { ... }
    emit { some IR transformations }
    do { some cpp code }
}
let pat = InstrPat {
    match {
        %opc:(ADD|SUB) %d:def(wire(32)), %constants:constant(0)+ / %others:.+
    }
    emit {
        %opc %d, %others
    }
}
let pat = InstrPat {
    match {
        ADD %d:def(wire(32)), constant(0)+
    }
    emit {
        REPLACE_USES %d, constant(0)
    }
}
```

Rewrite built-ins:

```
rewrite.REPLACE_USES
rewrite.REPLACE_ALL
rewrite.DELETE
//...
```

If Instr emit pattern overwrites all defs of Instr match pattern, the
instruction is automatically deleted.

- Multiple emission backends possible: Instr+CInstr matching state machine, C++
  emission to mather function taking opcode and ArrayRef of operands, ...

Rewrite patterns:

- support inline CPP in constructor?

## Core (built-in dialect)

Goal: Arbitrary graphs of dyno objects

### Instr

Container for IR instructions:

- FatRef Operands
- Full SSA use-list tracking

Main "currency" in Dyno-IR. e.g. printing/parsing should go through this.

Minimize mutation, prefer new instructions. This is important for update
tracking (analysis maintenance, continuous InstCombine etc.).

#### Impl

Instr is a trailing object (i.e. one allocation) containing a header (e.g.
opcode) and trailing Operands(FatObjRefs).

### CInstr (unimpl)

Compressed container for IR instructions:

- Thin Operands
- No SSA use-list tracking (only use-counts) Seamless automatic conversion
  between Instr and CInstr.

### CFG

Build hierarchy of dyno objects. Imposes optional ordering on top of
Instr/CInstr.

Mental model:

- Instrs represent a data-flow DAG through def/use
- Blocks are a subgraph of the Instr graph
- Instrs def/use oher blocks to build control flow. Can have Instrs that are
  part of one/none/multiple CFGs (e.g. Multiple verions of the IR (think
  e-graphs)

#### Impl

Block is implemented as a vector of Node(InstrRef, unsinged prev, unsigned
next). This can be used as an unordered container, or an inline doubly linked
list.

### Pass Management (unimpl)

- Represent execution pipeline graph using Instrs <dialect>.exec.<pass>
  <Analysis output> <Analysis input>
- scriptable using DSL (maybe even compile static execution graph to C++)
- Passes can actively modify the execution graph if they detect that certain
  optimizations need to run again or can be skipped
  - e.g. pass looks at all instructions, determines that instruction xyz is not
    used, deletes some lowering passes from the execution graph
- EVERYTHING should be an "Analysis" input/output (including the IR (context)
  itself, CFG, etc.), all input/output state must be explicitly modeled
- Automatic parallelization of execution graphs that don't share inputs
- You should be able to write tests like this:

```
// RUN: dyno-cli -exec=test %s | FileCheck ...
let prog = CFG {
    // Some hw instrs
    ; Some instr check-lines
}

let test = ExecutionGraph {
    hw.exec.inline def(%a) %prog
    hw.exec.simplify def(%b) %prog
    core.exec.print %prog
}
```

Even:

```
let prog = CFG {
    // Some hw instrs
}
let prog2 = CFG {
    // Some hw instrs
}

let test = ExecutionGraph {
    hw.exec.inline def(%a) %prog
    hw.exec.simplify def(%b) %a
    dmt.exec.lec def(%r) %b
    core.exec.print %r
}
```

### Builtin Analysis

E.g. mapping of Opcode to all instrs

## IR mutation

### IR Builders (also act as Mutators)

- Discourage direct instruction mutation, always create new instructions by
  calling buildXYZ
- Make the builder the only code that directly creates/updates/deletes IR
  - hooking creation/deletion in allocator is evil:
    - instruction might not be fully constructed at the hook-point, or IR might
      be in inconsistent state (e.g. SSA violation), so the only valid thing to
      do in a hook is to track changes
    - we might want to hold onto the instruction instead of deleting it. In the
      hook it's already too late.
    - you pay for hook even if you don't use it
  - Instead: builder is the natural point to track IR mutations without dynamic
    hooks, deduplicate/const-prop without even building the instr, etc.
- template Builder (or builder functions) with different modes
  - TrackingMode: LazyUpdate (store InstrUpdate/BlockUpdate in list, don't
    execute yet), LazyTracking (store InstrUpdate/BlockUpdate in list, but
    execute immediately), Eager (call dynamic hooks, hopefully we don't need
    this)
  - InstrMutationMode: Immutable (must create a new instruction), Mutable (may
    update the old intruction, e.g. sth like buildXYZ<MUT>(InstrRef old, ...) vs
    buildXYZ<NEW>(InstrRef old, ...)),
  - Most powerful: LazyUpate + Immutable (should be the default)

### Lazy IR mutation and update tracking: InstrUpdate, BlockUpdate

User should mostly interact with the Builder to perform mutations.

```cpp
// Maybe fuse these
enum class InstrUpdateKind {
    NOP,
    INVALID,
    INSTR_CREATE,
    INSTR_DELETE,
    OPCODE_CHANGE,
    OPERAND_REPLACE,
    OPERAND_REPLACE_ALL_USES
};
enum class BlockUpdateKind {
    NOP,
    INVALID,
    BLOCK_CREATE,
    BLOCK_DELETE,
    BLOCK_SPLICE,
    INSTR_INSERT,
    INSTR_REMOVE,
};

class InstrUpdate {
    InstrUpdate revert() {}
};
class BlockUpdate //...

class InstrMutRef<BruhInstrRef> {
    InstrUpdate changeOpcode() {};
};
class OperandMutRef {
    InstrUpdate replace() {};
    InstrUpdate replaceAllUsesWith() {};
};

class BlockUpdates {
    std::vector<BlockUpdates>
    // (Maybe) Fast-paths
    ObjMap<Instr,bool> deletionMap;

    BlockUpdates revert() {}
};

class CFG {
    void executeUpdates(BlockUpdate&);
};

class BlockRef_mut_iterator {
    BlockUpdate erase();
    BlockUpdate insertPrev();
};
```

- Instead of dynamic hooking, the user can perform IR mutations and then pass
  the captured updates to whatever analysis needs maintenance.
- IR rollback by discarding lazy updates, or executing updates in reverse
  (database write-ahead-log style)

# Dynomite Solver

dmt dialect

## Solvers

### SAT

### SMT

### AIG

```cpp
class AIGNode {
    ObjID op[2];
};
// ID space;
// lowest bit: negated
// low numbers: fataignodes (or highest bit, do we want AIG primary inputs to change?)

class AIGNodeTRef : public ObjRef<AIGNode> {
};
class AIGNodeRef {
    // Supports fat and non-fat refs

};
class AIGStore {
};
class FatAIGNode {
    InstDefUse defUse;
    AIGNode node;
};
```

```
dmt.AIG_IN %0:fataignode %1(wire)
op.AND
dmt.AIG_OUT %1:wire(32) ...32 fat/thin aignodes
```

### BDD

## Types

- var: smt variable
- sat(-clause)
- aig(-node)
- bdd(-node)
- assertion
- assumption

## Instrs

```Data-flow:
dmt.AIG <Output wire> <Input wire> <AIG primary outputs> <AIG primary inputs>
dmt.BDD <Output wire> <Input wire> <BDD root>

Solver propositions/atoms:
op.ASSERT
op.ASSUME
op.AND/OR/NOT/CMP

Solver execution graph:
dmt.exec.AIG_REWRITE
dmt.exec.AIG_SWEEP
dmt.exec.SOLVE <e.g. op.AND (op.ASSERT (op.CMP_LT var(x) var(y))) (op.ASSUME ...)>
## Conversion
op arithmetic <-> dmt.AIG (eager bitblasting)
dmt.AIG <-> dmt.BDD
```
