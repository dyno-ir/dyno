#pragma once
#include "dyno/Context.h"

namespace dyno {

class VirtualDialectContext {
  void *self;
  using create_t = void *();
  using destroy_t = void(void *);
  using register_t = void(Context &, void *);
  create_t *createFP;
  destroy_t *destroyFP;
  register_t *registerFP;

public:
  VirtualDialectContext(const VirtualDialectContext &) = delete;
  VirtualDialectContext(VirtualDialectContext &&o) { *this = std::move(o); }
  VirtualDialectContext &operator=(const VirtualDialectContext &) = delete;
  VirtualDialectContext &operator=(VirtualDialectContext &&o) {
    self = std::exchange(o.self, nullptr);
    createFP = o.createFP;
    destroyFP = o.destroyFP;
    registerFP = o.registerFP;
    return *this;
  }

  VirtualDialectContext(void *self, create_t *createFP, destroy_t *destroyFP,
                        register_t registerFP)
      : self(self), createFP(createFP), destroyFP(destroyFP),
        registerFP(registerFP) {};

  VirtualDialectContext create() {
    return {createFP(), createFP, destroyFP, registerFP};
  }
  void registerSelf(Context &ctx) { registerFP(ctx, self); }
  ~VirtualDialectContext() {
    if (self)
      destroyFP(self);
  }
};

// Simple context wrapper to easily clone a context with the same dialect set
// up.
class FatContext : public Context {
  SmallVec<VirtualDialectContext, 32> contexts;

  template <typename T> static void *create() { return new T(); }
  template <typename T> static void destroy(void *t) {
    delete reinterpret_cast<T *>(t);
  }
  template <typename T> static void registerF(Context &ctx, void *dialCtx) {
    ctx.registerDialect(*reinterpret_cast<T *>(dialCtx));
  }

public:
  template <typename ContextT, typename... Args> void add(Args &&...args) {
    ContextT *ctx = new ContextT(std::forward<Args>(args)...);
    contexts.emplace_back((void *)ctx, &FatContext::create<ContextT>,
                          &FatContext::destroy<ContextT>,
                          &FatContext::registerF<ContextT>);
    this->registerDialect(*ctx);
  }

  // clones dialect setup, not contents
  FatContext create() {
    FatContext store;
    store.contexts.reserve(contexts.size());
    for (auto &virtCtx : contexts) {
      auto &ref = store.contexts.emplace_back(virtCtx.create());
      ref.registerSelf(store);
    }
    return store;
  }
};

}; // namespace dyno
