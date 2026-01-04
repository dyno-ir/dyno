#pragma once

#include "aig/PrintParse.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/PrintParse.h"
#include "meta/PrintParse.h"
#include "op/PrintParse.h"

namespace dyno {

class HWParser
    : public Parser<HWParser, CoreDialectParser<HWParser>,
                    MetaDialectParser<HWParser>, OpDialectParser<HWParser>,
                    HWDialectParser<HWParser>, AIGDialectParser<HWParser>> {
  HWContext &ctx;

public:
  explicit HWParser(HWContext &ctx) : ctx(ctx) {
    this->sourceLocInfo = &ctx.sourceLocInfo;
    std::get<HWDialectParser<HWParser>>(parsers).ctx = &ctx;
    std::get<OpDialectParser<HWParser>>(parsers).mapStore = &ctx.getMaps();
  }
  auto &getInstrs() { return ctx.getInstrs(); }
  auto &getConstants() { return ctx.getConstants(); }
  auto &getCFG() { return ctx.getCFG(); }
};

}; // namespace dyno
