#pragma once

#include "dyno/Parser.h"
#include "meta/MetaContext.h"
#include "meta/PrintParse.h"
#include "op/PrintParse.h"
namespace dyno {

class MetaParser : public Parser<MetaParser, CoreDialectParser<MetaParser>,
                                 OpDialectParser<MetaParser>,
                                 MetaDialectParser<MetaParser>> {

  MetaContext &ctx;

public:
  auto &getInstrs() { return ctx.getInstrs(); }
  auto &getConstants() { return ctx.getConstants(); }
  auto &getCFG() { return ctx.getCFG(); }

  explicit MetaParser(MetaContext &ctx) : ctx(ctx) {
    this->sourceLocInfo = &ctx.sourceLocInfo;
    std::get<OpDialectParser<MetaParser>>(parsers).mapStore = &ctx.getMaps();
  }
};

}; // namespace dyno
