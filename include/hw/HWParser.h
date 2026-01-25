#pragma once

#include "aig/PrintParse.h"
#include "dyno/Parser.h"
#include "dyno/Context.h"
#include "hw/HWContext.h"
#include "hw/PrintParse.h"
#include "meta/PrintParse.h"
#include "op/PrintParse.h"

namespace dyno {

class HWParser
    : public Parser<CoreDialectParser, MetaDialectParser, OpDialectParser,
                    HWDialectParser, AIGDialectParser> {
public:
  explicit HWParser(Context &ctx) : Parser(ctx) {}
};

}; // namespace dyno
