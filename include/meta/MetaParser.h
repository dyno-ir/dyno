#pragma once

#include "dyno/Parser.h"
#include "meta/PrintParse.h"
#include "op/PrintParse.h"
namespace dyno {

class MetaParser
    : public Parser<CoreDialectParser, OpDialectParser, MetaDialectParser> {

public:
  explicit MetaParser(Context &ctx) : Parser(ctx) {}
};

}; // namespace dyno
