#include <dyno/GlobalResolver.h>

using namespace dyno;

std::array<void **, GlobalResolver::numDialects * GlobalResolver::numTypes>
    dyno::GlobalResolver::globResolverTable = {};
