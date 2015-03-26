#include "Environment.h"
#include "Types.h"

#include <algorithm>

malValuePtr malEnv::get(const String& symbol)
{
    auto it = m_map.find(symbol);
    if (it != m_map.end()) {
        return it->second;
    }
    ASSERT(false, "'%s' not found", symbol.c_str());
}

malValuePtr malEnv::set(const String& symbol, malValuePtr value)
{
    m_map[symbol] = value;
    return value;
}
