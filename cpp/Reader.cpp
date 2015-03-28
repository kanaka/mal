#include "MAL.h"
#include "Types.h"

#include <regex>

typedef std::regex              Regex;

static const Regex intRegex("^[-+]?\\d+$");
static const Regex closeRegex("[\\)\\]}]");

static const Regex whitespaceRegex("[\\s,]+|;.*");
static const Regex tokenRegexes[] = {
    Regex("~@"),
    Regex("[\\[\\]{}()'`~^@]"),
    Regex("\"(?:\\\\.|[^\\\\\"])*\""),
    Regex("[^\\s\\[\\]{}('\"`,;)]+"),
};

class Tokeniser
{
public:
    Tokeniser(const String& input);

    String peek() const {
        //TODO: need to split ASSERT into ASSERT & MALCHECK
        //      most are MALCHECK's, this is an internal error
        ASSERT(!eof(), "Tokeniser reading past EOF in peek");
        return m_token;
    }

    String next() {
        //TODO: need to split ASSERT into ASSERT & MALCHECK
        //      most are MALCHECK's, this is an internal error
        ASSERT(!eof(), "Tokeniser reading past EOF in next");
        String ret = peek();
        nextToken();
        return ret;
    }

    bool eof() const {
        return m_iter == m_end;
    }

private:
    void skipWhitespace();
    void nextToken();

    bool matchRegex(const Regex& regex);

    typedef String::const_iterator StringIter;

    String      m_token;
    StringIter  m_iter;
    StringIter  m_end;
};

Tokeniser::Tokeniser(const String& input)
:   m_iter(input.begin())
,   m_end(input.end())
{
    nextToken();
}

bool Tokeniser::matchRegex(const Regex& regex)
{
    if (eof()) {
        return false;
    }

    std::smatch match;
    auto flags = std::regex_constants::match_continuous;
    if (!std::regex_search(m_iter, m_end, match, regex, flags)) {
        return false;
    }

    ASSERT(match.size() == 1, "Should only have one submatch, not %d",
                              match.size());
    ASSERT(match.position(0) == 0, "Need to match first character");
    ASSERT(match.length(0) > 0, "Need to match a non-empty string");

    // Don't advance  m_iter now, do it after we've consumed the token in
    // next().  If we do it now, we hit eof() when there's still one token left.
    m_token = match.str(0);

    return true;
}

void Tokeniser::nextToken()
{
    m_iter += m_token.size();

    skipWhitespace();
    if (eof()) {
        return;
    }

    for (auto &it : tokenRegexes) {
        if (matchRegex(it)) {
            return;
        }
    }

    String mismatch(m_iter, m_end);
    if (mismatch[0] == '"') {
        ASSERT(false, "Expected \", got EOF");
    }
    else {
        ASSERT(false, "Unexpected \"%s\"", mismatch.c_str());
    }
}

void Tokeniser::skipWhitespace()
{
    while (matchRegex(whitespaceRegex)) {
        m_iter += m_token.size();
    }
}

static malValuePtr readAtom(Tokeniser& tokeniser);
static malValuePtr readForm(Tokeniser& tokeniser);
static void readList(Tokeniser& tokeniser, malValueVec* items,
                      const String& end);
static malValuePtr processMacro(Tokeniser& tokeniser, const String& symbol);

malValuePtr readStr(const String& input)
{
    Tokeniser tokeniser(input);
    if (tokeniser.eof()) {
        throw malEmptyInputException();
    }
    return readForm(tokeniser);
}

static malValuePtr readForm(Tokeniser& tokeniser)
{
    ASSERT(!tokeniser.eof(), "Expected form, got EOF");
    String token = tokeniser.peek();

    ASSERT(!std::regex_match(token, closeRegex),
            "Unexpected \"%s\"", token.c_str());

    if (token == "(") {
        tokeniser.next();
        std::unique_ptr<malValueVec> items(new malValueVec);
        readList(tokeniser, items.get(), ")");
        return mal::list(items.release());
    }
    if (token == "[") {
        tokeniser.next();
        std::unique_ptr<malValueVec> items(new malValueVec);
        readList(tokeniser, items.get(), "]");
        return mal::vector(items.release());
    }
    if (token == "{") {
        tokeniser.next();
        std::unique_ptr<malValueVec> items(new malValueVec);
        items->push_back(mal::symbol("hash-map"));
        readList(tokeniser, items.get(), "}");
        return mal::list(items.release());
    }
    return readAtom(tokeniser);
}

static malValuePtr readAtom(Tokeniser& tokeniser)
{
    struct ReaderMacro {
        const char* token;
        const char* symbol;
    };
    ReaderMacro macroTable[] = {
        { "@",   "deref" },
        { "`",   "quasiquote" },
        { "'",   "quote" },
        { "~@",  "splice-unquote" },
        { "~",   "unquote" },
    };
    const ReaderMacro* macroTableEnd = macroTable + ARRAY_SIZE(macroTable);

    struct Constant {
        const char* token;
        malValuePtr value;
    };
    Constant constTable[] = {
        { "false",  mal::falseValue()  },
        { "nil",    mal::nilValue()          },
        { "true",   mal::trueValue()   },
    };
    const Constant* constTableEnd = constTable + ARRAY_SIZE(constTable);

    String token = tokeniser.next();
    if (token[0] == '"') {
        return mal::string(unescape(token));
    }
    if (token[0] == ':') {
        return mal::keyword(token);
    }
    if (token == "^") {
        malValuePtr meta = readForm(tokeniser);
        malValuePtr value = readForm(tokeniser);
        // Note that meta and value switch places
        return mal::list(mal::symbol("with-meta"), value, meta);
    }
    for (Constant* it = constTable; it != constTableEnd; ++it) {
        if (token == it->token) {
            return it->value;
        }
    }
    for (ReaderMacro *it = macroTable; it < macroTableEnd; ++it) {
        if (token == it->token) {
            return processMacro(tokeniser, it->symbol);
        }
    }
    if (std::regex_match(token, intRegex)) {
        return mal::integer(token);
    }
    return mal::symbol(token);
}

static void readList(Tokeniser& tokeniser, malValueVec* items,
                      const String& end)
{
    while (1) {
        ASSERT(!tokeniser.eof(), "Expected \"%s\", got EOF", end.c_str());
        if (tokeniser.peek() == end) {
            tokeniser.next();
            return;
        }
        items->push_back(readForm(tokeniser));
    }
}

static malValuePtr processMacro(Tokeniser& tokeniser, const String& symbol)
{
    return mal::list(mal::symbol(symbol), readForm(tokeniser));
}
