#include "MAL.h"
#include "Types.h"

#include <regex>

typedef std::regex              Regex;
typedef std::sregex_iterator    RegexIter;

static const Regex tokenRegex("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]+)");
static const Regex intRegex("^[-+]?\\d+$");
static const Regex closeRegex("[\\)\\]}]");

class Tokeniser
{
public:
    Tokeniser(const String& input);

    String peek() const {
        ASSERT(!eof(), "Tokeniser reading past EOF in peek");
        return m_iter->str(1);
    }

    String next() {
        ASSERT(!eof(), "Tokeniser reading past EOF in next");
        String ret = peek();
        checkPrefix();
        ++m_iter;
        skipWhitespace();
        return ret;
    }

    bool eof() const {
        return m_iter == m_end;
    }

private:
    void skipWhitespace();
    void checkPrefix();

    RegexIter   m_iter;
    RegexIter   m_end;
};

Tokeniser::Tokeniser(const String& input)
: m_iter(input.begin(), input.end(), tokenRegex)
, m_end()
{
    skipWhitespace();
}

static bool isWhitespace(const String& token)
{
    return token.empty() || (token[0] == ';');
}

void Tokeniser::checkPrefix()
{
    // This is the unmatched portion before the match.
    auto prefix = m_iter->prefix();

    if (prefix.length() == 0) {
        return;
    }

    const String& text = prefix.str();
    if (text == "\"") {
        ASSERT(false, "Expected \", got EOF");
    }
    ASSERT(false, "Unexpected \"%s\"", text.c_str());
}

void Tokeniser::skipWhitespace()
{
    while (!eof() && isWhitespace(peek())) {
        checkPrefix();
        ++m_iter;
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
        readList(tokeniser, items.get(), "}");
        return mal::hash(items.release());
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
