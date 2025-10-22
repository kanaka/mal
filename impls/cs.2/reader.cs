using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace mal
{
    class Reader
    {
        static string TOKENIZER_REGEX = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)";
        static Regex TOKENIZER_INSTANCE = new Regex(TOKENIZER_REGEX);
        static Dictionary<char, string> UNESCAPE = new Dictionary<char, string>()
        {
            {'n', "\n"}, {'"', "\""}, {'\\', "\\"}, {'t', "\t"}
        };
        static Dictionary<string, string> QUOTING = new Dictionary<string, string>()
        {
            {"'", "quote"}, {"`", "quasiquote"}, {"~", "unquote"}, {"~@", "splice-unquote"}, {"@", "deref"}
        };
        static Dictionary<string, MalType> LITERALS = new Dictionary<string, MalType>()
        {
            {"nil", MalNil.MAL_NIL}, {"true", MalBoolean.MAL_TRUE}, {"false", MalBoolean.MAL_FALSE}
        };

        IList<string> tokens;
        int position;

        Reader(IList<string> tokens)
        {
            this.tokens = tokens;
            this.position = 0;
        }

        string next()
        {
            if (position >= tokens.Count)
            {
                return null;
            }
            else
            {
                string val = peek();
                position++;
                return val;
            }
        }

        string peek()
        {
            return (position < tokens.Count) ? tokens[position] : null;
        }

        static IList<string> tokenize(string input)
        {
            IList<string> tokens = new List<string>();
            Match match = TOKENIZER_INSTANCE.Match(input);
            while (match.Success)
            {
                string value = match.Groups[1].Value;
                if (value?.Length > 0)
                {
                    tokens.Add(value);
                }
                match = match.NextMatch();
            }

            return tokens;
        }

        public static MalType read_str(string input)
        {
            var tokens = tokenize(input);
            var reader = new Reader(tokens);
            return read_form(reader);
        }

        public static MalType read_form(Reader reader)
        {
            string first = reader.peek();
            if (first == null) return null; // signal that we don't want to print back
            if (first.StartsWith(";")){
                // drop the token if it's a comment, continue with the next token
                reader.next();
                return read_form(reader);
            }
            if (first == "^") // expect two other forms
            {
                reader.next(); // drop the '^'
                MalHashmap metadata = read_hashmap(reader);
                MalType value = read_form(reader);
                List<MalType> items = new List<MalType>() { new MalSymbol("with-meta"), value, metadata };
                MalList listWithMeta = new MalList(items);
                return listWithMeta;
            }
            else if (QUOTING.ContainsKey(first))
            {
                string quotation = QUOTING.GetValueOrDefault(reader.next(), null);
                MalType quoted = read_form(reader);
                List<MalType> items = new List<MalType>()
                {
                    new MalSymbol(quotation),
                    quoted
                };
                MalList quotedList = new MalList(items);
                return quotedList;
            }
            else if (CLOSING_BRACKETS.ContainsValue(first))
            {
                return null; // empty form
            }
            else if (first == "{")
            {
                return read_hashmap(reader);
            }
            else if (first == "[")
            {
                return read_vector(reader);
            }
            else if (CLOSING_BRACKETS.ContainsKey(first))
            {
                return read_list(reader);
            }
            else
            {
                return read_atom(reader);
            }
        }

        public static Dictionary<String, String> CLOSING_BRACKETS = new Dictionary<string, string>() {
            {"(", ")"}, {"[", "]"}, {"{", "}"}
        };
        static MalList read_list(Reader reader)
        {
            string openingBracket = reader.next();
            string closingBracket = CLOSING_BRACKETS.GetValueOrDefault(openingBracket);

            List<MalType> items = new List<MalType>();
            string peeked = null;
            do
            {
                MalType item = read_form(reader);
                if (item != null)
                {
                    items.Add(item);
                }
                peeked = reader.peek();
            } while (peeked != null && peeked != closingBracket);

            // confirm the list has valid closing parens
            var closing = reader.next();
            if (closing == closingBracket)
            {
                return new MalList(items);
            }
            else
            {
                throw new MalException(new MalString("Expression has unbalanced parenthesis"));
            }
        }

        static MalVector read_vector(Reader reader)
        {
            MalList values = read_list(reader);
            return new MalVector(values.items);
        }

        static MalHashmap read_hashmap(Reader reader)
        {
            MalList kvs = read_list(reader);
            if (kvs.items.Count % 2 == 1)
            {
                throw new MalException(new MalString("Hashmap needs an even number of forms"));
            }
            Dictionary<MalType, MalType> pairs = new Dictionary<MalType, MalType>();
            for (int i = 0; i < kvs.items.Count; i += 2)
            {
                MalType key = kvs.items[i];
                MalType val = kvs.items[i + 1];
                pairs.Add(key, val);
            }
            return new MalHashmap(pairs);
        }
        static MalType read_atom(Reader reader)
        {
            // Attempt to parse a number first
            string item = reader.next();
            if (item.StartsWith(":"))
            {
                return new MalKeyword(item.Substring(1));
            }
            else if (LITERALS.ContainsKey(item))
            {
                return LITERALS.GetValueOrDefault(item);
            }
            else if (item.StartsWith("\""))
            {
                if (item.Length > 1 && item.EndsWith("\""))
                {
                    string source = item.Substring(1, item.Length - 2);
                    string unescaped = unescape(source);
                    return new MalString(unescaped); // length minus the quotes
                }
                else
                {
                    throw new MalException(new MalString("String contains unbalanced quotes"));
                }
            }
            try
            {
                int intValue = int.Parse(item);
                return new MalInteger(intValue);
            }
            catch
            {
                return new MalSymbol(item);
            }
        }

        static string unescape(string input)
        {
            bool escaping = false;
            string output = "";
            foreach (char c in input)
            {
                if (!escaping && c == '\\')
                {
                    escaping = true;
                    continue;
                }
                else if (escaping)
                {
                    if (UNESCAPE.ContainsKey(c))
                    {
                        output += UNESCAPE[c];
                        escaping = false;
                    }
                    else
                    {
                        throw new MalException(new MalString("String contains unbalanced escaped characters"));
                    }
                }
                else
                {
                    output += c;
                }

            }
            if (escaping)
            {
                throw new MalException(new MalString("String contains unbalanced escaped characters"));
            }
            return output;
        }
    }
}
