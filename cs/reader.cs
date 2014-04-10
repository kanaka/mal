using System;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using Mal;
using MalVal = Mal.types.MalVal;
using MalSymbol = Mal.types.MalSymbol;
using MalList = Mal.types.MalList;
using MalVector = Mal.types.MalVector;
using MalHashMap = Mal.types.MalHashMap;
using MalThrowable = Mal.types.MalThrowable;
using MalContinue = Mal.types.MalContinue;

namespace Mal {
    public class reader {
        public class ParseError : MalThrowable {
            public ParseError(string msg) : base(msg) { }
        }

        public class Reader {
            List<string> tokens;
            int position;
            public Reader(List<string> t) {
                tokens = t;
                position = 0;
            }

            public string peek() {
                if (position >= tokens.Count) {
                    return null;
                } else {
                    return tokens[position];
                }
            }
            public string next() {
                return tokens[position++];
            }
        }

        public static List<string> tokenize(string str) {
            List<string> tokens = new List<string>();
            string pattern = @"[\s ,]*(~@|[\[\]{}()'`~@]|""(?:[\\].|[^\\""])*""|;.*|[^\s \[\]{}()'""`~@,;]*)";
            Regex regex = new Regex(pattern);
            foreach (Match match in regex.Matches(str)) {
                string token = match.Groups[1].Value;
                if ((token != null) && !(token == "") && !(token[0] == ';')) {
                    //Console.WriteLine("match: ^" + match.Groups[1] + "$");
                    tokens.Add(token);
                }
            }
            return tokens;
        }

        public static MalVal read_atom(Reader rdr) {
            string token = rdr.next();
            string pattern = @"(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^("".*"")$|(^[^""]*$)";
            Regex regex = new Regex(pattern);
            Match match = regex.Match(token);
            //Console.WriteLine("token: ^" + token + "$");
            if (!match.Success) {
                throw new ParseError("unrecognized token '" + token + "'");
            }
            if (match.Groups[1].Value != String.Empty) {
                return new Mal.types.MalInteger(int.Parse(match.Groups[1].Value));
            } else if (match.Groups[3].Value != String.Empty) {
                return Mal.types.Nil;
            } else if (match.Groups[4].Value != String.Empty) {
                return Mal.types.True;
            } else if (match.Groups[5].Value != String.Empty) {
                return Mal.types.False;
            } else if (match.Groups[6].Value != String.Empty) {
                string str = match.Groups[6].Value;
                str = str.Substring(1, str.Length-2)
                    .Replace("\\\"", "\"")
                    .Replace("\\n", "\n");
                return new Mal.types.MalString(str);
            } else if (match.Groups[7].Value != String.Empty) {
                return new Mal.types.MalSymbol(match.Groups[7].Value);
            } else {
                throw new ParseError("unrecognized '" + match.Groups[0] + "'");
            }
        }

        public static MalVal read_list(Reader rdr, MalList lst, char start, char end) {
            string token = rdr.next();
            if (token[0] != start) {
                throw new ParseError("expected '" + start + "'");
            }

            while ((token = rdr.peek()) != null && token[0] != end) {
                lst.conj_BANG(read_form(rdr));
            }

            if (token == null) {
                throw new ParseError("expected '" + end + "', got EOF");
            }
            rdr.next();

            return lst;
        }

        public static MalVal read_hash_map(Reader rdr) {
            MalList lst = (MalList)read_list(rdr, new MalList(), '{', '}');
            return new MalHashMap(lst);
        }


        public static MalVal read_form(Reader rdr) {
            string token = rdr.peek();
            if (token == null) { throw new MalContinue(); }
            MalVal form = null;

            switch (token) {
                case "'": rdr.next();
                    return new MalList(new MalSymbol("quote"),
                                       read_form(rdr));
                case "`": rdr.next();
                    return new MalList(new MalSymbol("quasiquote"),
                                       read_form(rdr));
                case "~":
                    rdr.next();
                    return new MalList(new MalSymbol("unquote"),
                                       read_form(rdr));
                case "~@":
                    rdr.next();
                    return new MalList(new MalSymbol("splice-unquote"),
                                       read_form(rdr));
                case "^": rdr.next();
                    MalVal meta = read_form(rdr);
                    return new MalList(new MalSymbol("with-meta"),
                                       read_form(rdr),
                                       meta);
                case "@": rdr.next();
                    return new MalList(new MalSymbol("deref"),
                                       read_form(rdr));

                case "(": form = read_list(rdr, new MalList(), '(' , ')'); break;
                case ")": throw new ParseError("unexpected ')'");
                case "[": form = read_list(rdr, new MalVector(), '[' , ']'); break;
                case "]": throw new ParseError("unexpected ']'");
                case "{": form = read_hash_map(rdr); break;
                case "}": throw new ParseError("unexpected '}'");
                default:  form = read_atom(rdr); break;
            }
            return form;
        }


        public static MalVal read_str(string str) {
            return read_form(new Reader(tokenize(str)));
        }
    }
}
