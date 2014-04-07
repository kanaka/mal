using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using Mal;
using MalVal = Mal.types.MalVal;
using MalList = Mal.types.MalList;

namespace Mal {
    public class printer {
        public static string join(List<MalVal> value,
                                string delim, bool print_readably) {
            List<string> strs = new List<string>();
            foreach (MalVal mv in value) {
                strs.Add(mv.ToString(print_readably));
            }
            return String.Join(delim, strs.ToArray());
        }

        public static string join(Dictionary<string,MalVal> value,
                                string delim, bool print_readably) {
            List<string> strs = new List<string>();
            foreach (KeyValuePair<string, MalVal> entry in value) {
                if (print_readably) {
                    strs.Add("\"" + entry.Key.ToString() + "\"");
                } else {
                    strs.Add(entry.Key.ToString());
                }
                strs.Add(entry.Value.ToString(print_readably));
            }
            return String.Join(delim, strs.ToArray());
        }

        public static string _pr_str(MalVal mv, bool print_readably) {
            return mv.ToString(print_readably);
        }

        public static string _pr_str_args(MalList args, String sep,
                                          bool print_readably) {
            return join(args.getValue(), sep, print_readably);
        }

        public static string escapeString(string str) {
            return Regex.Escape(str);
        }

    }
}
