import "io" for File
import "./reader" for MalReader
import "./readline" for Readline
import "./printer" for Printer
import "./types" for MalVal, MalSymbol, MalSequential, MalList, MalVector, MalMap, MalNativeFn, MalFn, MalAtom, MalException
import "./interop" for Interop

class Core {
  static fn(func) { MalNativeFn.new(func) }
  static ns {
    return {
      "=":     fn { |a| a[0] == a[1] },
      "throw": fn { |a|
                 MalException.set(a[0])
                 Fiber.abort("___MalException___")
               },

      "nil?":     fn { |a| a[0] == null },
      "true?":    fn { |a| a[0] == true },
      "false?":   fn { |a| a[0] == false },
      "string?":  fn { |a| a[0] is String && !MalVal.isKeyword(a[0]) },
      "symbol":   fn { |a| a[0] is MalSymbol ? a[0] : MalSymbol.new(a[0]) },
      "symbol?":  fn { |a| a[0] is MalSymbol },
      "keyword":  fn { |a| MalVal.isKeyword(a[0]) ? a[0] : MalVal.newKeyword(a[0]) },
      "keyword?": fn { |a| MalVal.isKeyword(a[0]) },
      "number?":  fn { |a| a[0] is Num },
      "fn?":      fn { |a| a[0] is MalNativeFn || (a[0] is MalFn && !a[0].isMacro) },
      "macro?":   fn { |a| a[0] is MalFn && a[0].isMacro },

      "pr-str":      fn { |a| a.map { |e| Printer.pr_str(e, true) }.join(" ") },
      "str":         fn { |a| a.map { |e| Printer.pr_str(e, false) }.join() },
      "prn":         fn { |a|
                       System.print(a.map { |e| Printer.pr_str(e, true) }.join(" "))
                       return null
                     },
      "println":     fn { |a|
                       System.print(a.map { |e| Printer.pr_str(e, false) }.join(" "))
                       return null
                     },
      "read-string": fn { |a| MalReader.read_str(a[0]) },
      "readline":    fn { |a| Readline.readLine(a[0]) },
      "slurp":       fn { |a| File.read(a[0]) },

      "<":       fn { |a| a[0] < a[1] },
      "<=":      fn { |a| a[0] <= a[1] },
      ">":       fn { |a| a[0] > a[1] },
      ">=":      fn { |a| a[0] >= a[1] },
      "+":       fn { |a| a[0] + a[1] },
      "-":       fn { |a| a[0] - a[1] },
      "*":       fn { |a| a[0] * a[1] },
      "/":       fn { |a| a[0] / a[1] },
      "time-ms": fn { |a| (System.gettimeofday * 1000).floor },

      "list":      fn { |a| MalList.new(a) },
      "list?":     fn { |a| a[0] is MalList },
      "vector":    fn { |a| MalVector.new(a) },
      "vector?":   fn { |a| a[0] is MalVector },
      "hash-map":  fn { |a| MalMap.fromList(a) },
      "map?":      fn { |a| a[0] is MalMap },
      "assoc":     fn { |a| a[0].assoc(a[1...a.count]) },
      "dissoc":    fn { |a| a[0].dissoc(a[1...a.count]) },
      "get":       fn { |a| a[0] == null ? null : a[0].data[a[1]] },
      "contains?": fn { |a| a[0].data.containsKey(a[1]) },
      "keys":      fn { |a| MalList.new(a[0].data.keys.toList) },
      "vals":      fn { |a| MalList.new(a[0].data.values.toList) },

      "sequential?": fn { |a| a[0] is MalSequential },
      "cons":        fn { |a| MalList.new([a[0]] + a[1].elements) },
      "concat":      fn { |a| MalList.new(a.reduce([]) { |acc,e| acc + e.elements }) },
      "nth":         fn { |a| a[1] < a[0].count ? a[0][a[1]] : Fiber.abort("nth: index out of range") },
      "first":       fn { |a| a[0] == null ? null : a[0].first },
      "rest":        fn { |a| a[0] == null ? MalList.new([]) : a[0].rest },
      "empty?":      fn { |a| a[0].isEmpty },
      "count":       fn { |a| a[0] == null ? 0 : a[0].count },
      "apply":       fn { |a| a[0].call(a[1...(a.count - 1)] + a[-1].elements) },
      "map":         fn { |a| MalList.new(a[1].elements.map { |e| a[0].call([e]) }.toList) },

      "conj": fn { |a|
                if (a[0] is MalList) return MalList.new(a[-1..1] + a[0].elements)
                if (a[0] is MalVector) return MalVector.new(a[0].elements + a[1..-1])
              },
      "seq":  fn { |a|
                if (a[0] == null) return null
                if (a[0].count == 0) return null
                if (a[0] is String) return MalList.new(a[0].toList)
                if (a[0] is MalVector) return MalList.new(a[0].elements)
                return a[0]
              },

      "meta":      fn { |a| a[0].meta },
      "with-meta": fn { |a|
                     var x = a[0].clone()
                     x.meta = a[1]
                     return x
                   },
      "atom":      fn { |a| MalAtom.new(a[0]) },
      "atom?":     fn { |a| a[0] is MalAtom },
      "deref":     fn { |a| a[0].value },
      "reset!":    fn { |a| a[0].value = a[1] },
      "swap!":     fn { |a| a[0].value = a[1].call([a[0].value] + a[2..-1]) },

      "wren-eval": fn { |a| Interop.wren_eval(a[0]) }
    }
  }
}
