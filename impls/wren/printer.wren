import "./types" for MalVal, MalList, MalVector, MalMap, MalNativeFn, MalFn, MalAtom

class Printer {
  static joinElements(elements, print_readably) {
    return elements.map { |e| pr_str(e, print_readably) }.join(" ")
  }

  static joinMapElements(data, print_readably) {
    return data.map { |e| pr_str(e.key, print_readably) + " " + pr_str(e.value, print_readably) }.join(" ")
  }

  static escape(s) {
    return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""
  }

  static pr_str(obj) { pr_str(obj, true) }

  static pr_str(obj, print_readably) {
    if (obj == null) return "nil"
    if (obj is MalList) return "(%(joinElements(obj.elements, print_readably)))"
    if (obj is MalVector) return "[%(joinElements(obj.elements, print_readably))]"
    if (obj is MalMap) return "{%(joinMapElements(obj.data, print_readably))}"
    if (obj is MalNativeFn) return "#<MalNativeFn>"
    if (obj is MalFn) return "#<MalFn>"
    if (obj is MalAtom) return "(atom %(pr_str(obj.value, print_readably)))"
    if (MalVal.isKeyword(obj)) return ":%(obj[1..-1])"
    if (obj is String) return print_readably ? escape(obj) : obj
    return obj.toString
  }
}
