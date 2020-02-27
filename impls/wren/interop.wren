import "meta" for Meta
import "./types" for MalList, MalMap

class Interop {
  static wren_eval(str) {
    var f = Meta.compileExpression(str)
    return f == null ? null : wren2mal(f.call())
  }

  static wren2mal(v) {
    if (v == null || v == true || v == false) return v
    if (v is Num || v is String) return v
    if (v is Map) {
      var m = {}
      for (e in v) {
        m[wren2mal(e.key)] = wren2mal(e.value)
      }
      return MalMap.new(m)
    }
    if (v is Sequence) return MalList.new(v.map { |e| wren2mal(e) }.toList)
    return null
  }
}
