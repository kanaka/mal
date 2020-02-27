import "./types" for MalList

class Env {
  construct new() {
    _outer = null
    _data = {}
  }
  construct new(outer) {
    _outer = outer
    _data = {}
  }
  construct new(outer, binds, exprs) {
    _outer = outer
    _data = {}
    for (i in 0...binds.count) {
      if (binds[i].value == "&") {
        _data[binds[i + 1].value] = MalList.new(exprs[i..-1])
        break
      } else {
        _data[binds[i].value] = exprs[i]
      }
    }
  }

  set(k, v) { _data[k] = v }

  find(k) {
    if (_data.containsKey(k)) return this
    if (_outer) return _outer.find(k)
    return null
  }

  get(k) {
    var foundEnv = find(k)
    if (!foundEnv) Fiber.abort("'%(k)' not found")
    return foundEnv.getValue(k)
  }

  getValue(k) { _data[k] }
}
