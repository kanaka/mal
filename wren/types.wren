class MalVal {
  static newKeyword(value) { "\u029e%(value)" }
  static isKeyword(obj) { obj is String && obj.count > 0 && obj[0] == "\u029e" }
  meta { _meta }
  meta=(value) { _meta = value }
}

class MalSymbol is MalVal {
  construct new(value) { _value = value }
  value { _value }
  toString { _value }
  ==(other) { other is MalSymbol && other.value == _value }
  !=(other) { !(this == other) }
}

class MalSequential is MalVal {
  construct new(elements) { _elements = elements }
  elements { _elements }
  [index] { _elements[index] }
  isEmpty { _elements.count == 0 }
  count { _elements.count }
  first { isEmpty ? null : _elements[0] }
  rest { MalList.new(isEmpty ? [] : elements[1..-1]) }
  ==(other) {
    if (!(other is MalSequential)) return false
    if (other.count != count) return false
    for (i in 0...count) {
      if (other[i] != this[i]) return false
    }
    return true
  }
  !=(other) { !(this == other) }
}

class MalList is MalSequential {
  construct new(elements) { super(elements) }
  clone() { MalList.new(elements) }
}

class MalVector is MalSequential {
  construct new(elements) { super(elements) }
  clone() { MalVector.new(elements) }
}

class MalMap is MalVal {
  construct new(data) { _data = data }
  construct fromList(elements) {
    _data = {}
    var i = 0
    while (i < elements.count) {
      _data[elements[i]] = elements[i + 1]
      i = i + 2
    }
  }
  clone() { MalMap.new(_data) }
  data { _data }
  assoc(pairsList) {
    var newData = {}
    for (e in _data) {
      newData[e.key] = e.value
    }
    var i = 0
    while (i < pairsList.count) {
      newData[pairsList[i]] = pairsList[i + 1]
      i = i + 2
    }
    return MalMap.new(newData)
  }
  dissoc(keysList) {
    var newData = {}
    for (e in _data) {
      newData[e.key] = e.value
    }
    for (k in keysList) {
      newData.remove(k)
    }
    return MalMap.new(newData)
  }
  ==(other) {
    if (!(other is MalMap)) return false
    if (other.data.count != data.count) return false
    for (e in _data) {
      if (other.data[e.key] != e.value) return false
    }
    return true
  }
  !=(other) { !(this == other) }
}

class MalNativeFn is MalVal {
  construct new(fn) { _fn = fn }
  call(args) { _fn.call(args) }
  clone() { MalNativeFn.new(_fn) }
}

class MalFn is MalVal {
  construct new(ast, params, env, fn) {
    _ast = ast
    _params = params
    _env = env
    _fn = fn
    _isMacro = false
  }
  construct new(ast, params, env, fn, isMacro) {
    _ast = ast
    _params = params
    _env = env
    _fn = fn
    _isMacro = isMacro
  }
  ast { _ast }
  params { _params }
  env { _env }
  isMacro { _isMacro }
  clone() { MalFn.new(_ast, _params, _env, _fn, _isMacro) }
  makeMacro() { MalFn.new(_ast, _params, _env, _fn, true) }
  call(args) { _fn.call(args) }
}

class MalAtom is MalVal {
  construct new(value) { _value = value }
  value { _value }
  value=(other) { _value = other }
  clone() { MalAtom.new(value) }
}

class MalException {
  static value { __exception }
  static set(exception) { __exception = exception }
}
