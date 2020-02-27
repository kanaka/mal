
import Foundation

func READ(_ input: String) throws -> MalData {
    return try read_str(input)
}

func EVAL(_ anAst: MalData, env anEnv: Env) throws -> MalData {
    var ast = anAst, env = anEnv
    while true {
        switch ast.dataType {
        case .List:
            let list = ast as! [MalData]
            guard !list.isEmpty else { return list }
            if let sym = list[0] as? Symbol {
                switch sym.name {
                case "def!":
                    let value = try EVAL(list[2], env: env), key = list[1] as! Symbol
                    env.set(value, forKey: key)
                    return value
                case "let*":
                    let newEnv = Env(outer: env), expr = list[2]
                    let bindings = list[1].listForm
                    for i in stride(from: 0, to: bindings.count-1, by: 2) {
                        let key = bindings[i], value = bindings[i+1]
                        let result = try EVAL(value, env: newEnv)
                        newEnv.set(result, forKey: key as! Symbol)
                    }
                    env = newEnv
                    ast = expr
                    continue
                case "do":
                    try _ = list.dropFirst().dropLast().map { try EVAL($0, env: env) }
                    ast = list.last ?? Nil()
                    continue
                case "if":
                    let predicate = try EVAL(list[1], env: env)
                    if predicate as? Bool == false || predicate is Nil {
                        ast = list.count>3 ? list[3] : Nil()
                    } else {
                        ast = list[2]
                    }
                    continue
                case "fn*":
                    let fn = {(params: [MalData]) -> MalData in
                        let newEnv = Env(binds: (list[1].listForm as! [Symbol]), exprs: params, outer: env)
                        return try EVAL(list[2], env: newEnv)
                    }
                    return Function(ast: list[2], params: (list[1].listForm as! [Symbol]), env:env , fn: fn)

                default:
                    break
                }
            }
            // not a symbol. maybe: function, list, or some wrong type
            let evaluated = try eval_ast(list, env: env) as! [MalData]
            guard let function = evaluated[0] as? Function else {
                throw MalError.SymbolNotFound(list[0] as? Symbol ?? Symbol("Symbol"))
            }
            if let fnAst = function.ast { // a full fn
                ast = fnAst
                env = Env(binds: function.params!, exprs: evaluated.dropFirst().listForm, outer: function.env!)
            } else { // normal function
                return try function.fn(evaluated.dropFirst().listForm)
            }
            continue
/* fn 的尾递归优化
fn 的语法形式： （（fn （a，b）（+ a b ）） 1 2） 形参，函数体，实参
fn 本来的实现。
    1.生成：制造一个闭包
            1.1 闭包的功能：读入实参， 建立 形参=实参 的环境，在这个环境中 求值函数体
            1.2 闭包本身不带有环境，当求值闭包时使用当时的环境
    2.使用：
            以使用时的环境，使用实参调用闭包，闭包的返回值作为返回值。over （一次函数调用）
fn 的 TCO 实现。
    1.生成: 形参 函数体 闭包（闭包包含最初的形参和函数体）+ 生成fn时的环境
    2.使用：
            取出 函数体，
            使用求值时的形参，以 fn 中的 env 为外层 env 建立环境 （）
            通过循环，在新建的环境中求值函数体
 */
        case .Vector:
            let vector = ast as! ContiguousArray<MalData>
            return try ContiguousArray(vector.map { element in try EVAL(element, env: env) })
        case .HashMap:
            let hashMap = ast as! HashMap<String, MalData>
            return try hashMap.mapValues { value in try EVAL(value, env: env) }
        default:
            return try eval_ast(ast, env: env)
        }
    }
}


func PRINT(_ input: MalData) -> String {
    return pr_str(input, print_readably: true)
}

@discardableResult func rep(_ input: String, env: Env) throws -> String {
    return try PRINT(EVAL(READ(input), env: env))
}

func eval_ast(_ ast: MalData, env: Env) throws -> MalData {
    switch ast.dataType {
    case .Symbol:
        let sym = ast as! Symbol
        if let function =  try? env.get(forKey: sym) {
            return function
        } else {
            throw MalError.SymbolNotFound(sym)
        }
    case .List:
        let list = ast as! [MalData]
        return try list.map { element in try EVAL(element, env: env) }
    default:
        return ast
    }
}


var repl_env = Env()
for (key, value) in ns {
    repl_env.set(Function(fn: value), forKey: Symbol(key))
}
try _ = rep("(def! not (fn* (a) (if a false true)))", env: repl_env)

while true {
    print("user> ", terminator: "")
    if let input = readLine(strippingNewline: true) {
        guard input != "" else { continue }
        do {
            try print(rep(input, env: repl_env))
        } catch let error as MalError {
            print(error.info())
        }
    } else {
        exit(0);
    }
}
