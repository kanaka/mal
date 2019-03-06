
import Foundation

enum MalDataType: String {
    case Number, String, List, Vector, HashMap, Symbol, Keyword, Atom, Nil, True, False, Function, Unknown
}

protocol MalData {
    var dataType: MalDataType { get }
    
    var count: Int { get }
    var listForm: [MalData] { get }
}
extension MalData {
    var dataType: MalDataType { // not used
        return MalDataType(rawValue: String(describing: type(of: self))) ?? MalDataType.Unknown
    }
    var count: Int { return 0 }
    var listForm: [MalData] { return [] }
}

typealias Number = Int
typealias List = Array
typealias Vector = ContiguousArray
typealias HashMap = Dictionary

struct Symbol: MalData {
    let dataType = MalDataType.Symbol
    let name: String
    init(_ name: String) {
        self.name = name
    }
}

struct Nil: MalData {
    let dataType = MalDataType.Nil
}

class Atom: MalData {
    let dataType = MalDataType.Atom
    var value: MalData
    init(_ value: MalData) {
        self.value = value
    }
}

struct Function: MalData {
    let dataType = MalDataType.Function
    
    let ast: MalData?
    let params: [Symbol]?
    let env: Env?
    let fn: (([MalData]) throws -> MalData)
    let isMacro: Bool
    let meta: MalData?
    
    init(ast: MalData? = nil, params: [Symbol]? = nil, env: Env? = nil, isMacro: Bool = false, meta: MalData? = nil,
         fn: @escaping ([MalData]) throws -> MalData) {
        self.ast = ast
        self.params = params
        self.env = env
        self.isMacro = isMacro
        self.fn = fn
        self.meta = meta
    }
    init(withFunction function: Function, isMacro: Bool) {
        self.ast = function.ast
        self.params = function.params
        self.env = function.env
        self.fn = function.fn
        self.meta = function.meta
        self.isMacro = isMacro
    }
    init(withFunction function: Function, meta: MalData) {
        self.ast = function.ast
        self.params = function.params
        self.env = function.env
        self.fn = function.fn
        self.isMacro = function.isMacro
        self.meta = meta
    }

}


extension String: MalData {
    var dataType: MalDataType {
        return !self.isEmpty && self[startIndex] == "\u{029E}" ? .Keyword : .String }
}
extension Number: MalData {
    var dataType: MalDataType { return .Number }
}
extension Bool  : MalData {
    var dataType: MalDataType { return self == true ? .True : .False }
}

extension List  : MalData {
    var dataType: MalDataType { return .List }
    var listForm: [MalData] { return self as! [MalData] }
}
extension Vector: MalData {
    var dataType: MalDataType { return .Vector }
    var listForm: [MalData] { return List(self) as! [MalData] }
}
extension ArraySlice: MalData {
    var dataType: MalDataType { return .List }
    var listForm: [MalData] { return List(self) as! [MalData] }
}
extension HashMap: MalData {
    var dataType: MalDataType { return .HashMap }
    static func hashMap(fromList list: [MalData]) throws -> [String: MalData] {
        var hashMap: [String: MalData] = [:]
        for index in stride(from: 0, to: list.count, by: 2) {
            guard list[index] is String, index+1 < list.count else { throw MalError.Error }
            hashMap.updateValue(list[index+1], forKey: list[index] as! String)
        }
        return hashMap
    }
}

// MARK: Errors
enum MalError: Error {
    case ParensMismatch
    case QuotationMarkMismatch
    case EmptyData
    case SymbolNotFound(Symbol)
    case InvalidArgument
    case Error
    case IndexOutOfBounds
    case MalException(MalData)
    func info() -> MalData {
        switch self {
        case .ParensMismatch:
            return "unbalanced parens"
        case .QuotationMarkMismatch:
            return "unbalanced quotation mark"
        case .EmptyData:
            return "empty data"
        case .InvalidArgument:
            return "invalid argument"
        case .SymbolNotFound(let symbol):
            return "'\(symbol.name)' not found"
        case .IndexOutOfBounds:
            return "index out of bounds"
        case .MalException(let data):
            return data
        default:
            return "uncaught error!"
        }
    }
}
