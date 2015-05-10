require "./types"

module Mal

def self.error(msg)
  raise Mal::EvalException.new msg
end

macro calc_op(op)
  -> (args : Array(Mal::Type)) {
    x, y = args[0], args[1]
    raise Mal::EvalException.new "invalid arguments" unless x.is_a?(Int32) && y.is_a?(Int32)
    (x {{op.id}} y) as Mal::Type
  } as Mal::Func
end

def self.list(args)
  args.each_with_object(Mal::List.new){|a,l| l << a}
end

def self.list?(args)
  args.first.is_a?(Mal::List)
end

def self.empty?(args)
  a = args.first
  a.is_a?(Array) ? a.empty? : false
end

def self.count(args)
  a = args.first
  raise Mal::EvalException.new "first parameter is not a list or vector" unless a.is_a?(Array)
  a.size
end

# Note:
# Simply using ->self.some_func doesn't work
macro func(name)
  -> (args : Array(Mal::Type)) { self.{{name.id}}(args) as Mal::Type } as Mal::Func
end

macro rel_op(op)
  -> (args : Array(Mal::Type)) { (args[0] {{op.id}} args[1]) as Mal::Type } as Mal::Func
end

NS = {
  "+" => calc_op(:+)
  "-" => calc_op(:-)
  "*" => calc_op(:*)
  "/" => calc_op(:/)
  "list" => func(:list)
  "list?" => func(:list?)
  "empty?" => func(:empty?)
  "count" => func(:count)
  "=" => rel_op(:==)
} of String => Mal::Func

end
