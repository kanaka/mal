require "./types"
require "./error"

module Mal

macro calc_op(op)
  -> (args : Array(Mal::Type)) {
    x, y = args[0].unwrap, args[1].unwrap
    eval_error "invalid arguments" unless x.is_a?(Int32) && y.is_a?(Int32)
    Mal::Type.new(x {{op.id}} y)
  }
end

def self.list(args)
  args.each_with_object(Mal::List.new){|a,l| l << a}
end

def self.list?(args)
  args.first.unwrap.is_a?(Mal::List)
end

def self.empty?(args)
  a = args.first.unwrap
  a.is_a?(Array) ? a.empty? : false
end

def self.count(args)
  a = args.first
  eval_error "first parameter is not a list or vector" unless a.is_a? Array
  a.size as Int32
end

# Note:
# Simply using ->self.some_func doesn't work
macro func(name)
  -> (args : Array(Mal::Type)) { Mal::Type.new self.{{name.id}}(args) }
end

macro rel_op(op)
  -> (args : Array(Mal::Type)) { Mal::Type.new (args[0] {{op.id}} args[1]) }
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
