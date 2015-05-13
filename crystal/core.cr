require "./types"
require "./error"
require "./printer"

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
  args.first.unwrap.is_a? Mal::List
end

def self.empty?(args)
  a = args.first.unwrap
  a.is_a?(Array) ? a.empty? : false
end

def self.count(args)
  a = args.first.unwrap
  case a
  when Array
    a.size as Int32
  when Nil
    0
  else
    eval_error "invalid argument for function 'count'"
  end
end

def self.pr_str_(args)
  args.map{|a| pr_str(a)}.join(" ")
end

def self.str(args)
  args.map{|a| pr_str(a, false)}.join
end

def self.prn(args)
  puts self.pr_str_(args)
  nil
end

def self.println(args)
  puts args.map{|a| pr_str(a, false)}.join(" ")
  nil
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
  "<" => rel_op(:<)
  ">" => rel_op(:>)
  "<=" => rel_op(:<=)
  ">=" => rel_op(:>=)
  "pr-str" => func(:pr_str_)
  "str" => func(:str)
  "prn" => func(:prn)
  "println" => func(:println)
} of String => Mal::Func

end
