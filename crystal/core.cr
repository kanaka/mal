require "./types"
require "./error"
require "./printer"
require "./reader"

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

def self.read_string(args)
  head = args.first.unwrap
  eval_error "argument of read-str must be string" unless head.is_a? String
  read_str head
end

def self.slurp(args)
  head = args.first.unwrap
  eval_error "argument of slurp must be string" unless head.is_a? String
  begin
    File.read head
  rescue e : Errno
    eval_error "no such file"
  end
end

def self.cons(args)
  arg1 = args[1].unwrap
  eval_error "2nd arg of cons must be list" unless arg1.is_a? Array
  arg1.each_with_object(Mal::List.new << args[0]) do |elem, list|
    list << elem
  end
end

def self.concat(args)
  args.each_with_object(Mal::List.new) do |arg, list|
    a = arg.unwrap
    eval_error "arguments of concat must be list" unless a.is_a?(Array)
    a.each{|e| list << e}
  end
end

def self.nth(args)
  a0, a1 = args[0].unwrap, args[1].unwrap
  eval_error "1st argument of nth must be list or vector" unless a0.is_a? Array
  eval_error "2nd argument of nth must be integer" unless a1.is_a? Int32
  a0[a1]
end

def self.first(args)
  a0 = args[0].unwrap

  return nil if a0.nil?
  eval_error "1st argument of first must be list or vector or nil" unless a0.is_a? Array
  a0.empty? ? nil : a0.first
end

def self.rest(args)
  a0 = args[0].unwrap

  return Mal::List.new if a0.nil?
  eval_error "1st argument of first must be list or vector or nil" unless a0.is_a? Array
  return Mal::List.new if a0.empty?
  a0[1..-1].each_with_object(Mal::List.new){|e,l| l << e}
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
  "read-string" => func(:read_string)
  "slurp" => func(:slurp)
  "cons" => func(:cons)
  "concat" => func(:concat)
  "nth" => func(:nth)
  "first" => func(:first)
  "rest" => func(:rest)
} of String => Mal::Func

end
