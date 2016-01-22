require "time"

require "./types"
require "./error"
require "./printer"
require "./reader"
require "./readline"

module Mal

macro calc_op(op)
  -> (args : Array(Mal::Type)) {
    x, y = args[0].unwrap, args[1].unwrap
    eval_error "invalid arguments for binary operator {{op.id}}" unless x.is_a?(Int32) && y.is_a?(Int32)
    Mal::Type.new(x {{op.id}} y)
  }
end

def self.list(args)
  args.to_mal
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
  head, tail = args[0] as Mal::Type, args[1].unwrap
  eval_error "2nd arg of cons must be list" unless tail.is_a? Array
  ([head] + tail).to_mal
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
  a0[1..-1].to_mal
end

def self.apply(args)
  eval_error "apply must take at least 2 arguments" unless args.size >= 2

  head = args.first.unwrap
  last = args.last.unwrap

  eval_error "last argument of apply must be list or vector" unless last.is_a? Array

  case head
  when Mal::Closure
    head.fn.call(args[1..-2] + last)
  when Mal::Func
    head.call(args[1..-2] + last)
  else
    eval_error "1st argument of apply must be function or closure"
  end
end

def self.map(args)
  func = args.first.unwrap
  list = args[1].unwrap

  eval_error "2nd argument of map must be list or vector" unless list.is_a? Array

  f = case func
      when Mal::Closure then func.fn
      when Mal::Func    then func
      else                   eval_error "1st argument of map must be function"
      end

  list.each_with_object(Mal::List.new) do |elem, mapped|
    mapped << f.call([elem])
  end
end

def self.nil?(args)
  args.first.unwrap.nil?
end

def self.true?(args)
  a = args.first.unwrap
  a.is_a?(Bool) && a
end

def self.false?(args)
  a = args.first.unwrap
  a.is_a?(Bool) && !a
end

def self.symbol?(args)
  args.first.unwrap.is_a?(Mal::Symbol)
end

def self.symbol(args)
  head = args.first.unwrap
  eval_error "1st argument of symbol function must be string" unless head.is_a? String
  Mal::Symbol.new head
end

def self.keyword(args)
  head = args.first.unwrap
  eval_error "1st argument of symbol function must be string" unless head.is_a? String
  "\u029e" + head
end

def self.keyword?(args)
  head = args.first.unwrap
  head.is_a?(String) && !head.empty? && head[0] == '\u029e'
end

def self.vector(args)
  args.to_mal(Mal::Vector)
end

def self.vector?(args)
  args.first.unwrap.is_a? Mal::Vector
end

def self.hash_map(args)
  eval_error "hash-map must take even number of arguments" unless args.size.even?
  map = Mal::HashMap.new
  args.each_slice(2) do |kv|
    k = kv[0].unwrap
    eval_error "key must be string" unless k.is_a? String
    map[k] = kv[1]
  end
  map
end

def self.map?(args)
  args.first.unwrap.is_a? Mal::HashMap
end

def self.assoc(args)
  head = args.first.unwrap
  eval_error "1st argument of assoc must be hashmap" unless head.is_a? Mal::HashMap
  eval_error "assoc must take a list and even number of arguments" unless (args.size - 1).even?

  map = Mal::HashMap.new
  head.each{|k, v| map[k] = v}

  args[1..-1].each_slice(2) do |kv|
    k = kv[0].unwrap
    eval_error "key must be string" unless k.is_a? String
    map[k] = kv[1]
  end

  map
end

def self.dissoc(args)
  head = args.first.unwrap
  eval_error "1st argument of assoc must be hashmap" unless head.is_a? Mal::HashMap

  map = Mal::HashMap.new
  head.each{|k,v| map[k] = v}

  args[1..-1].each do |arg|
    key = arg.unwrap
    eval_error "key must be string" unless key.is_a? String
    map.delete key
  end

  map
end

def self.get(args)
  a0, a1 = args[0].unwrap, args[1].unwrap
  return nil unless a0.is_a? Mal::HashMap
  eval_error "2nd argument of get must be string" unless a1.is_a? String

  # a0[a1]? isn't available because type ofa0[a1] is infered NoReturn
  a0.has_key?(a1) ? a0[a1] : nil
end

def self.contains?(args)
  a0, a1 = args[0].unwrap, args[1].unwrap
  eval_error "1st argument of get must be hashmap" unless a0.is_a? Mal::HashMap
  eval_error "2nd argument of get must be string" unless a1.is_a? String
  a0.has_key? a1
end

def self.keys(args)
  head = args.first.unwrap
  eval_error "1st argument of assoc must be hashmap" unless head.is_a? Mal::HashMap
  head.keys.each_with_object(Mal::List.new){|e,l| l << Mal::Type.new(e)}
end

def self.vals(args)
  head = args.first.unwrap
  eval_error "1st argument of assoc must be hashmap" unless head.is_a? Mal::HashMap
  head.values.to_mal
end

def self.sequential?(args)
  args.first.unwrap.is_a? Array
end

def self.readline(args)
  head = args.first.unwrap
  eval_error "1st argument of readline must be string" unless head.is_a? String
  my_readline head
end

def self.meta(args)
  m = args.first.meta
  m.nil? ? nil : m
end

def self.with_meta(args)
  t = args.first.dup
  t.meta = args[1]
  t
end

def self.atom(args)
  Mal::Atom.new args.first
end

def self.atom?(args)
  args.first.unwrap.is_a? Mal::Atom
end

def self.deref(args)
  head = args.first.unwrap
  eval_error "1st argument of deref must be atom" unless head.is_a? Mal::Atom
  head.val
end

def self.reset!(args)
  head = args.first.unwrap
  eval_error "1st argument of reset! must be atom" unless head.is_a? Mal::Atom
  head.val = args[1]
end

def self.swap!(args)
  atom = args.first.unwrap
  eval_error "1st argument of swap! must be atom" unless atom.is_a? Mal::Atom

  a = [atom.val] + args[2..-1]

  func = args[1].unwrap
  case func
  when Mal::Func
    atom.val = func.call a
  when Mal::Closure
    atom.val = func.fn.call a
  else
    eval_error "2nd argumetn of swap! must be function"
  end
end

def self.conj(args)
  seq = args.first.unwrap
  case seq
  when Mal::List
    (args[1..-1].reverse + seq).to_mal
  when Mal::Vector
    (seq + args[1..-1]).to_mal(Mal::Vector)
  else
    eval_error "1st argument of conj must be list or vector"
  end
end

def self.time_ms(args)
  Time.now.epoch_ms.to_i32
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
  "+"           => calc_op(:+),
  "-"           => calc_op(:-),
  "*"           => calc_op(:*),
  "/"           => calc_op(:/),
  "list"        => func(:list),
  "list?"       => func(:list?),
  "empty?"      => func(:empty?),
  "count"       => func(:count),
  "="           => rel_op(:==),
  "<"           => rel_op(:<),
  ">"           => rel_op(:>),
  "<="          => rel_op(:<=),
  ">="          => rel_op(:>=),
  "pr-str"      => func(:pr_str_),
  "str"         => func(:str),
  "prn"         => func(:prn),
  "println"     => func(:println),
  "read-string" => func(:read_string),
  "slurp"       => func(:slurp),
  "cons"        => func(:cons),
  "concat"      => func(:concat),
  "nth"         => func(:nth),
  "first"       => func(:first),
  "rest"        => func(:rest),
  "throw"       => -> (args : Array(Mal::Type)) { raise Mal::RuntimeException.new args[0] },
  "apply"       => func(:apply),
  "map"         => func(:map),
  "nil?"        => func(:nil?),
  "true?"       => func(:true?),
  "false?"      => func(:false?),
  "symbol?"     => func(:symbol?),
  "symbol"      => func(:symbol),
  "keyword"     => func(:keyword),
  "keyword?"    => func(:keyword?),
  "vector"      => func(:vector),
  "vector?"     => func(:vector?),
  "hash-map"    => func(:hash_map),
  "map?"        => func(:map?),
  "assoc"       => func(:assoc),
  "dissoc"      => func(:dissoc),
  "get"         => func(:get),
  "contains?"   => func(:contains?),
  "keys"        => func(:keys),
  "vals"        => func(:vals),
  "sequential?" => func(:sequential?),
  "readline"    => func(:readline),
  "meta"        => func(:meta),
  "with-meta"   => func(:with_meta),
  "atom"        => func(:atom),
  "atom?"       => func(:atom?),
  "deref"       => func(:deref),
  "deref"       => func(:deref),
  "reset!"      => func(:reset!),
  "swap!"       => func(:swap!),
  "conj"        => func(:conj),
  "time-ms"     => func(:time_ms),
} of String => Mal::Func

end
