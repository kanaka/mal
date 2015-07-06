require "./types"

def pr_str(value, print_readably = true)
  case value
  when Nil          then "nil"
  when Bool         then value.to_s
  when Int32        then value.to_s
  when Mal::List    then "(#{value.map{|v| pr_str(v, print_readably) as String}.join(" ")})"
  when Mal::Vector  then "[#{value.map{|v| pr_str(v, print_readably) as String}.join(" ")}]"
  when Mal::Symbol  then value.str.to_s
  when Mal::Func    then "<function>"
  when Mal::Closure then "<closure>"
  when Mal::HashMap
    # step1_read_print.cr requires specifying type
    "{#{value.map{|k, v| "#{pr_str(k, print_readably)} #{pr_str(v, print_readably)}" as String}.join(" ")}}"
  when String
    case
    when value.empty?()
      print_readably ? value.inspect : value
    when value[0] == '\u029e'
      ":#{value[1..-1]}"
    else
      print_readably ? value.inspect : value
    end
  when Mal::Atom
    "(atom #{pr_str(value.val, print_readably)})"
  else
    raise "invalid MalType: #{value.to_s}"
  end
end

def pr_str(t : Mal::Type, print_readably = true)
  pr_str(t.unwrap, print_readably) + (t.macro? ? " (macro)" : "")
end
