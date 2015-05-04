require "./types"

def pr_str(value)
  case value
  when Nil         then "nil"
  when Bool        then value.to_s
  when Int32       then value.to_s
  when String      then value.inspect
  when Array       then "(#{value.map{|v| pr_str(v) as String}.join(" ")})"
  when Mal::Symbol then value.val.to_s
  else                  raise "invalid MalType: #{value.to_s}"
  end
end
