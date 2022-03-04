require_relative "errors"
require_relative "types"

module Mal
  extend self

  def pr_str(mal, print_readably = false)
    case mal
    when Types::List
      "(#{mal.map { |m| pr_str(m, print_readably) }.join(" ")})"
    when Types::Vector
      "[#{mal.map { |m| pr_str(m, print_readably) }.join(" ")}]"
    when Types::Hashmap
      "{#{mal.map { |k, v| [pr_str(k, print_readably), pr_str(v, print_readably)].join(" ") }.join(" ")}}"
    when Types::Keyword
      if print_readably
        pr_str_keyword(mal)
      else
        ":#{mal.value}"
      end
    when Types::String
      if print_readably
        pr_str_string(mal)
      else
        mal.value
      end
    when Types::Atom
      "(atom #{pr_str(mal.value, print_readably)})"
    when Types::Base, Types::Callable
      mal.inspect
    else
      raise InvalidTypeError, "unable to print value <#{mal.inspect}>"
    end
  end

  def pr_str_keyword(mal)
    value = mal.value.dup

    value.gsub!('\\','\\\\\\\\')
    value.gsub!("\n",'\n')
    value.gsub!('"','\"')

    ":#{value}"
  end

  def pr_str_string(mal)
    value = mal.value.dup

    value.gsub!('\\','\\\\\\\\')
    value.gsub!("\n",'\n')
    value.gsub!('"','\"')

    "\"#{value}\""
  end
end
