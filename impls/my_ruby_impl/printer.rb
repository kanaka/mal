class Printer
  def self.pr_str(ast)
    case ast
    when MalListType
      pr_list_type(ast)
    when MalModifierType
      pr_modifier_type(ast)
    when MalScalarType, MalOperatorType
      pr_scalar_type(ast)
    end
  end

  def self.pr_list_type(type)
    type.begin_char + type.data.map { |d| pr_str(d) }.join(' ') + type.end_char
  end

  def self.pr_scalar_type(type)
    type.data_str
  end

  def self.pr_modifier_type(type)
    '(' + type.identifier + pr_str(type.data) + ')'
  end
end
