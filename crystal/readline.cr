# Note:
# Crystal already has "readline" library.
# I implemented a subset of it again for practice.

@[Link("readline")]
lib LibReadline
  fun readline(prompt : UInt8*) : UInt8*
  fun add_history(line : UInt8*)
end

def my_readline(prompt = "")
  line = LibReadline.readline(prompt)
  if line
    LibReadline.add_history(line)
    String.new(line)
  else
    nil
  end
ensure
  LibC.free(line as Void*) if line
end
