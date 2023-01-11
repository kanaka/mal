Reader = require "reader"
Printer = require "printer"

types = require "types"
--local throw = types.throw
local Err = types.Err
local is_instanceOf = types.isinstanceof

function raw_read(prompt)
  io.write(prompt)
  local v = io.read()
  if v == nil then
    io.write('\n')
  end
  return v
end


function READ(v)
  return  Reader.read_str(v)
end

function EVAL(a)
  return a
end

function PRINT(a)
  print(Printer.stringfy_val(a, true))
end

function rep(str)
  return PRINT(EVAL(READ(str)))
end



function main()
    local line = ''
    while true do
      local line = raw_read('user> ')
      if line == nil then
        break
      end
      status, err = pcall( function () print(rep(line)) end)
      if not status then
        if is_instanceOf(err, Err) then
          err = Printer.stringfy_val(err)
        end
        print(err)
        print(debug.traceback())
      end
    end
end

main()
