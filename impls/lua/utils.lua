local M = {}

function M.try(f, catch_f)
    local status, exception = pcall(f)
    if not status then
        catch_f(exception)
    end
end

function M.instanceOf(subject, super)
    super = tostring(super)
    local mt = getmetatable(subject)

    while true do
        if mt == nil then return false end
        if tostring(mt) == super then return true end
        mt = getmetatable(mt)
    end
end

--[[
function M.isArray(o)
    local i = 0
    for _ in pairs(o) do
        i = i + 1
        if o[i] == nil then return false end
    end
    return true
end
]]--

function M.map(func, obj)
    local new_obj = {}
    for i,v in ipairs(obj) do
        new_obj[i] = func(v)
    end
    return new_obj
end

function M.dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. M.dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

return M
