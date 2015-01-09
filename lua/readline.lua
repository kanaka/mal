local LN = require('linenoise')

local M = {}

local history_loaded = false
local history_file = os.getenv("HOME") .. "/.mal-history"

function M.readline(prompt)
    if not history_loaded then
        history_loaded = true
        for line in io.lines(history_file) do
            LN.historyadd(line)
        end
    end

    line = LN.linenoise(prompt)
    if line then
        LN.historyadd(line)
        local f = io.open(history_file, "a")
        f:write(line.."\n")
        f:close()
    end
    return line
end

return M
