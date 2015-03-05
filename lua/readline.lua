local LN = require('linenoise')

local M = {}

local history_loaded = false
local history_file = os.getenv("HOME") .. "/.mal-history"

M.raw = false

function M.readline(prompt)
    if not history_loaded then
        history_loaded = true
        for line in io.lines(history_file) do
            LN.historyadd(line)
        end
    end

    if M.raw then
        io.write(prompt); io.flush();
        line = io.read()
    else
        line = LN.linenoise(prompt)
    end
    if line then
        LN.historyadd(line)
        local f = io.open(history_file, "a")
        f:write(line.."\n")
        f:close()
    end
    return line
end

return M
