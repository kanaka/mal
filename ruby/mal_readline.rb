require "readline"

$history_loaded = false
$histfile = "#{ENV['HOME']}/.mal-history"

def _readline(prompt)
    if !$history_loaded && File.exist?($histfile)
        $history_loaded = true
        File.readlines($histfile).each {|l| Readline::HISTORY.push(l.chomp)}
    end

    if line = Readline.readline(prompt, true)
        File.open($histfile, 'a+') {|f| f.write(line+"\n")}
        return line
    else
        return nil
    end
end
