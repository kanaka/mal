MalReadline := Object clone do (
    historyLoaded := false
    historyFile := (System getEnvironmentVariable("HOME")) .. "/.mal-history"

    loadHistory := method(
        if(File exists(historyFile), ReadLine loadHistory(historyFile))
        historyLoaded = true
    )

    readLine := method(prompt,
        if(historyLoaded not, loadHistory)
        line := ReadLine readLine(prompt)
        if(line isNil, return(nil))
        if(line isEmpty, return(line))
        ReadLine addHistory(line)
        ReadLine saveHistory(historyFile)
        line
    )
)
