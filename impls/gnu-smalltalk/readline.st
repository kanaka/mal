DLD addLibrary: 'libreadline'.
DLD addLibrary: 'libhistory'.

Object subclass: ReadLine [
    ReadLine class >> readLine: prompt [
        <cCall: 'readline' returning: #stringOut args: #(#string)>
    ]

    ReadLine class >> addHistory: item [
        <cCall: 'add_history' returning: #void args: #(#string)>
    ]

    ReadLine class >> readHistory: filePath [
        <cCall: 'read_history' returning: #int args: #(#string)>
    ]

    ReadLine class >> writeHistory: filePath [
        <cCall: 'write_history' returning: #int args: #(#string)>
    ]
]
