//******************************************************************************
// MAL - readline
//******************************************************************************

import Foundation

private let HISTORY_FILE = "~/.mal-history"

func with_history_file(do_to_history_file:(UnsafePointer<Int8>) -> ()) {
    HISTORY_FILE.withCString {
        (c_str) -> () in
        let abs_path = tilde_expand(UnsafeMutablePointer<Int8>(c_str))
        if abs_path != nil {
            do_to_history_file(abs_path)
            free(abs_path)
        }
    }
}

func load_history_file() {
    using_history()
    with_history_file {
        let _ = read_history($0)
    }
}

func save_history_file() {
    // Do this? stifle_history(1000)
    with_history_file {
        let _ = write_history($0)
    }
}

func _readline(prompt: String) -> String? {
    let line = prompt.withCString {
        (c_str) -> UnsafeMutablePointer<Int8> in
        return readline(c_str)
    }
    if line != nil {
        if let result = String(UTF8String: line) {
            add_history(line)
            return result
        }
    }
    return nil
}
