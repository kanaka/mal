package readline

/*
// IMPORTANT: choose one
#cgo LDFLAGS: -ledit
//#cgo LDFLAGS: -lreadline // NOTE: libreadline is GPL

// free()
#include <stdlib.h>
// readline()
#include <stdio.h> // FILE *
#include <readline/readline.h>
// add_history()
#include <readline/history.h>
*/
import "C"

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"unsafe"
)

var HISTORY_FILE = ".mal-history"
var history_path string

func loadHistory(filename string) error {
	content, err := ioutil.ReadFile(history_path)
	if err != nil {
		return err
	}

	for _, add_line := range strings.Split(string(content), "\n") {
		if add_line == "" {
			continue
		}
		c_add_line := C.CString(add_line)
		C.add_history(c_add_line)
		C.free(unsafe.Pointer(c_add_line))
	}

	return nil
}

func init() {
	history_path = filepath.Join(os.Getenv("HOME"), HISTORY_FILE)
	loadHistory(history_path)
}

func Readline(prompt string) (string, error) {
	c_prompt := C.CString(prompt)
	defer C.free(unsafe.Pointer(c_prompt))

	c_line := C.readline(c_prompt)
	defer C.free(unsafe.Pointer(c_line))
	line := C.GoString(c_line)

	if c_line == nil {
		return "", errors.New("C.readline call failed")
	}
	C.add_history(c_line)

	// append to file
	f, e := os.OpenFile(history_path, os.O_APPEND|os.O_WRONLY, 0600)
	if e == nil {
		defer f.Close()

		_, e = f.WriteString(line + "\n")
		if e != nil {
			fmt.Printf("error writing to history")
		}
	}

	return line, nil
}
