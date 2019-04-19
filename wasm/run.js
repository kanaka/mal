#!/usr/bin/env node

// Copyright Joel Martin
// License MIT

const { promisify } = require('util')
const fs = require('fs')
const readFile = promisify(fs.readFile)
const assert = require('assert')
const { TextDecoder, TextEncoder } = require('text-encoding')
const node_readline = require('./node_readline.js')

assert('WebAssembly' in global, 'WebAssembly not detected')

//
// Memory interaction utilities
//

// Convert node Buffer to Uint8Array
function toUint8Array(buf) {
  let u = new Uint8Array(buf.length)
  for (let i = 0; i < buf.length; ++i) {
    u[i] = buf[i]
  }
  return u
}

// Read null terminated string out of webassembly memory
function get_string(memory, addr) {
    //console.warn("get_string:", addr)
    let u8 = new Uint8Array(memory.buffer, addr)
    let length = u8.findIndex(e => e == 0)
    let bytes = new Uint8Array(memory.buffer, addr, length)
    let str = new TextDecoder('utf8').decode(bytes)
    return str
}

// Write null terminated string into webassembly memory
function put_string(memory, addr, str, max_length) {
    let buf8 = new Uint8Array(memory.buffer, addr)

    let bytes = new TextEncoder('utf8').encode(str)
    if (max_length && bytes.length > max_length) {
        bytes = bytes.slice(0, max_length)
    }

    buf8.set(bytes, 0)
    buf8[bytes.length] = 0 // null terminator
    return bytes.length+1
}

// Put argv structure at beginning of memory
function marshal_argv(memory, offset, args) {
    let view = new DataView(memory.buffer, offset)
    let buf8 = new Uint8Array(memory.buffer, offset)

    let stringStart = (args.length + 1) * 4
    for (let i = 0; i < args.length; i++) {
        let len = put_string(memory, stringStart, args[i])
        view.setUint32(i*4, stringStart, true)
        stringStart = stringStart + len
    }
    view.setUint32(args.length*4, 0, true)
    return offset + stringStart // start of free memory
}

// Based on:
// https://gist.github.com/kripken/59c67556dc03bb6d57052fedef1e61ab

// Loads a WebAssembly dynamic library, returns a promise.
async function loadWebAssembly(filename, args) {
  // Fetch the file and compile it
  const wasm_str = await readFile(filename)
  const wasm_bin = toUint8Array(wasm_str)
  const module = await WebAssembly.compile(wasm_bin)
  let memory = new WebAssembly.Memory({ initial: 256 })
  // Core imports
  function printline(addr, stream) {
      console.log(get_string(memory, addr).replace(/\n$/, ''))
  }

  // Returns addr on success and -1 on failure
  // Truncates to max_length
  function readline(prompt, addr, max_length) {
      let line = node_readline.readline(get_string(memory, prompt))
      if (line === null) { return 0 }
      put_string(memory, addr, line, max_length)
      return 1
  }

  function read_file(path_addr, buf) {
      let path = get_string(memory, path_addr)
      let contents = fs.readFileSync(path, 'utf8')
      return put_string(memory, buf, contents)
  }

  function get_time_ms() {
      // subtract 30 years to make sure it fits into i32 without
      // wrapping or becoming negative
      return (new Date()).getTime() - 0x38640900
  }

  // Marshal arguments
  const memoryStart = 0
  let memoryBase = marshal_argv(memory, memoryStart, args)
  memoryBase = memoryBase + (8 - (memoryBase % 8))

  // Create the imports for the module, including the
  // standard dynamic library imports
  imports = {}
  imports.env = {}
  imports.env.exit = process.exit
  imports.env.printline = printline
  imports.env.readline = readline
  imports.env.read_file = read_file
  imports.env.get_time_ms = get_time_ms

  imports.env.stdout = 0
  imports.env.fputs = printline

  imports.env.memory = memory
  imports.env.memoryBase = memoryBase
  imports.env.table = new WebAssembly.Table({ initial: 0, element: 'anyfunc' })
  imports.env.tableBase = imports.env.tableBase || 0
  // Create the instance.
  return [new WebAssembly.Instance(module, imports), args.length, 0]
}

async function main() {
  assert(process.argv.length >= 3,
         'Usage: ./run.js prog.wasm [ARGS...]')

  const wasm = process.argv[2]
  const args = process.argv.slice(2)
  const [instance, argc, argv] = await loadWebAssembly(wasm, args)

  let exports = instance.exports
  assert(exports, 'no exports found')
  assert('_main' in exports, '_main not found in wasm module exports')
  if ('__post_instantiate' in exports) {
      //console.warn('calling exports.__post_instantiate()')
      exports['__post_instantiate']()
  }
  //console.warn(`calling exports._main(${argc}, ${argv})`)
  let start = new Date()
  let res = exports['_main'](argc, argv)
  let end = new Date()
  //console.warn('runtime: ' + (end-start) + 'ms')
  process.exit(res)
}

if (module.parent) {
    module.exports.loadWebAssembly = loadWebAssembly
} else {
    main()
}
