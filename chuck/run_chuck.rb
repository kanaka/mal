#!/usr/bin/env ruby

cmdline = ['chuck', '--caution-to-the-wind']
cmdline << ARGV.shift if ARGV[0] == '--silent'

scriptfile = ARGV[0]
script = File.readlines(scriptfile)
imports = script.grep(%r{^ *// *@import (.+)}) { $1 }
import_files = imports.flat_map { |i| Dir[i] }
cmdline += import_files
cmdline << scriptfile

ENV['CHUCK_ARGS'] = ARGV.drop(1).join("\a")
exec(*cmdline)
