common = require('./common.js');
types = require('../types');
reader = require('../reader');
core = require('../core');
var assert_eq = common.assert_eq,
    read_str = reader.read_str,
    nth = core.ns.nth;

console.log("Testing read of constants/strings");
assert_eq(2,read_str('2'));
assert_eq(12345,read_str('12345'));
assert_eq(12345,read_str('12345 "abc"'));
assert_eq('abc',read_str('"abc"'));
assert_eq('a string (with parens)',read_str('"a string (with parens)"'));

console.log("Testing read of symbols");
assert(types._symbol_Q(read_str('abc')));
assert_eq('abc',read_str('abc').value);
assert_eq('.',read_str('.').value);

console.log("Testing READ_STR of strings");
assert_eq('a string',read_str('"a string"'));
assert_eq('a string (with parens)',read_str('"a string (with parens)"'));
assert_eq('a string',read_str('"a string"()'));
assert_eq('a string',read_str('"a string"123'));
assert_eq('a string',read_str('"a string"abc'));
assert_eq('',read_str('""'));
assert_eq('abc ',read_str('"abc "'));
assert_eq(' abc',read_str('" abc"'));
assert_eq('$abc',read_str('"$abc"'));
assert_eq('abc$()',read_str('"abc$()"'));
assert_eq('"xyz"',read_str('"\\"xyz\\""'));


console.log("Testing READ_STR of lists");
assert_eq(2,core.ns.count(read_str('(2 3)')));
assert_eq(2,core.ns.first(read_str('(2 3)')));
assert_eq(3,core.ns.first(core.ns.rest(read_str('(2 3)'))));
L = read_str('(+ 1 2 "str1" "string (with parens) and \'single quotes\'")');
assert_eq(5,core.ns.count(L));
assert_eq('str1',nth(L,3));
assert_eq('string (with parens) and \'single quotes\'',nth(L,4));
assert_eq([2,3],read_str('(2 3)'));
assert_eq([2,3, 'string (with parens)'],read_str('(2 3 "string (with parens)")'));


console.log("Testing READ_STR of quote/quasiquote");
assert_eq('quote',nth(read_str('\'1'),0).value);
assert_eq(1,nth(read_str('\'1'),1));
assert_eq('quote',nth(read_str('\'(1 2 3)'),0).value);
assert_eq(3,nth(nth(read_str('\'(1 2 3)'),1),2));

assert_eq('quasiquote',nth(read_str('`1'),0).value);
assert_eq(1,nth(read_str('`1'),1));
assert_eq('quasiquote',nth(read_str('`(1 2 3)'),0).value);
assert_eq(3,nth(nth(read_str('`(1 2 3)'),1),2));

assert_eq('unquote',nth(read_str('~1'),0).value);
assert_eq(1,nth(read_str('~1'),1));
assert_eq('unquote',nth(read_str('~(1 2 3)'),0).value);
assert_eq(3,nth(nth(read_str('~(1 2 3)'),1),2));

assert_eq('splice-unquote',nth(read_str('~@1'),0).value);
assert_eq(1,nth(read_str('~@1'),1));
assert_eq('splice-unquote',nth(read_str('~@(1 2 3)'),0).value);
assert_eq(3,nth(nth(read_str('~@(1 2 3)'),1),2));


console.log("All tests completed");
