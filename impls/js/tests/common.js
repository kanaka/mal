fs = require('fs');
assert = require('assert');

function assert_eq(a, b) {
    GLOBAL.assert.deepEqual(a, b, a + " !== " + b);
}

function load(file) {
    console.log(process.cwd());
    //process.chdir('../');
    eval(fs.readFileSync(file,'utf8'));
}

exports.assert_eq = assert_eq;
exports.load = load;
