common = require('./common.js');
var assert_eq = common.assert_eq;
var rep = require('../step5_tco.js').rep;

console.log("Testing Stack Exhaustion Function");
rep('(def! sum-to (fn* (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))');
try {
    rep('(sum-to 10000)');
    throw new Error("Did not get expected stack exhaustion");
} catch (e) {
    if (e.toString().match(/RangeError/)) {
        console.log("Got expected stack exhaustion");
    } else {
        throw new Error("Unexpected error: " + e);
    }
}

console.log("Testing Tail Call Optimization/Elimination");
rep('(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))');
rep('(sum2 10000 0)');

console.log("All tests completed");
