common = require('./common.js');
var assert_eq = common.assert_eq;
var types = require('../types.js');
var core = require('../core.js');
var env = require('../env.js');
var symbol = types._symbol,
    hash_map = core.ns['hash-map'],
    hash_map_Q = core.ns['map?'],
    assoc = core.ns['assoc'],
    dissoc = core.ns['dissoc'],
    get = core.ns['get'],
    contains_Q = core.ns['contains?'],
    count = core.ns['count'],
    equal_Q = core.ns['='];


console.log("Testing hash_maps");
X = hash_map();
assert_eq(true, hash_map_Q(X));

assert_eq(null, get(X,'a'));
assert_eq(false, contains_Q(X, 'a'));
X1 = assoc(X, 'a', "value of X a");
assert_eq(null, get(X,'a'));
assert_eq(false, contains_Q(X, 'a'));
assert_eq("value of X a", get(X1, 'a'));
assert_eq(true, contains_Q(X1, 'a'));

Y = hash_map();
assert_eq(0, count(Y));
Y1 = assoc(Y, 'a', "value of Y a");
assert_eq(1, count(Y1));
Y2 = assoc(Y1, 'b', "value of Y b");
assert_eq(2, count(Y2));
assert_eq("value of Y a", get(Y2, 'a'));
assert_eq("value of Y b", get(Y2, 'b'));

X2 = assoc(X1, 'b', Y2);
assert_eq(2, count(Y2));

assert_eq(true, hash_map_Q(get(X2,'b')));

assert_eq('value of Y a', get(get(X2,'b'),'a'));
assert_eq('value of Y b', get(get(X2,'b'),'b'));

Y3 = dissoc(Y2, 'a');
assert_eq(2, count(Y2));
assert_eq(1, count(Y3));
assert_eq(null, get(Y3, 'a'));
Y4 = dissoc(Y3, 'b');
assert_eq(0, count(Y4));
assert_eq(null, get(Y4, 'b'));


console.log("Testing equal? function");
assert_eq(true,  equal_Q(2,2));
assert_eq(false, equal_Q(2,3));
assert_eq(false, equal_Q(2,3));
assert_eq(true,  equal_Q("abc","abc"));
assert_eq(false, equal_Q("abc","abz"));
assert_eq(false, equal_Q("zbc","abc"));
assert_eq(true,  equal_Q(symbol("abc"),symbol("abc")));
assert_eq(false, equal_Q(symbol("abc"),symbol("abz")));
assert_eq(false, equal_Q(symbol("zbc"),symbol("abc")));
L6 = [1, 2, 3];
L7 = [1, 2, 3];
L8 = [1, 2, "Z"];
L9 = ["Z", 2, 3];
L10 = [1, 2];
assert_eq(true,  equal_Q(L6, L7));
assert_eq(false, equal_Q(L6, L8));
assert_eq(false, equal_Q(L6, L9));
assert_eq(false, equal_Q(L6, L10));
assert_eq(false, equal_Q(L10, L6));


console.log("Testing ENV (1 level)")
env1 = new env.Env();
assert_eq('val_a',env1.set('a','val_a'));
assert_eq('val_b',env1.set('b','val_b'));
assert_eq('val_eq',env1.set('=','val_eq'));
assert_eq('val_a',env1.get('a'));
assert_eq('val_b',env1.get('b'));
assert_eq('val_eq',env1.get('='));

console.log("Testing ENV (2 levels)");
env2 = new env.Env(env1);
assert_eq('val_b2',env2.set('b','val_b2'));
assert_eq('val_c',env2.set('c','val_c'));
assert_eq(env1,env2.find('a'));
assert_eq(env2,env2.find('b'));
assert_eq(env2,env2.find('c'));
assert_eq('val_a', env2.get('a'));
assert_eq('val_b2',env2.get('b'));
assert_eq('val_c', env2.get('c'));

