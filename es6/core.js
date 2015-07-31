import * as types from './types';
import { pr_str } from './printer';
import { read_str } from './reader';

// String functions
function do_pr_str(...args) {
    return args.map(exp => pr_str(exp, true)).join(" ");
}

function str(...args) {
    return args.map(exp => pr_str(exp, false)).join("");
}

function prn(...args) {
    console.log.apply({}, args.map(exp => pr_str(exp, true)));
    return null;
}

function println(...args) {
    console.log.apply({}, args.map(exp => pr_str(exp, false)));
    return null;
}

function slurp(f) {
    if (typeof require !== 'undefined') {
        return require('fs').readFileSync(f, 'utf-8');
    } else {
        var req = new XMLHttpRequest();
        req.open("GET", f, false);
        req.send();
        if (req.status == 200) {
            return req.responseText;
        } else {
            throw new Error("Failed to slurp file: " + f);
        }
    }
}

// Sequence functions
function cons(a, b) { return [a].concat(b); }

function concat(lst) {
    lst = lst || [];
    return lst.concat.apply(lst, Array.prototype.slice.call(arguments, 1));
}

// types.ns is namespace of type functions
export const core_ns = new Map([
        ['=', types._equal_Q],

        ['pr-str', do_pr_str],
        ['str', str],
        ['prn', prn],
        ['println', println],
        ['read-string',read_str],
        ['slurp', slurp],

        ['<' , (a,b) => a<b],
        ['<=', (a,b) => a<=b],
        ['>' , (a,b) => a>b],
        ['>=', (a,b) => a>=b],
        ['+' , (a,b) => a+b],
        ['-' , (a,b) => a-b],
        ['*' , (a,b) => a*b],
        ['/' , (a,b) => a/b],
        ["time-ms", () => new Date().getTime()],

        ['list', (...a) => a],
        ['list?', types._list_Q],

        ['cons', (a,b) => [a].concat(b)],
        ['concat', (...a) => a.reduce((x,y) => x.concat(y), [])],
        ['empty?', a => a.length === 0],
        ['count', a => a === null ? 0 : a.length]]);
