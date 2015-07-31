import * as types from './types';
import { pr_str } from './printer';

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


// types.ns is namespace of type functions
export const core_ns = new Map([
        ['=', types._equal_Q],

        ['pr-str', do_pr_str],
        ['str', str],
        ['prn', prn],
        ['println', println],

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

        ['empty?', a => a.length === 0],
        ['count', a => a === null ? 0 : a.length]]);
