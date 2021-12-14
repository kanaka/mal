"use strict";

var readlineSync = require('readline-sync')

exports.readLine = function (x) {
  return function () {
    const result =  readlineSync.question(x);

    if(readlineSync.getRawInput() === String.fromCharCode(0)){
      return ":q"
    }
    return result;
  }
}


exports.argv = process.argv;