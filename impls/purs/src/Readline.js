"use strict";

var readlineSync = require('readline-sync')

exports.readLine = function (x) {
  return function () {
    return readlineSync.question(x)
  }
}


exports.argv = process.argv;
