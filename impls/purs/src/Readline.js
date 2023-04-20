"use strict";

import readlineSync from 'readline-sync'

export const readLine = function (x) {
  return function () {
    const result =  readlineSync.question(x);

    if(readlineSync.getRawInput() === String.fromCharCode(0)){
      return ":q"
    }
    return result;
  }
}


export const argv = process.argv;
