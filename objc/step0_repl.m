#import <Foundation/Foundation.h>

#import "mal_readline.h"

NSString *READ(NSString *str) {
    return str;
}

NSString *EVAL(NSString *ast, NSString *env) {
    return ast;
}

NSString *PRINT(NSString *exp) {
    return exp;
}

NSString *REP(NSString *line) {
    return PRINT(EVAL(READ(line), @""));
}

int main () {
    // Create an autorelease pool to manage the memory into the program
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // If using automatic reference counting (ARC), use @autoreleasepool instead:
//    @autoreleasepool {

    while (true) {
        char *rawline = _readline("user> ");
        if (!rawline) { break; }
        NSString *line = [NSString stringWithUTF8String:rawline];
        if ([line length] == 0) { continue; }
        printf("%s\n", [[REP(line) description] UTF8String]);
    }

    [pool drain];

//    }
}
