#import <Foundation/Foundation.h>

#import "mal_readline.h"
#import "types.h"
#import "reader.h"
#import "printer.h"

// read
NSObject *READ(NSString *str) {
    return read_str(str);
}

// eval

// forward declaration
NSObject *EVAL(NSObject *ast, NSDictionary *env);

NSObject *eval_ast(NSObject *ast, NSDictionary *env) {
    if ([ast isMemberOfClass:[MalSymbol class]]) {
        if ([env objectForKey:ast]) {
            return env[ast];
        } else {
            @throw [NSString stringWithFormat:@"'%@' not found", ast];
        }
    } else if ([ast isKindOfClass:[NSArray class]]) {
        NSMutableArray *newLst = [NSMutableArray array];
        for (NSObject * x in (NSArray *)ast) {
            [newLst addObject:EVAL(x, env)];
        }
        if ([ast isKindOfClass:[MalVector class]]) {
            return [MalVector fromArray:newLst];
        } else {
            return newLst;
        }
    } else if ([ast isKindOfClass:[NSDictionary class]]) {
        NSMutableDictionary *newDict = [NSMutableDictionary dictionary];
        for (NSString * k in (NSDictionary *)ast) {
            newDict[k] = EVAL(((NSDictionary *)ast)[k], env);
        }
        return newDict;
    } else {
        return ast;
    }
}

NSObject *EVAL(NSObject *ast, NSDictionary *env) {
    //NSLog(@"EVAL: %@", ast);
    if (!list_Q(ast)) {
        return eval_ast(ast, env);
    }

    NSArray * el = (NSArray *) eval_ast(ast, env);
    NSObject * (^ f)(NSArray *) = el[0];
    NSArray * args = [el subarrayWithRange:NSMakeRange(1, [el count] - 1)];
    return f(args);
}

// print
NSString *PRINT(NSObject *exp) {
    return _pr_str(exp, true);
}

// REPL
NSString *REP(NSString *line, NSDictionary *env) {
    return PRINT(EVAL(READ(line), env));
}

int main (int argc, const char * argv[]) {
    // Create an autorelease pool to manage the memory into the program
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // If using automatic reference counting (ARC), use @autoreleasepool instead:
//    @autoreleasepool {

    NSDictionary * repl_env = @{
        @"+": ^(NSArray *args){
            return [NSNumber numberWithInt:[args[0] intValue] + [args[1] intValue]];
        },
        @"-": ^(NSArray *args){
            return [NSNumber numberWithInt:[args[0] intValue] - [args[1] intValue]];
        },
        @"*": ^(NSArray *args){
            return [NSNumber numberWithInt:[args[0] intValue] * [args[1] intValue]];
        },
        @"/": ^(NSArray *args){
            return [NSNumber numberWithInt:[args[0] intValue] / [args[1] intValue]];
        },
        };

    while (true) {
        char *rawline = _readline("user> ");
        if (!rawline) { break; }
        NSString *line = [NSString stringWithUTF8String:rawline];
        if ([line length] == 0) { continue; }
        @try {
            printf("%s\n", [[REP(line, repl_env) description] UTF8String]);
        } @catch(NSString *e) {
            printf("Error: %s\n", [e UTF8String]);
        }
    }

    [pool drain];

//    }
}
