#import <Foundation/Foundation.h>

#import "mal_readline.h"
#import "types.h"
#import "reader.h"
#import "printer.h"
#import "env.h"

// read
NSObject *READ(NSString *str) {
    return read_str(str);
}

// eval

// forward declaration
NSObject *EVAL(NSObject *ast, Env *env);

NSObject *eval_ast(NSObject *ast, Env *env) {
    if ([ast isMemberOfClass:[MalSymbol class]]) {
        return [env get:(MalSymbol *)ast];
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

NSObject *EVAL(NSObject *ast, Env *env) {
    //NSLog(@"EVAL: %@", ast);
    if (!list_Q(ast)) {
        return eval_ast(ast, env);
    }

    // apply list
    if ([(NSArray *)ast count] == 0) {
        return ast;
    }
    NSArray * alst = (NSArray *)ast;
    id a0 = alst[0];
    if (![a0 isKindOfClass:[MalSymbol class]]) {
        @throw @"attempt to apply on non-symbol";
    }
    if ([(NSString *)a0 isEqualTo:@"def!"]) {
        return [env set:((MalSymbol *)alst[1]) val:EVAL(alst[2], env)];
    } else if ([(NSString *)a0 isEqualTo:@"let*"]) {
        Env *let_env = [Env fromOuter:env];
        NSArray * binds = (NSArray *)alst[1];
        for (int i=0; i < [binds count]; i+=2) {
            [let_env set:binds[i] val:EVAL(binds[i+1], let_env)];
        }
        return EVAL(alst[2], let_env);
    } else {
        NSArray * el = (NSArray *) eval_ast(ast, env);
        NSObject * (^ f)(NSArray *) = el[0];
        NSArray * args = _rest(el);
        return f(args);
    }
}

// print
NSString *PRINT(NSObject *exp) {
    return _pr_str(exp, true);
}

// REPL
NSString *REP(NSString *line, Env *env) {
    return PRINT(EVAL(READ(line), env));
}

int main () {
    Env * repl_env = [[Env alloc] init];

    // Create an autorelease pool to manage the memory into the program
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // If using automatic reference counting (ARC), use @autoreleasepool instead:
//    @autoreleasepool {

    [repl_env set:(MalSymbol *)@"+" val:^(NSArray *args){
        return [NSNumber numberWithInt:[args[0] intValue] + [args[1] intValue]];
    }];
    [repl_env set:(MalSymbol *)@"-" val:^(NSArray *args){
        return [NSNumber numberWithInt:[args[0] intValue] - [args[1] intValue]];
    }];
    [repl_env set:(MalSymbol *)@"*" val:^(NSArray *args){
        return [NSNumber numberWithInt:[args[0] intValue] * [args[1] intValue]];
    }];
    [repl_env set:(MalSymbol *)@"/" val:^(NSArray *args){
        return [NSNumber numberWithInt:[args[0] intValue] / [args[1] intValue]];
    }];

    while (true) {
        char *rawline = _readline("user> ");
        if (!rawline) { break; }
        NSString *line = [NSString stringWithUTF8String:rawline];
        if ([line length] == 0) { continue; }
        @try {
            printf("%s\n", [[REP(line, repl_env) description] UTF8String]);
        } @catch(NSString *e) {
            printf("Error: %s\n", [e UTF8String]);
        } @catch(NSException *e) {
            if ([[e name] isEqualTo:@"ReaderContinue"]) { continue; }
            printf("Exception: %s\n", [[e reason] UTF8String]);
        }
    }

    [pool drain];

//    }
}
