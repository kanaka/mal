#import <Foundation/Foundation.h>

#import "mal_readline.h"
#import "types.h"
#import "reader.h"
#import "printer.h"
#import "env.h"
#import "malfunc.h"
#import "core.h"

// read
NSObject *READ(NSString *str) {
    return read_str(str);
}

// eval
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
  while (true) {
    //NSLog(@"EVAL: %@ (%@)", _pr_str(ast, true), env);
    if (!list_Q(ast)) {
        return eval_ast(ast, env);
    }

    // apply list
    if ([(NSArray *)ast count] == 0) {
        return ast;
    }
    NSArray * alst = (NSArray *)ast;
    id a0 = alst[0];
    NSString * a0sym = [a0 isKindOfClass:[MalSymbol class]] ? (NSString *)a0
                                                            : @"__<*fn*>__";

    if ([a0sym isEqualTo:@"def!"]) {
        return [env set:((MalSymbol *)alst[1]) val:EVAL(alst[2], env)];
    } else if ([(NSString *)a0 isEqualTo:@"let*"]) {
        Env *let_env = [Env fromOuter:env];
        NSArray * binds = (NSArray *)alst[1];
        for (int i=0; i < [binds count]; i+=2) {
            [let_env set:binds[i] val:EVAL(binds[i+1], let_env)];
        }
        env = let_env;
        ast = alst[2]; // TCO
    } else if ([a0sym isEqualTo:@"do"]) {
        NSRange r = NSMakeRange(1, [alst count] - 2);
        eval_ast([alst subarrayWithRange:r], env);
        ast = [alst lastObject]; // TCO
    } else if ([a0sym isEqualTo:@"if"]) {
        NSObject * cond = EVAL(alst[1], env);
        if ([cond isKindOfClass:[NSNull class]] ||
            [cond isKindOfClass:[MalFalse class]]) {
            if ([alst count] > 3) {
                ast = alst[3]; // TCO
            } else {
                return [NSNull alloc];
            }
        } else {
            ast = alst[2]; // TCO
        }
    } else if ([a0sym isEqualTo:@"fn*"]) {
        return [[MalFunc alloc] init:alst[2] env:env params:alst[1]];
    } else {
        NSArray * el = (NSArray *) eval_ast(ast, env);
        NSArray * args = @[];
        if ([el count] > 1) {
            args = _rest(el);
        }
        if ([el[0] isKindOfClass:[MalFunc class]]) {
            MalFunc * mf = el[0];
            env = [Env fromBindings:[mf env] binds:[mf params] exprs:args];
            ast = [mf ast]; // TCO
        } else {
            NSObject * (^ f)(NSArray *) = el[0];
            return f(args);
        }
    }
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
    // Outside of pool to prevent "Block_release called upon
    // a stack..." message on exit
    Env * repl_env = [[Env alloc] init];
    NSArray *args = [[NSProcessInfo processInfo] arguments];

    // Create an autorelease pool to manage the memory into the program
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // If using automatic reference counting (ARC), use @autoreleasepool instead:
//    @autoreleasepool {

    // core.m: defined using Objective-C
    NSDictionary * core_ns = [Core ns];
    for (NSString* key in core_ns) {
        [repl_env set:(MalSymbol *)key val:[core_ns objectForKey:key]];
    }
    [repl_env set:(MalSymbol *)@"eval" val:^(NSArray *args) {
        return EVAL(args[0], repl_env);
    }];
    NSArray *argv = @[];
    if ([args count] > 2) {
        argv = [args subarrayWithRange:NSMakeRange(2, [args count] - 2)];
    }
    [repl_env set:(MalSymbol *)@"*ARGV*" val:argv];

    // core.mal: defined using the language itself
    REP(@"(def! not (fn* (a) (if a false true)))", repl_env);
    REP(@"(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env);

    if ([args count] > 1) {
        @try {
            REP([NSString stringWithFormat:@"(load-file \"%@\")", args[1]], repl_env);
        } @catch(NSString *e) {
            printf("Error: %s\n", [e UTF8String]);
        }
        return 0;
    }

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
