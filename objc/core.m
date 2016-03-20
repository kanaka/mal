#import <Foundation/Foundation.h>

#import "mal_readline.h"
#import "types.h"
#import "reader.h"
#import "printer.h"
#import "malfunc.h"
#import "core.h"

NSObject * wrap_tf(BOOL val) {
    return val ? [MalTrue alloc] : [MalFalse alloc];
}

@implementation Core

+ (NSDictionary *)ns {
    return @{
    @"=": ^(NSArray *args){
        return wrap_tf(equal_Q(args[0], args[1]));
    },
    @"throw": ^(NSArray *args){
        @throw args[0];
    },

    @"nil?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[NSNull class]]);
    },
    @"true?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[MalTrue class]]);
    },
    @"false?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[MalFalse class]]);
    },
    @"string?": ^(NSArray *args){
        return wrap_tf(string_Q(args[0]));
    },
    @"symbol": ^(NSArray *args){
        return [MalSymbol stringWithString:args[0]];
    },
    @"symbol?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[MalSymbol class]]);
    },
    @"keyword": ^(NSArray *args){
        return [NSString stringWithFormat:@"\u029e%@", args[0]];
    },
    @"keyword?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[NSString class]] &&
                       ![args[0] isKindOfClass:[MalSymbol class]] &&
                       !string_Q(args[0]));
    },

    @"pr-str": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        for (id e in args) { [res addObject:_pr_str(e,true)]; }
        return [res componentsJoinedByString:@" "];
    },
    @"str": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        for (id e in args) { [res addObject:_pr_str(e,false)]; }
        return [res componentsJoinedByString:@""];
    },
    @"prn": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        for (id e in args) { [res addObject:_pr_str(e,true)]; }
        printf("%s\n", [[res componentsJoinedByString:@" "] UTF8String]);
        fflush(stdout);
        return [NSNull alloc];
    },
    @"println": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        for (id e in args) { [res addObject:_pr_str(e,false)]; }
        printf("%s\n", [[res componentsJoinedByString:@" "] UTF8String]);
        fflush(stdout);
        return [NSNull alloc];
    },
    @"read-string": ^(NSArray *args){
        return read_str(args[0]);
    },
    @"readline": ^(NSArray *args){
        char * rawline = _readline((char *)[(NSString *)args[0] UTF8String]);
        if (rawline) {
            return (NSObject *)[NSString stringWithUTF8String:rawline];
        } else {
            return (NSObject *)[NSNull alloc];
        }
    },
    @"slurp": ^(NSArray *args){
        return [NSString stringWithContentsOfFile:args[0]
                         encoding: NSUTF8StringEncoding
                         error: NULL];
    },

    @"<": ^(NSArray *args){
        return wrap_tf([args[0] intValue] < [args[1] intValue]);
    },
    @"<=": ^(NSArray *args){
        return wrap_tf([args[0] intValue] <= [args[1] intValue]);
    },
    @">": ^(NSArray *args){
        return wrap_tf([args[0] intValue] > [args[1] intValue]);
    },
    @">=": ^(NSArray *args){
        return wrap_tf([args[0] intValue] >= [args[1] intValue]);
    },
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
    @"time-ms": ^(NSArray *args){
        long long ms = [[NSDate date] timeIntervalSince1970] * 1000;
        return [NSNumber numberWithUnsignedInteger:ms];
    },

    @"list": ^(NSArray *args){
        return args;
    },
    @"list?": ^(NSArray *args){
        return wrap_tf(list_Q(args[0]));
    },
    @"vector": ^(NSArray *args){
        return [MalVector fromArray:args];
    },
    @"vector?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[MalVector class]]);
    },
    @"hash-map": ^(NSArray *args){
        return hash_map(args);
    },
    @"map?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[NSDictionary class]]);
    },
    @"assoc": ^(NSArray *args){
        NSDictionary * dict = args[0];
        NSMutableDictionary * new_dict = [[NSMutableDictionary alloc]
                                          initWithDictionary:dict
                                          copyItems:NO];
        return assoc_BANG(new_dict, _rest(args));
    },
    @"dissoc": ^(NSArray *args){
        NSDictionary * dict = args[0];
        NSMutableDictionary * new_dict = [[NSMutableDictionary alloc]
                                          initWithDictionary:dict
                                          copyItems:NO];
        for (NSString * key in _rest(args)) {
            [new_dict removeObjectForKey:key];
        }
        return new_dict;
    },
    @"get": ^(NSArray *args){
        if ([args[0] isKindOfClass:[NSNull class]]) {
            return (NSObject *)[NSNull alloc];
        }
        NSObject * res = ((NSDictionary *)args[0])[args[1]];
        return res ? res : [NSNull alloc];
    },
    @"contains?": ^(NSArray *args){
        if ([args[0] isKindOfClass:[NSNull class]]) {
            return wrap_tf(false);
        }
        return wrap_tf(((NSDictionary *)args[0])[args[1]] != nil);
    },
    @"keys": ^(NSArray *args){
        return [(NSDictionary *)args[0] allKeys];
    },
    @"vals": ^(NSArray *args){
        return [(NSDictionary *)args[0] allValues];
    },
    
    @"sequential?": ^(NSArray *args){
        return wrap_tf([args[0] isKindOfClass:[NSArray class]]);
    },
    @"cons": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        [res addObject:args[0]];
        [res addObjectsFromArray:args[1]];
        return res;
    },
    @"concat": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        for (NSArray * arr in args) {
            [res addObjectsFromArray:arr];
        }
        return res;
    },
    @"nth": ^(NSArray *args){
        NSArray * lst = (NSArray *)args[0];
        int idx = [(NSNumber *)args[1] intValue];
        if (idx < [lst count]) {
            return lst[idx];
        } else {
            @throw @"nth: index out of range";
        }
    },
    @"first": ^(NSArray *args){
        if ([args[0] isKindOfClass:[NSNull class]]) {
            return (NSObject *)[NSNull alloc];
        }
        NSArray * lst = (NSArray *)args[0];
        if ([lst count] > 0) {
            return (NSObject *)lst[0];
        } else {
            return (NSObject *)[NSNull alloc];
        }
    },
    @"rest": ^(NSArray *args){
        if ([args[0] isKindOfClass:[NSNull class]]) {
            return @[];
        }
        NSArray * lst = (NSArray *)args[0];
        if ([lst count] > 1) {
            return _rest(lst);
        } else {
            return @[];
        }
    },
    @"empty?": ^(NSArray *args){
        if ([args[0] isKindOfClass:[NSNull class]]) {
            return wrap_tf(true);
        } else {
            return wrap_tf([args[0] count] == 0);
        }
    },
    @"count": ^(NSArray *args){
        if ([args[0] isKindOfClass:[NSNull class]]) {
            return @0;
        } else {
            return [NSNumber numberWithInt:[args[0] count]];
        }
    },
    @"apply": ^(NSArray *args){
        NSObject * (^ f)(NSArray *) = args[0];
        NSMutableArray * fargs = [NSMutableArray array];
        if ([args count] > 1) {
            NSRange r = NSMakeRange(1, [args count]-2);
            [fargs addObjectsFromArray:[args subarrayWithRange:r]];
        }
        [fargs addObjectsFromArray:(NSArray *)[args lastObject]];
        return apply(f, fargs);
    },
    @"map": ^(NSArray *args){
        NSObject * (^ f)(NSArray *) = args[0];
        NSMutableArray * res = [NSMutableArray array];
        for (NSObject * x in (NSArray *)args[1]) {
            [res addObject:apply(f, @[x])];
        }
        return res;
    },
    @"conj": ^(NSArray *args){
        NSMutableArray * res = [NSMutableArray array];
        if ([args[0] isKindOfClass:[MalVector class]]) {
            [res addObjectsFromArray:args[0]];
            [res addObjectsFromArray:_rest(args)];
            return (NSObject *)[MalVector arrayWithArray:res];
        } else {
            [res addObjectsFromArray:[[_rest(args) reverseObjectEnumerator]
                                      allObjects]];
            [res addObjectsFromArray:args[0]];
            return (NSObject *)res;
        }
    },
    @"seq": ^(NSArray *args){
        if (list_Q(args[0])) {
            if ([args[0] count] == 0) { return (NSObject *)[NSNull alloc]; }
            return (NSObject *)args[0];
        } else if ([args[0] isKindOfClass:[MalVector class]]) {
            if ([args[0] count] == 0) { return (NSObject *)[NSNull alloc]; }
            return (NSObject *)[NSArray arrayWithArray:args[0]];
        } else if (string_Q(args[0])) {
            NSString * str = args[0];
            if ([str length] == 0) { return (NSObject *)[NSNull alloc]; }
            NSMutableArray * res = [NSMutableArray array];
            for (int i=0; i < [str length]; i++) {
                char c = [str characterAtIndex:i];
                [res addObject:[NSString stringWithFormat:@"%c", c]];
            }
            return (NSObject *)res;
        } else if ([args[0] isKindOfClass:[NSNull class]]) {
            return (NSObject *)args[0];
        } else {
            @throw @"seq: called on non-sequence";
        }
    },

    @"meta": ^(NSArray *args){
        if ([args[0] isKindOfClass:[MalFunc class]]) {
            return [(MalFunc *)args[0] meta];
        } else {
            return (NSObject *)[NSNull alloc];
        }
    },
    @"with-meta": ^(NSArray *args){
        if ([args[0] isKindOfClass:[MalFunc class]]) {
            MalFunc * cmf = [(MalFunc *)args[0] copy];
            cmf.meta = args[1];
            return cmf;
        } else {
            @throw @"with-meta: object type not supported";
        }
    },
    @"atom": ^(NSArray *args){
        return [MalAtom fromObject:args[0]];
    },
    @"atom?": ^(NSArray *args){
        return wrap_tf(atom_Q(args[0]));
    },
    @"deref": ^(NSArray *args){
        return [(MalAtom *)args[0] val];
    },
    @"reset!": ^(NSArray *args){
        MalAtom * atm = (MalAtom *)args[0];
        return atm.val = args[1];
    },
    @"swap!": ^(NSArray *args){
        MalAtom * atm = (MalAtom *)args[0];
        NSObject * (^ f)(NSArray *) = args[1];
        NSMutableArray * fargs = [NSMutableArray array];
        [fargs addObject:atm.val];
        if ([args count] > 2) {
            NSRange r = NSMakeRange(2, [args count]-2);
            [fargs addObjectsFromArray:[args subarrayWithRange:r]];
        }
        return atm.val = apply(f, fargs);
    },
    };
}

@end
