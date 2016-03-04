#import <Foundation/Foundation.h>

#import "types.h"
#import "printer.h"
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

    @"list": ^(NSArray *args){
        return args;
    },
    @"list?": ^(NSArray *args){
        return wrap_tf(list_Q(args[0]));
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

    };
}

@end
