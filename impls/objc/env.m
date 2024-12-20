#import <Foundation/Foundation.h>

#import "types.h"
//#import "env.h"

@implementation Env

@synthesize data = _data;
@synthesize outer = _outer;

- (id)initWithBindings:(Env *)outer binds:(NSArray *)binds exprs:(NSArray *)exprs {
    self = [super init];
    if (self) {
        _outer = outer;
        _data = [NSMutableDictionary dictionary];

        for (int i=0; i < [binds count]; i++) {
            if ([(NSString *)binds[i] isEqualTo:@"&"]) {
                if ([exprs count] > i) {
                    NSRange r = NSMakeRange(i, [exprs count] - i);
                    _data[binds[i+1]] = [exprs subarrayWithRange:r];
                } else {
                    _data[binds[i+1]] = @[];
                }
                break;
            } else {
                _data[binds[i]] = exprs[i];
            }
        }
    }
    return self;
}

- (id)initWithOuter:(Env *)outer {
    return [self initWithBindings:outer binds:@[] exprs:@[]];
}

- (id)init {
    return [self initWithBindings:nil binds:@[] exprs:@[]];
}

+ (id)fromOuter:(Env *)outer {
    return [[Env alloc] initWithOuter:outer];
}

+ (id)fromBindings:(Env *)outer binds:(NSArray *)binds exprs:(NSArray *)exprs {
    return [[Env alloc] initWithBindings:outer binds:binds exprs:exprs];
}

- (NSObject *) set:(MalSymbol *)key val:(NSObject *)val {
    _data[key] = val;
    return val;
}

- (NSObject *) get:(MalSymbol *)key {
    NSObject * value;
    Env * e = self;
    while (true) {
        value = e.data[key];
        if (value != nil) return value;
        e = e.outer;
        if (e == nil) return nil;
    }
}

@end
