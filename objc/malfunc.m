#import "types.h"

#import "malfunc.h"

@implementation MalFunc

@synthesize ast = _ast;
@synthesize env = _env;
@synthesize params = _params;
@synthesize isMacro = _isMacro;
@synthesize meta = _meta;

- (id)init:(NSArray *)ast env:(Env *)env params:(NSArray *)params {
    self = [super init];
    if (self) {
        _ast = ast;
        _env = env;
        _params = params;
        _isMacro = false;
        _meta = [NSNull alloc];
    }
    return self;
}

- (id)apply:(NSArray *)args {
    return EVAL(_ast, [Env fromBindings:_env binds:_params exprs:args]);
}

- (id)copyWithZone:(NSZone *)zone
{
    MalFunc * copy = [[[self class] alloc] init:_ast env:_env params:_params];
    if (copy) {
        copy.isMacro = _isMacro;
        copy.meta = _meta;
    }
    return copy;
}

@end


NSObject * apply(id f, NSArray *args) {
    if ([f isKindOfClass:[MalFunc class]]) {
        return [f apply:args];
    } else {
        NSObject * (^ fn)(NSArray *) = f;
        return fn(args);
    }
}
