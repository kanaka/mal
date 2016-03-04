#import "types.h"

#import "malfunc.h"

@implementation MalFunc

@synthesize ast = _ast;
@synthesize env = _env;
@synthesize params = _params;

- (id)init:(NSArray *)ast env:(Env *)env params:(NSArray *)params {
    self = [super init];
    if (self) {
        _ast = ast;
        _env = env;
        _params = params;
    }
    return self;
}

- (id)apply:(NSArray *)args {
    return EVAL(_ast, [Env fromBindings:_env binds:_params exprs:args]);
}

@end
