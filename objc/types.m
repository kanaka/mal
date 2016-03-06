#import "types.h"

@implementation MalTrue
@end

@implementation MalFalse
@end


// NSString subclassing based on:
// http://stackoverflow.com/a/21331422/471795

// Symbols

@interface MalSymbol ()
@property (nonatomic, strong) NSString *stringHolder;
@end

@implementation MalSymbol

- (instancetype)initWithCharactersNoCopy:(unichar *)characters length:(NSUInteger)length freeWhenDone:(BOOL)freeBuffer {
    self = [super init];
    if (self) {
        self.stringHolder = [[NSString alloc] initWithCharactersNoCopy:characters length:length freeWhenDone:freeBuffer];
    }
    return self;
}

- (NSUInteger)length {
    return self.stringHolder.length;
}

- (unichar)characterAtIndex:(NSUInteger)index {
    return [self.stringHolder characterAtIndex:index];
}

@end


BOOL string_Q(id obj) {
    if ([obj isKindOfClass:[NSString class]]) {
        NSString * s = obj;
        if (![s isKindOfClass:[MalSymbol class]]) {
            return ![s hasPrefix:@"\u029e"];
        }
    }
    return false;
}

// Lists

BOOL list_Q(id obj) {
    return ([obj isKindOfClass:[NSArray class]] &&
            ![obj isKindOfClass:[MalVector class]]);
}

NSArray * _rest(NSArray * obj) {
    return [obj subarrayWithRange:NSMakeRange(1, [obj count]-1)];
}

// Vectors 

@implementation MalVector

@synthesize array = _array;
@synthesize count = _count;

- (id)initWithArray:(NSArray *)arr {
    self = [self init];
    if (self) {
        _array = arr;
        _count = [arr count];
    }
    return self;
}
        
- (id)init {
    self = [super init];
    if (self) {
        _array = @[];
        _count = 0;
    }
    return self;
}

+ (id)fromArray:(NSArray *)arr {
    return [[MalVector alloc] initWithArray:arr];
}    

- (id)objectAtIndex:(NSUInteger)index {
    return _array[index];
}

@end


// Hash Maps

NSDictionary * assoc_BANG(NSMutableDictionary * d, NSArray * kvs) {
    for (int i=0; i < [kvs count]; i+=2) {
        d[kvs[i]] = kvs[i+1];
    }
    return d;
}

NSDictionary * hash_map(NSArray *kvs) {
    return assoc_BANG([NSMutableDictionary dictionary], kvs);
}


// Mal Functions

BOOL block_Q(id obj) {
    id block = ^{};
    Class blockClass = [block class];
    while ([blockClass superclass] != [NSObject class]) {
        blockClass = [blockClass superclass];
    }
    return [obj isKindOfClass:blockClass];
}



@implementation MalAtom

@synthesize val = _val;

- (id)init:(NSObject *)val {
    self = [super init];
    if (self) {
        _val = val;
    }
    return self;
}

+ (id)fromObject:(NSObject *)val {
    return [[MalAtom alloc] init:val];
}

@end

BOOL atom_Q(id obj) {
    return [obj isKindOfClass:[MalAtom class]];
}

// General functions

BOOL sequential_Q(NSObject * obj) {
    return [obj isKindOfClass:[NSArray class]];
}

BOOL equal_Q(NSObject * a, NSObject * b) {
    //NSLog(@"= %@ (%@), %@ (%@)", a, [a class], b, [b class]);
    if (!(([a class] == [b class]) ||
          ([a isKindOfClass:[NSArray class]] &&
           [b isKindOfClass:[NSArray class]]) ||
          ([a isKindOfClass:[NSNumber class]] &&
           [b isKindOfClass:[NSNumber class]]) ||
          (string_Q(a) && string_Q(b)))) {
        return false;
    }
    if ([a isKindOfClass:[MalTrue class]]) {
        return true;
    } else if ([a isKindOfClass:[MalFalse class]]) {
        return true;
    } else if ([a isKindOfClass:[NSNumber class]]) {
        return [(NSNumber *)a intValue] == [(NSNumber *)b intValue];
    } else {
        return [a isEqual:b];
    }
}
