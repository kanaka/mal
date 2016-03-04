#import <Foundation/Foundation.h>

//
// Env definition
//

@class MalSymbol;

@interface Env : NSObject
 
@property (copy) NSMutableDictionary * data;
@property (copy) Env * outer;

- (id)initWithBindings:(Env *)outer binds:(NSArray *)binds exprs:(NSArray *)exprs;
- (id)initWithOuter:(Env *)outer;
- (id)init;

+ (id)fromOuter:(Env *)outer;
+ (id)fromBindings:(Env *)outer binds:(NSArray *)binds exprs:(NSArray *)exprs;

- (NSObject *) set:(MalSymbol *)key val:(NSObject *)val;
- (Env *) find:(MalSymbol *)key;
- (NSObject *) get:(MalSymbol *)key;

@end

//
// Mal Types
//


@interface MalTrue : NSObject
@end

@interface MalFalse : NSObject
@end

@interface MalSymbol: NSString
@end


// Lists

BOOL list_Q(id obj);


// Vectors

@interface MalVector : NSArray

@property (copy) NSArray * array;
@property(readonly) NSUInteger count;

- (id)initWithArray:(NSArray *)arr;
- (id)init;

+ (id)fromArray:(NSArray *)arr;

- (id)objectAtIndex:(NSUInteger)index;

@end


// Hash Maps

NSDictionary * hash_map(NSArray *kvs);


// Mal Functions

BOOL block_Q(id obj);



// General functions

BOOL equal_Q(NSObject * a, NSObject * b);
