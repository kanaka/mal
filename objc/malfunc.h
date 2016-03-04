#import <Foundation/Foundation.h>

/*
// Forward declaration of Env (see env.h for full interface)
@class Env;
*/
// Forward declaration of EVAL function
NSObject *EVAL(id ast, id env);
 
@interface MalFunc : NSObject <NSCopying>

@property (copy) NSArray * ast;
@property (copy) Env * env;
@property (copy) NSArray * params;
@property BOOL isMacro;
@property (copy) NSObject * meta;

- (id)init:(NSArray *)ast env:(Env *)env params:(NSArray *)params;

- (id)apply:(NSArray *)args;

@end

NSObject * apply(id f, NSArray *args);
