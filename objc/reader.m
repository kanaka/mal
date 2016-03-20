#import <Foundation/Foundation.h>

#import "types.h"

// Only used here, so define interface locally
@interface Reader : NSObject

- (id)initWithTokens:(NSArray *)toks;
- (id)init;

- (NSString *) next;
- (NSString *) peek;

@end


@implementation Reader

NSArray  *_tokens;
int _position;

- (id)initWithTokens:(NSArray *)toks {
    self = [super init];
    if (self) {
        _tokens = toks;
        _position = 0;
    }
    return self;
}

- (id)init {
    return [self initWithTokens:@[]];
}

- (NSString *)next {
    _position++;
    return _tokens[_position-1];
}

- (NSString *)peek {
    if ([_tokens count] > _position) {
        return _tokens[_position];
    } else {
        return nil;
    }
}

@end


NSArray * tokenize(NSString *str) {
    NSRegularExpression *regex = [NSRegularExpression
        regularExpressionWithPattern:@"[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\\\].|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)"
        options:0
        error:NULL];

    NSArray *matches = [regex
        matchesInString:str
        options:0
        range:NSMakeRange(0, [str length])];

    NSMutableArray * tokens = [NSMutableArray array];
    for (NSTextCheckingResult *match in matches) {
        NSString * mstr = [str substringWithRange:[match rangeAtIndex:1]];
        if ([mstr characterAtIndex:0] == ';') { continue; }
        [tokens addObject:mstr];
    }
    return tokens;
}

NSObject * read_atom(Reader * rdr) {
    NSRegularExpression *regex = [NSRegularExpression
        regularExpressionWithPattern:@"(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^\"(.*)\"$|:(.*)|(^[^\"]*$)"
        options:0
        error:NULL];
    NSNumberFormatter *numf = [[NSNumberFormatter alloc] init];
    numf.numberStyle = NSNumberFormatterDecimalStyle;

    NSString *token = [rdr next];

    NSArray *matches = [regex
        matchesInString:token
        options:0
        range:NSMakeRange(0, [token length])];

    if ([matches count] > 0) {

        NSTextCheckingResult *match = matches[0];
        if ([match rangeAtIndex:1].location < -1ULL/2) { // integer
            return [numf numberFromString:token];
        } else if ([match rangeAtIndex:2].location < -1ULL/2) { // float
            return [numf numberFromString:token];
        } else if ([match rangeAtIndex:3].location < -1ULL/2) { // nil
            return [NSNull alloc];
        } else if ([match rangeAtIndex:4].location < -1ULL/2) { // true
            return [MalTrue alloc]; // TODO: intern
        } else if ([match rangeAtIndex:5].location < -1ULL/2) { // false
            return [MalFalse alloc]; // TODO: intern
        } else if ([match rangeAtIndex:6].location < -1ULL/2) { // string
            NSString * str = [token substringWithRange:[match rangeAtIndex:6]];
            return [[[str
                      stringByReplacingOccurrencesOfString:@"\\\"" withString:@"\""]
                     stringByReplacingOccurrencesOfString:@"\\n" withString:@"\n"]
                    stringByReplacingOccurrencesOfString:@"\\\\" withString:@"\\"];
        } else if ([match rangeAtIndex:7].location < -1ULL/2) { // keyword
            return [NSString stringWithFormat:@"\u029e%@",
                    [token substringWithRange:[match rangeAtIndex:7]]];
        } else if ([match rangeAtIndex:8].location < -1ULL/2) { // symbol
            return [MalSymbol stringWithString:token];
        }
    }

    @throw @"read_atom: invalid token";
}

// Only used locally, so declare here
NSObject * read_form(Reader * rdr);

NSArray * read_list(Reader * rdr, char start, char end) {
    NSString * token = [rdr next];
    NSMutableArray * ast = [NSMutableArray array];

    if ([token characterAtIndex:0] != start) {
        @throw [NSString stringWithFormat:@"expected '%c'", start];
    }
    while ((token = [rdr peek]) && ([token characterAtIndex:0] != end)) {
        [ast addObject:read_form(rdr)];
    }
    if (!token) {
        @throw [NSString stringWithFormat:@"expected '%c', got EOF", end];
    }
    [rdr next];
    return ast;
}

NSObject * read_form(Reader * rdr) {
    NSString *token = [rdr peek];
    switch ([token characterAtIndex:0]) {
    case '\'': [rdr next];
              return @[[MalSymbol stringWithString:@"quote"],
                       read_form(rdr)];
    case '`': [rdr next];
              return @[[MalSymbol stringWithString:@"quasiquote"],
                       read_form(rdr)];
    case '~': [rdr next];
              if ([token isEqualToString:@"~@"]) {
                  return @[[MalSymbol stringWithString:@"splice-unquote"],
                           read_form(rdr)];
              } else {
                  return @[[MalSymbol stringWithString:@"unquote"],
                           read_form(rdr)];
              }
    case '^': [rdr next];
              NSObject * meta = read_form(rdr);
              return @[[MalSymbol stringWithString:@"with-meta"],
                       read_form(rdr),
                       meta];
    case '@': [rdr next];
              return @[[MalSymbol stringWithString:@"deref"],
                       read_form(rdr)];

    // lists
    case ')':
        @throw @"unexpected ')'";
    case '(':
        return read_list(rdr, '(', ')');

    // vectors
    case ']':
        @throw @"unexpected ']'";
    case '[':
        return [MalVector fromArray:read_list(rdr, '[', ']')];

    // hash maps
    case '}':
        @throw @"unexpected '}'";
    case '{':
        return hash_map(read_list(rdr, '{', '}'));
    default:
        return read_atom(rdr);
    }
}

NSObject * read_str(NSString *str) {
    NSArray * tokens = tokenize(str);
    if ([tokens count] == 0) { @throw [NSException exceptionWithName:@"ReaderContinue"
                                       reason:@"empty token"
                                       userInfo:nil]; }
    //if ([tokens count] == 0) { @throw [[MalContinue alloc] init]; }
    return read_form([[Reader alloc] initWithTokens:tokens]);
}
