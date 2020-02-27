#import <Foundation/Foundation.h>

#import "types.h"

NSString * _pr_str(NSObject * obj, BOOL print_readably) {
    //NSLog(@"class: %@", [obj class]);
    if ([obj isMemberOfClass:[NSNull class]]) {
        return @"nil";
    } else if ([obj isMemberOfClass:[MalTrue class]]) {
        return @"true";
    } else if ([obj isMemberOfClass:[MalFalse class]]) {
        return @"false";
    } else if ([obj isKindOfClass:[MalSymbol class]]) {
        return (NSString *) obj;
    } else if ([obj isKindOfClass:[NSString class]]) {
        NSString * str = (NSString *)obj;
        if ([str length] > 0 && ([str hasPrefix:@"\u029e"])) {
            return [NSString stringWithFormat:@":%@",
                      [str substringWithRange:NSMakeRange(1, [str length]-1)]];
        } else if (print_readably) {
            str = [[[(NSString *)obj
                     stringByReplacingOccurrencesOfString:@"\\" withString:@"\\\\"]
                    stringByReplacingOccurrencesOfString:@"\"" withString:@"\\\""]
                   stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
            return [NSString stringWithFormat:@"\"%@\"", str];
        } else {
            return [NSString stringWithString:str];
        }
    } else if ([obj isKindOfClass:[NSArray class]]) {
        NSMutableArray * elems = [NSMutableArray array];
        for (NSObject * elem in (NSArray *)obj) {
            [elems addObject:_pr_str(elem, print_readably)];
        }
        if ([obj isKindOfClass:[MalVector class]]) {
            return [NSString stringWithFormat:@"[%@]",
                    [elems componentsJoinedByString:@" "]];
        } else {
            return [NSString stringWithFormat:@"(%@)",
                    [elems componentsJoinedByString:@" "]];
        }
    } else if ([obj isKindOfClass:[NSDictionary class]]) {
        NSDictionary * dict = (NSDictionary *)obj;
        NSMutableArray * elems = [NSMutableArray array];
        for (NSString * key in dict) {
            [elems addObject:_pr_str(key, print_readably)];
            [elems addObject:_pr_str(dict[key], print_readably)];
        }
        return [NSString stringWithFormat:@"{%@}",
                [elems componentsJoinedByString:@" "]];
    } else if (block_Q(obj)) {
        return @"#<native function>";
    } else if (atom_Q(obj)) {
        return [NSString stringWithFormat:@"(atom %@)",
                _pr_str([(MalAtom *)obj val], print_readably)];
    } else {
        return [obj description];
    }
}
