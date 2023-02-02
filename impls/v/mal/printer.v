module mal

import maps

pub fn pr_str(ast MalType, readable bool) string {
	return match ast {
		MalInt {
			ast.val.str()
		}
		MalFloat {
			ast.val.str()
		}
		MalString {
            if readable { '"${escape(ast.val)}"' } else { ast.val }
		}
		MalKeyword {
			':${ast.key}'
		}
		MalNil {
			'nil'
		}
		MalTrue {
			'true'
		}
		MalFalse {
			'false'
		}
		MalSymbol {
			ast.sym
		}
		MalList {
			'(' + ast.list.map(pr_str(it, readable)).join(' ') + ')'
		}
		MalVector {
			'[' + ast.vec.map(pr_str(it, readable)).join(' ') + ']'
		}
		MalHashmap {
			'{' + maps.to_array(ast.hm, fn [readable] (k string, v MalType) string {
				return '${k} ${pr_str(v, readable)}'
			}).join(' ') + '}'
		}
		MalFn {
			'#<fn>'
		}
	}
}

fn escape( str string ) string {
    return str
        .replace('\\', '\\\\')
        .replace('\n', '\\n')
        .replace('"', '\\"')
}

fn unescape( str string ) string {
    return str
        .replace('\\n', '\n')
        .replace('\\"', '"')
        .replace('\\\\', '\\')
}
