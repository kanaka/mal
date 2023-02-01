module mal

import maps

pub fn pr_str(ast MalType) string {
	return match ast {
		MalInt {
			ast.val.str()
		}
		MalFloat {
			ast.val.str()
		}
		MalString {
			'"${ast.val}"'
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
			'(' + ast.list.map(pr_str).join(' ') + ')'
		}
		MalVector {
			'[' + ast.vec.map(pr_str).join(' ') + ']'
		}
		MalHashmap {
			'{' + maps.to_array(ast.hash, fn (k string, v MalType) string {
				return '${k} ${pr_str(v)}'
			}).join(' ') + '}'
		}
	}
}
