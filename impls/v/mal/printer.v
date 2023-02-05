module mal

import maps
import regex

pub fn pr_str(ast Type, readable bool) string {
	return match ast {
		Int {
			ast.val.str()
		}
		Float {
			ast.val.str()
		}
		String {
			if readable {
				'"${escape(ast.val)}"'
			} else {
				ast.val
			}
		}
		Keyword {
			':${ast.kw}'
		}
		Nil {
			'nil'
		}
		True {
			'true'
		}
		False {
			'false'
		}
		Symbol {
			ast.sym
		}
		List {
			'(' + ast.list.map(pr_str(it, readable)).join(' ') + ')'
		}
		Vector {
			'[' + ast.vec.map(pr_str(it, readable)).join(' ') + ']'
		}
		Hashmap {
			'{' + maps.to_array(ast.hm, fn [readable] (k string, v Type) string {
				return '${k} ${pr_str(v, readable)}'
			}).join(' ') + '}'
		}
		Fn {
			'#<fn>'
		}
	}
}

fn escape(str string) string {
	return str
		.replace('\\', '\\\\')
		.replace('\n', '\\n')
		.replace('"', '\\"')
}

fn unescape(str string) string {
	mut re := regex.regex_opt('\\\\(.)') or { panic(err) }
	return re.replace_by_fn(str, fn (re regex.RE, str string, start int, end int) string {
		g := re.get_group_by_id(str, 0)
		return match g {
			'n' { '\n' }
			else { g }
		}
	})
}
