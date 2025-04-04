export type ns_entry = (str, (intrinsic | function));
export type namespace = []ns_entry;

export fn load_namespace(ns: namespace, env: *env) void = {
	for(let e.. ns){
		env_set(env, e.0, e.1);
	};
};

export let core: namespace = [
	("pr", &prn),
	("list", &mallist),
	("count", &count),
	("list?", &listp),
	("empty?", &emptyp),
	("not", &not),
	("+", &plus),
	("-", &minus),
	("*", &mult),
	("/", &div),
	(">", &greater_than),
	("<", &smaller_than),
	(">=", &greq_than),
	("<=", &seq_than),
	("=", &mal_eq),
	("prn", &prn),
	("println", &prn_line),
	("pr-str", &pr_str),
	("str", &pr_str_ugly),
];

