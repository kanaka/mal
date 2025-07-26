// Some inspirations taken from https://git.sr.ht/~jummit/rekkyo

type memory = struct {
	envs: []*env,
	hashs: []hashmap,
	symbols: (void | hashmap),
	funcs: []function,
	lists: []list,
	vecs: []vector,
	strings: []string,
	atoms: []atom,
	intrinsics: []intrinsic,
};

type garbage_collector = struct {
	marked: memory,
	memory: memory,
};

let gc = garbage_collector {
	marked = memory {
		symbols = void,
		funcs = [],
		...
	},
	memory = memory {
		symbols = void,
		funcs = [],
		...
	},
};

fn reset_memory(memory: *memory) void = {

	memory.envs = memory.envs[..0];
	memory.hashs = memory.hashs[..0];
	memory.funcs = memory.funcs[..0];
	memory.lists = memory.lists[..0];
	memory.vecs = memory.vecs[..0];
	memory.strings = memory.strings[..0];
	memory.atoms = memory.atoms[..0];
	memory.intrinsics = memory.intrinsics[..0];

	match(memory.symbols){
	case let hm: hashmap =>
		hm.data = hm.data[..0];
	case void =>
		void;
	};
};

fn finish_memory(memory: memory) void = {

	free(memory.envs);
	free(memory.hashs);
	free(memory.funcs);
	free(memory.lists);
	free(memory.vecs);
	free(memory.strings);
	free(memory.atoms);
	free(memory.intrinsics);

	match(memory.symbols){
	case let hm: hashmap =>
		hm_free(hm);
	case void =>
		void;
	};
};

fn mark_hash(hm: hashmap) void = {

	append(gc.marked.hashs, hm)!;
	mark(hm.meta);

	for(let v .. hm.data){
		mark(v.key);
		mark(v.val);
	};
};

fn mark_env(envi: *env) void = {

	for(let e .. gc.marked.envs){
		if(e == envi) return void;
	};

	append(gc.marked.envs, envi)!;
	mark(envi.data);

	match(envi.outer){
	case null => void;
	case let e: *env =>
		mark_env(e);
	};
};

fn mark_col(col: []MalType) void = {
	for(let v .. col) {
		mark(v);
	};
};

fn mark (val: MalType) void = {

	match(gc.marked.symbols){
	case void =>
		gc.marked.symbols = hm_init(false);
	case => void;
	};

	match(val){
	case let v: vector =>
		for(let x .. gc.marked.vecs){
			if(x == v) return void;
		};
		append(gc.marked.vecs, v)!;
		mark_col(v.data);
		mark(v.meta);
	case let l: list =>
		for(let x .. gc.marked.lists){
			if(x == l) return void;
		};
		append(gc.marked.lists, l)!;
		mark_col(l.data);
		mark(l.meta);
	case  let f: function =>
		for(let x .. gc.marked.funcs){
			if(x == f) return void;
		};
		append(gc.marked.funcs, f)!;
		mark(f.meta);
		mark(f.body);
		mark_col(f.args);
		mark_env(f.envi);
	case let i: intrinsic =>
		for(let x .. gc.marked.intrinsics){
			if(x == i) return void;
		};
		append(gc.marked.intrinsics, i)!;
		mark(i.meta);
	case let m: macro =>
		let m = m:function;
		for(let x .. gc.marked.funcs){
			if(x == m) return void;
		};
		append(gc.marked.funcs, m)!;
		mark(m.meta);
		mark(m.body);
		mark_col(m.args);
		mark_env(m.envi);
	case let h: hashmap =>
		for(let x .. gc.marked.hashs){
			if(x == h) return void;
		};
		mark_hash(h);
	case let s: symbol =>
		match(hm_get(gc.marked.symbols: hashmap, s)){
		case undefined_key =>
			hm_add(gc.marked.symbols: hashmap, s, s);
		case =>  void;
		};
	case let s: string =>
		for(let x .. gc.marked.strings){
			if(x == s) return void;
		};
		append(gc.marked.strings, s)!;
		mark(s.meta);
	case let a: atom =>
		for(let x .. gc.marked.atoms){
			if(x == a) return void;
		};
		append(gc.marked.atoms, a)!;
		mark(*a);
	case => void;
	};
};

fn sweep() void ={

	const marked_symbols = match(gc.marked.symbols){
		case void =>
		     gc.marked.symbols = hm_init(false);
		yield gc.marked.symbols: hashmap;
	case let hm: hashmap =>
		yield hm;
	};

	const memory_symbols = match(gc.memory.symbols){
	case void =>
		gc.memory.symbols = hm_init(false);
		yield gc.memory.symbols: hashmap;
	case let hm: hashmap =>
		yield hm;
	};

	for (let i: size = 0; len(memory_symbols.data) > i; i += 1) {
		match(hm_get(marked_symbols, memory_symbols.data[i].key)){
		case undefined_key =>
			free(memory_symbols.data[i].key: symbol);
		case =>
			void;
		};
	};
	for :sweep (let i: size = 0; len(gc.memory.atoms) > i; i += 1) {
		for(let x .. gc.marked.atoms){
			if(x == gc.memory.atoms[i]) continue :sweep;
		};
		free(gc.memory.atoms[i]);
	};
	for :sweep (let i: size = 0; len(gc.memory.strings) > i; i += 1) {
		for(let x .. gc.marked.strings){
			if(x == gc.memory.strings[i]) continue :sweep;
		};
		free_string(gc.memory.strings[i]);
	};
	for :sweep (let i: size = 0; len(gc.memory.hashs) > i; i += 1) {
		for(let x .. gc.marked.hashs){
			if(x == gc.memory.hashs[i]) continue :sweep;
		};
		hm_free(gc.memory.hashs[i]);
	};
	for :sweep (let i: size = 0; len(gc.memory.envs) > i; i += 1) {
		for(let x .. gc.marked.envs){
			if(x == gc.memory.envs[i]) continue :sweep;
		};
		free(gc.memory.envs[i]); //.data is collected as a hashmap
	};
	for :sweep (let i: size = 0; len(gc.memory.vecs) > i; i += 1) {
		for(let x .. gc.marked.vecs){
			if(x == gc.memory.vecs[i]) continue :sweep;
		};
		free_vec(gc.memory.vecs[i]);
	};
	for :sweep (let i: size = 0; len(gc.memory.lists) > i; i += 1) {
		for(let x .. gc.marked.lists){
			if(x == gc.memory.lists[i]) continue :sweep;
		};
		free_list(gc.memory.lists[i]);
	};
	for :sweep (let i: size = 0; len(gc.memory.funcs) > i; i += 1) {
		for(let x .. gc.marked.funcs){
			if(x == gc.memory.funcs[i]) continue :sweep;
		};
		free_func(gc.memory.funcs[i]);
	};
	for :sweep (let i: size = 0; len(gc.memory.intrinsics) > i; i += 1) {
		for(let x .. gc.marked.intrinsics){
			if(x == gc.memory.intrinsics[i]) continue :sweep;
		};
		free(gc.memory.intrinsics[i]);
	};

	reset_memory(&gc.memory);

	gc = garbage_collector {
		marked = gc.memory,
		memory = gc.marked,
	};
};

// it doesn't make sense to call this with anything but the global repl_env,
// because as of this version there's no way to keep track of objects reachable
// through the ast of the current evaluation and it's possible continuations.

export fn run_gc(envi: *env) void = {

	mark_env(envi);
	sweep();
};
