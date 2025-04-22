// Some inspirations taken from https://git.sr.ht/~jummit/rekkyo

type memory = struct {
	envs: []*env,
	hashs: []hashmap,
	symbols: []symbol,
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
		funcs = [],
		...
	},
	memory = memory {
		funcs = [],
		...
	},
};

fn finish_memory(memory: memory) void = {
	free(memory.envs);
	free(memory.hashs);
	free(memory.symbols);
	free(memory.funcs);
	free(memory.lists);
	free(memory.vecs);
	free(memory.strings);
	free(memory.atoms);
	free(memory.intrinsics);
};

fn mark_hash(hm: hashmap) void = {

	append(gc.marked.hashs, hm);
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

	append(gc.marked.envs, envi);
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

	match(val){
	case let v: vector =>
		for(let x .. gc.marked.vecs){
			if(x == v) return void;
		};
		append(gc.marked.vecs, v);
		mark_col(v.data);
		mark(v.meta);
	case let l: list =>
		for(let x .. gc.marked.lists){
			if(x == l) return void;
		};
		append(gc.marked.lists, l);
		mark_col(l.data);
		mark(l.meta);
	case  let f: function =>
		for(let x .. gc.marked.funcs){
			if(x == f) return void;
		};
		append(gc.marked.funcs, f);
		mark(f.meta);
		mark(f.body);
		mark_col(f.args);
		mark_env(f.envi);
	case let i: intrinsic =>
		for(let x .. gc.marked.intrinsics){
			if(x == i) return void;
		};
		append(gc.marked.intrinsics, i);
		mark(i.meta);
	case let m: macro =>
		let m = m:function;
		for(let x .. gc.marked.funcs){
			if(x == m) return void;
		};
		append(gc.marked.funcs, m);
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
		for(let x .. gc.marked.symbols){
			if(x == s) return void;
		};
		append(gc.marked.symbols, s);
	case let s: string =>
		for(let x .. gc.marked.strings){
			if(x == s) return void;
		};
		append(gc.marked.strings, s);
		mark(s.meta);
	case let a: atom =>
		for(let x .. gc.marked.atoms){
			if(x == a) return void;
		};
		append(gc.marked.atoms, a);
		mark(*a);
	case => void;
	};
};

fn sweep() void ={

	// since we're only ever comparing pointers and never dereferencing here
	// it should be ok to not call delete(gc.memory.<t>[i-1]) on the after
	// freeing the value.
	//
	// This means doing anything with gc.memory after calling this function
	// is extremely unsafe.

	for :sweep (let i: size = len(gc.memory.symbols); i > 0; i -= 1) {
		for(let x .. gc.marked.symbols){
			if(x == gc.memory.symbols[i-1]) continue :sweep;
		};
		free(gc.memory.symbols[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.atoms); i > 0; i -= 1) {
		for(let x .. gc.marked.atoms){
			if(x == gc.memory.atoms[i-1]) continue :sweep;
		};
		free(gc.memory.atoms[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.strings); i > 0; i -= 1) {
		for(let x .. gc.marked.strings){
			if(x == gc.memory.strings[i-1]) continue :sweep;
		};
		free_string(gc.memory.strings[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.hashs); i > 0; i -= 1) {
		for(let x .. gc.marked.hashs){
			if(x == gc.memory.hashs[i-1]) continue :sweep;
		};
		hm_free(gc.memory.hashs[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.envs); i > 0; i -= 1) {
		for(let x .. gc.marked.envs){
			if(x == gc.memory.envs[i-1]) continue :sweep;
		};
		free(gc.memory.envs[i-1]); //.data is collected as a hashmap
	};
	for :sweep (let i: size = len(gc.memory.vecs); i > 0; i -= 1) {
		for(let x .. gc.marked.vecs){
			if(x == gc.memory.vecs[i-1]) continue :sweep;
		};
		free_vec(gc.memory.vecs[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.lists); i > 0; i -= 1) {
		for(let x .. gc.marked.lists){
			if(x == gc.memory.lists[i-1]) continue :sweep;
		};
		free_list(gc.memory.lists[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.funcs); i > 0; i -= 1) {
		for(let x .. gc.marked.funcs){
			if(x == gc.memory.funcs[i-1]) continue :sweep;
		};
		free_func(gc.memory.funcs[i-1]);
	};
	for :sweep (let i: size = len(gc.memory.intrinsics); i > 0; i -= 1) {
		for(let x .. gc.marked.intrinsics){
			if(x == gc.memory.intrinsics[i-1]) continue :sweep;
		};
		free(gc.memory.intrinsics[i-1]);
	};
};

// it doesn't make sense to call this with anything but the global repl_env.

export fn run_gc(envi: *env) void = {

	mark_env(envi);
	sweep();
	finish_memory(gc.memory);

	gc = garbage_collector {
		marked = memory {
			funcs = [],
			...
		},
		memory = gc.marked,
	};
};

