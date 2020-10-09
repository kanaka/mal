/ Similar to envs, atoms are also mutable globals
/ this is again, very ugly, but much better than
/ wiring atoms around.
global_atom_storage: ();

allocate_atom: {`global_atom_storage set global_atom_storage, enlist x; count global_atom_storage};
get_atom: {global_atom_storage (x - 1)};
set_atom: {global_atom_storage[x - 1]:y};
make_atom: allocate_atom;
