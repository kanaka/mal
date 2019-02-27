// Implement our old naive O(n) map because Yeti's hash table (h_new()) cannot
// be used inside arrays and structs (we can't get a pointer to hash table).
// This prevents saving pointer to environment in MalFunction for example.

struct Hash {
  pointer keys
  pointer vals
}

func hash_new(void)
{
  return Hash(keys=&[], vals=&[])
}

func hash_get(h, key)
{
  for (i = 1; i <= numberof(*h.keys); ++i) {
    if ((*h.keys)(i) == key) return *((*h.vals)(i))
  }
  return nil
}

func hash_has_key(h, key)
{
  for (i = 1; i <= numberof(*h.keys); ++i) {
    if ((*h.keys)(i) == key) return 1
  }
  return 0
}

func hash_set(&h, key, val)
{
  if (is_void(*h.keys)) {
    h.keys = &[key]
    h.vals = &[&val]
    return
  }
  for (i = 1; i <= numberof(*h.keys); ++i) {
    if ((*h.keys)(i) == key) {
      (*h.vals)(i) = &val
      return
    }
  }
  tmp = *h.keys
  grow, tmp, [key]
  h.keys = &tmp
  tmp = *h.vals
  grow, tmp, [&val]
  h.vals = &tmp
}

func hash_delete(&h, key)
{
  if (is_void(*h.keys) || numberof(*h.keys) == 0) return
  k = *h.keys
  v = *h.vals
  if (numberof(k) == 1) {
    if (k(1) == key) {
      h.keys = &[]
      h.vals = &[]
      return
    }
  }
  for (i = 1; i <= numberof(k); ++i) {
    if (k(i) == key) {
      if (i == 1) {
        h.keys = &(k(i+1:))
        h.vals = &(v(i+1:))
      } else if (i == numberof(k)) {
        h.keys = &(k(1:i-1))
        h.vals = &(v(1:i-1))
      } else {
        h.keys = &grow(k(1:i-1), k(i+1:))
        h.vals = &grow(v(1:i-1), v(i+1:))
      }
      return
    }
  }
}
