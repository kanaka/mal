(defclass MalException [Exception]
  (defn --init-- [self val] (setv self.val val)))

(defclass Atom []
  (defn --init-- [self val] (setv self.val val)))

