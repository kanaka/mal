(import [types :as pytypes])

(defclass MalException [Exception]
  (defn --init-- [self val] (setv self.val val)))

(defclass Atom []
  (defn --init-- [self val] (setv self.val val)))

(defn clone [obj]
  (if (= (type obj) pytypes.FunctionType)
    (pytypes.FunctionType obj.__code__ obj.__globals__
                          :name obj.__name__
                          :argdefs obj.__defaults__
                          :closure obj.__closure__)
    obj))
