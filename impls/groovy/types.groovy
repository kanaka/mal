import groovy.transform.InheritConstructors
import groovy.transform.AutoClone

class types {
    def static copy(obj) {
        def new_obj = obj
        if (obj instanceof Collection || obj instanceof Map) {
            new_obj = obj.clone()
            if (obj.hasProperty("meta")) {
                new_obj.getMetaClass().meta = obj.getProperties().meta
            }
            if (obj.hasProperty("isvector")) {
                new_obj.getMetaClass().isvector = obj.getProperties().isvector
            }
        } else if (obj instanceof Object) {
            new_obj = obj.clone()
        }
        return new_obj
    }

    @InheritConstructors
    static class MalException extends Exception {
        def obj
        MalException(String message) {
            super(message)
            obj = message
        }
        MalException(_obj) {
            super("mal exception containing object")
            obj = _obj
        }
    }

    def static string_Q(o) {
        return o instanceof String && (o.size() == 0 || o[0] != "\u029e")
    }

    @AutoClone
    static class MalSymbol implements Comparable {
        String value
        MalSymbol(String name) {
            value = name
        }
        int compareTo(o) { value <=> o.value }
    }

    def static keyword(o) {
        types.&keyword_Q(o) ? o : ("\u029e" + o)
    }
    def static keyword_Q(o) {
        return o instanceof String && o.size() > 0 && o[0] == "\u029e"
    }

    def static list_Q(o) {
        //return (o instanceof List || o instanceof Object[]) &&
        return o instanceof List && !o.hasProperty("isvector")
    }

    def static vector(o) {
        def v = o.collect()
        v.metaClass.isvector = true
        v
    }
    def static vector_Q(o) {
        return o instanceof List && o.hasProperty("isvector") && o.isvector
    }

    def static hash_map(lst) {
        def m = [:]
        assoc_BANG(m, lst)
    }
    def static assoc_BANG(m, kvs) {
        for (int i=0; i<kvs.size(); i+=2) {
            m[kvs[i]] = kvs[i+1];
        }
        return m
    }
    def static dissoc_BANG(m, ks) {
        for (int i=0; i<ks.size(); i++) {
            m.remove(ks[i])
        }
        return m
    }
    def static hash_map_Q(o) {
        return o instanceof Map
    }

    def static sequential_Q(o) {
        return types.list_Q(o) || types.vector_Q(o)
    }

    @AutoClone
    static class MalFunc {
        def EVAL
        def ast
        def env
        def params
        def ismacro

        MalFunc(_EVAL, _ast, _env, _params) {
            EVAL = _EVAL
            ast = _ast
            env = _env
            params = _params
            ismacro = false
        }

        def call(args) {
            def new_env = env.class.newInstance([env, params, args] as Object[])
            return EVAL(ast, new_env)
        }
    }

    @AutoClone
    static class MalAtom {
        def value
        MalAtom(_value) {
            value = _value
        }
    }
}
