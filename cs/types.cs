using System;
using System.Collections.Generic;
using Mal;

namespace Mal {
    public class types {
        public class MalThrowable : Exception {
            public MalThrowable() : base() { }
            public MalThrowable(string msg) : base(msg) {  }
        }
        public class MalError : MalThrowable {
            public MalError(string msg) :base(msg) { }
        }
        public class MalContinue : MalThrowable { }


        public abstract class MalVal {
            // Default is just to call regular toString()
            public virtual string ToString(bool print_readably) {
                return "<unknown>";
            }
            public virtual bool list_Q() { return false; }
        }

        public class MalConstant : MalVal {
            string value;
            public MalConstant(string name) { value = name; }
            public MalConstant copy() { return this; }

            public override string ToString() {
                return value;
            }
            public override string ToString(bool print_readably) {
                return value;
            }
        }

        static public MalConstant Nil = new MalConstant("nil");
        static public MalConstant True = new MalConstant("true");
        static public MalConstant False = new MalConstant("false");

        public class MalInteger : MalVal {
            int value;
            public MalInteger(int v) { value = v; }
            public MalInteger copy() { return this; }

            public int getValue() { return value; }
            public override string ToString() {
                return value.ToString();
            }
            public override string ToString(bool print_readably) {
                return value.ToString();
            }
            public MalInteger add(MalInteger other) {
                return new MalInteger(value + other.getValue());
            }
            public MalInteger subtract(MalInteger other) {
                return new MalInteger(value - other.getValue());
            }
            public MalInteger multiply(MalInteger other) {
                return new MalInteger(value * other.getValue());
            }
            public MalInteger divide(MalInteger other) {
                return new MalInteger(value / other.getValue());
            }
            public MalConstant lt(MalInteger other) {
                return (value < other.getValue()) ? True : False;
            }
            public MalConstant lte(MalInteger other) {
                return (value <= other.getValue()) ? True : False;
            }
            public MalConstant gt(MalInteger other) {
                return (value > other.getValue()) ? True : False;
            }
            public MalConstant gte(MalInteger other) {
                return (value >= other.getValue()) ? True : False;
            }
        }

        public class MalSymbol : MalVal {
            string value;
            public MalSymbol(string v) { value = v; }
            public MalSymbol copy() { return this; }

            public string getName() { return value; }
            public override string ToString() {
                return value;
            }
            public override string ToString(bool print_readably) {
                return value;
            }
        }

        public class MalString : MalVal {
            string value;
            public MalString(string v) { value = v; }
            public MalString copy() { return this; }

            public string getValue() { return value; }
            public override string ToString() {
                return "\"" + value + "\"";
            }
            public override string ToString(bool print_readably) {
                if (print_readably) {
                    //return "\"" + printer.escapeString(value) + "\"";
                    return "\"" + value + "\"";
                } else {
                    return value;
                }
            }
        }



        public class MalList : MalVal {
            public string start = "(", end = ")";
            List<MalVal> value;
            public MalList() {
                value = new List<MalVal>();
            }
            public MalList(List<MalVal> val) {
                value = val;
            }
            public MalList(params MalVal[] mvs) {
                conj_BANG(mvs);
            }

            public MalList copy() {
                return (MalList)this.MemberwiseClone();
            }

            public List<MalVal> getValue() { return value; }
            public override bool list_Q() { return true; }

            public override string ToString() {
                return start + printer.join(value, " ", true) + end;
            }
            public override string ToString(bool print_readably) {
                return start + printer.join(value, " ", print_readably) + end;
            }

            public MalList conj_BANG(params MalVal[] mvs) {
                for (int i = 0; i < mvs.Length; i++) {
                    value.Add(mvs[i]);
                }
                return this;
            }

            public int size() { return value.Count; }
            public MalVal nth(int idx) { return value[idx]; }
            public MalVal rest() {
                if (size() > 0) {
                    return new MalList(value.GetRange(1, value.Count-1));
                } else {
                    return new MalList();
                }
            }
        }

        public class MalVector : MalList {
            // Same implementation except for instantiation methods
            public MalVector() :base() {
                start = "[";
                end = "]";
            }
            public MalVector(List<MalVal> val)
                    :base(val) {
                start = "[";
                end = "]";
            }
            /*
            public MalVector(MalVal[] mvs) : base(mvs.ToArray()) {
                start = "[";
                end = "]";
            }
            */
            public new MalVector copy() {
                return (MalVector)this.MemberwiseClone();
            }

            public override bool list_Q() { return false; }
        }

        public class MalHashMap : MalVal {
            Dictionary<string, MalVal> value;
            public MalHashMap(Dictionary<string, MalVal> val) {
                value = val;
            }
            public MalHashMap(MalList lst) {
                value = new Dictionary<String, MalVal>();
                assoc_BANG(lst.getValue().ToArray());
            }
            /*
            public MalHashMap(params MalVal[] mvs) {
                value = new Dictionary<String, MalVal>();
                assoc_BANG(mvs);
            }
            */
            public MalHashMap copy() {
                return (MalHashMap)this.MemberwiseClone();
            }

            public Dictionary<string, MalVal> getValue() { return value; }

            public override string ToString() {
                return "{" + printer.join(value, " ", true) + "}";
            }
            public override string ToString(Boolean print_readably) {
                return "{" + printer.join(value, " ", print_readably) + "}";
            }

            /*
            public Set _entries() {
                return value.entrySet();
            }
            */
            
            public MalHashMap assoc_BANG(params MalVal[] mvs) {
                for (int i=0; i<mvs.Length; i+=2) {
                    value.Add(((MalString)mvs[i]).getValue(), mvs[i+1]);
                }
                return this;
            }
        }

        public abstract class MalFunction : MalVal {
            public abstract MalVal apply(MalList args);
        }

    }
}
