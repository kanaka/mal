using MalVal = Mal.types.MalVal;
using MalInteger = Mal.types.MalInteger;
using MalList = Mal.types.MalList;
using MalFunction = Mal.types.MalFunction;

namespace Mal {
    public class core {
        public class plus : MalFunction {
            public override MalVal apply(MalList args) {
                return ((MalInteger)args.nth(0)).add(
                        ((MalInteger)args.nth(1)));
            }
        }
        public class minus : MalFunction {
            public override MalVal apply(MalList args) {
                return ((MalInteger)args.nth(0)).subtract(
                        ((MalInteger)args.nth(1)));
            }
        }
        public class multiply : MalFunction {
            public override MalVal apply(MalList args) {
                return ((MalInteger)args.nth(0)).multiply(
                        ((MalInteger)args.nth(1)));
            }
        }
        public class divide : MalFunction {
            public override MalVal apply(MalList args) {
                return ((MalInteger)args.nth(0)).divide(
                        ((MalInteger)args.nth(1)));
            }
        }
    }
}
