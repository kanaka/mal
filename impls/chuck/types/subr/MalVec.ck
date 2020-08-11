public class MalVec extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if (args.size() == 1) {
            args[0] @=> MalObject a0;
            if (a0.type == "vector") {
                return a0;
            } else if (a0.type == "list") {
                return MalVector.create((a0$MalList).value());
            }
        }
        return MalError.create(MalString.create("vec: wrong arguments"));
    }
}
