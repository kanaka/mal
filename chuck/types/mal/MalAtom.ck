public class MalAtom extends MalObject
{
    "atom" => type;

    fun MalObject value()
    {
        return object$MalObject;
    }

    fun void init(MalObject value)
    {
        value @=> object;
    }

    fun static MalObject create(MalObject value)
    {
        MalAtom m;
        m.init(value);
        return m;
    }

    fun MalObject clone()
    {
        MalAtom value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
