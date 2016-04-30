public class Util
{
    fun static MalObject[] sequenceToMalObjectArray(MalObject m)
    {
        if( m.type == "list" )
        {
            return (m$MalList).value();
        }
        else if( m.type == "vector" )
        {
            return (m$MalVector).value();
        }
    }
}
