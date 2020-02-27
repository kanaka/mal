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

    fun static string keyName(MalObject m)
    {
        if( m.type == "string" )
        {
            return (m$MalString).value();
        }
        else if (m.type == "keyword" )
        {
            return (m$MalKeyword).value();
        }
    }

    fun static void println(string message)
    {
        chout <= message + "\n";
    }
}
